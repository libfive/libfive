#include "gui/shape.hpp"
#include "gui/shader.hpp"

Shape::Shape(Kernel::Tree t, std::shared_ptr<std::map<Kernel::Tree::Id,
                                                      float>> vars)
    : tree(t), vars(vars), vert_vbo(QOpenGLBuffer::VertexBuffer),
      tri_vbo(QOpenGLBuffer::IndexBuffer)
{
    // Construct evaluators to run meshing (in parallel)
    es.reserve(8);
    for (unsigned i=0; i < es.capacity(); ++i)
    {
        es.emplace_back(Kernel::Evaluator(t, *vars));
    }

    connect(&mesh_watcher, &decltype(mesh_watcher)::finished,
            this, &Shape::onFutureFinished);
}

void Shape::updateFrom(const Shape* other)
{
    assert(other->id() == id());
    updateVars(*(other->vars));
}

void Shape::updateVars(const std::map<Kernel::Tree::Id, float>& vs)
{
    bool changed = false;
    for (auto& v : vs)
    {
        auto va = vars->find(v.first);
        if (va != vars->end() && va->second != v.second)
        {
            changed = true;
            va->second = v.second;
        }
    }

    if (changed)
    {
        cancel.store(true);

        // Start a special render operation that uses a flag in the div
        // field that tells the system load new var values before starting
        startRender(Settings(next, MESH_DIV_NEW_VARS));
    }
}

void Shape::draw(const QMatrix4x4& M)
{
    if (mesh && !gl_ready)
    {
        initializeOpenGLFunctions();

        GLfloat* verts = new GLfloat[mesh->verts.size() * 6];
        unsigned i = 0;
        for (auto& v : mesh->verts)
        {
            verts[i++] = v.x();
            verts[i++] = v.y();
            verts[i++] = v.z();
            verts[i++] = 1;
            verts[i++] = 1;
            verts[i++] = 1;
        }
        vert_vbo.create();
        vert_vbo.setUsagePattern(QOpenGLBuffer::StaticDraw);
        vert_vbo.bind();
        vert_vbo.allocate(verts, i*sizeof(*verts));
        delete [] verts;

        uint32_t* tris = new uint32_t[mesh->branes.size() * 3];
        i = 0;
        for (auto& t: mesh->branes)
        {
            tris[i++] = t[0];
            tris[i++] = t[1];
            tris[i++] = t[2];
        }
        tri_vbo.create();
        tri_vbo.setUsagePattern(QOpenGLBuffer::StaticDraw);
        tri_vbo.bind();
        tri_vbo.allocate(tris, i*sizeof(*tris));
        delete [] tris;

        if (!vao.isCreated())
        {
            vao.create();
        }
        vao.bind();
        vert_vbo.bind();
        tri_vbo.bind();
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 6*sizeof(GLfloat), NULL);
        glVertexAttribPointer(
                1, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(GLfloat),
                (GLvoid*)(3 * sizeof(GLfloat)));
        glEnableVertexAttribArray(0);
        glEnableVertexAttribArray(1);

        gl_ready = true;
    }

    if (gl_ready)
    {
        Shader::shaded->bind();
        glUniformMatrix4fv(Shader::shaded->uniformLocation("M"), 1, GL_FALSE, M.data());
        vao.bind();
        glDrawElements(GL_TRIANGLES, mesh->branes.size() * 3, GL_UNSIGNED_INT, NULL);
        vao.release();
        Shader::shaded->release();
    }
}

void Shape::drawMonochrome(const QMatrix4x4& M, QColor color)
{
    if (gl_ready)
    {
        Shader::monochrome->bind();
        glUniformMatrix4fv(Shader::monochrome->uniformLocation("M"),
                           1, GL_FALSE, M.data());
        glUniform3f(Shader::monochrome->uniformLocation("frag_color"),
                color.redF(), color.greenF(), color.blueF());
        vao.bind();
        glDrawElements(GL_TRIANGLES, mesh->branes.size() * 3, GL_UNSIGNED_INT, NULL);
        vao.release();
        Shader::monochrome->release();
    }
}

void Shape::startRender(Settings s)
{
    if (mesh_future.isRunning())
    {
        if (next.div != MESH_DIV_ABORT)
        {
            next = s;
        }
    }
    else
    {
        if (s.div == MESH_DIV_NEW_VARS)
        {
            for (auto& e : es)
            {
                e.updateVars(*vars);
            }
            s = s.base();
        }
        mesh_future = QtConcurrent::run(this, &Shape::renderMesh, s);
        mesh_watcher.setFuture(mesh_future);
        next = s.next();
    }
}

bool Shape::done() const
{
    return next.div == MESH_DIV_EMPTY && mesh_future.isFinished();
}

Kernel::Mesh* Shape::renderMesh(Settings s)
{
    cancel.store(false);
    Kernel::Region<3> r({s.min.x(), s.min.y(), s.min.z()},
                        {s.max.x(), s.max.y(), s.max.z()});
    auto m = Kernel::Mesh::render(es.data(), r, 1 / (s.res / (1 << s.div)),
                                  pow(10, -s.quality), cancel);
    return m.release();
}

Kernel::Evaluator* Shape::dragFrom(const QVector3D& v)
{
    auto e = new Kernel::Evaluator(tree, *vars);
    e->specialize({v.x(), v.y(), v.z()});
    return e;
}

void Shape::deleteLater()
{
    if (mesh_future.isRunning())
    {
        next.div = MESH_DIV_ABORT;
        cancel.store(true);
    }
    else
    {
        QObject::deleteLater();
    }
}

void Shape::onFutureFinished()
{
    mesh.reset(mesh_future.result());
    gl_ready = false;
    emit(gotMesh());

    if (next.div == MESH_DIV_ABORT)
    {
        QObject::deleteLater();
    }
    else if (next.div >= 0 || next.div == MESH_DIV_NEW_VARS)
    {
        startRender(next);
    }
}

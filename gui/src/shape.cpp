#include "gui/shape.hpp"
#include "gui/shader.hpp"

const int Shape::MESH_DIV_EMPTY;
const int Shape::MESH_DIV_ABORT;
const int Shape::MESH_DIV_NEW_VARS;

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

    connect(this, &Shape::gotMesh, this, &Shape::redraw);
    connect(&mesh_watcher, &decltype(mesh_watcher)::finished,
            this, &Shape::onFutureFinished);
}

bool Shape::updateFrom(const Shape* other)
{
    assert(other->id() == id());
    return updateVars(*(other->vars));
}

bool Shape::updateVars(const std::map<Kernel::Tree::Id, float>& vs)
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
        // Only abort non-default renders
        if (target_div != default_div)
        {
            cancel.store(true);
        }

        // Start a special render operation that uses a flag in the div
        // field that tells the system load new var values before starting
        startRender({next.first, MESH_DIV_NEW_VARS});
    }

    return changed;
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
        QVector3D shade = (grabbed == true && drag_valid == false) ?
            QVector3D(1.0, 0.78, 0.70) : QVector3D(1.0, 1.0, 1.0);

        Shader::shaded->bind();
        glUniform3f(Shader::shaded->uniformLocation("shade"),
                    shade.x(), shade.y(), shade.z());
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
    startRender(QPair<Settings, int>(s, s.defaultDiv()));
}

void Shape::startRender(QPair<Settings, int> s)
{
    if (default_div == MESH_DIV_EMPTY)
    {
        default_div = s.second;
    }

    if (running)
    {
        if (next.second != MESH_DIV_ABORT)
        {
            next = s;
        }
    }
    else
    {
        if (s.second == MESH_DIV_NEW_VARS)
        {
            for (auto& e : es)
            {
                e.updateVars(*vars);
            }
            s.second = default_div;
        }

        target_div = s.second;

        timer.start();
        running = true;
        mesh_future = QtConcurrent::run(this, &Shape::renderMesh, s);
        mesh_watcher.setFuture(mesh_future);

        next = {s.first, s.second - 1};
    }
}

bool Shape::done() const
{
    return next.second == MESH_DIV_EMPTY && mesh_future.isFinished();
}

////////////////////////////////////////////////////////////////////////////////

void Shape::setDragValid(bool happy)
{
    bool changed = happy != drag_valid;
    drag_valid = happy;
    if (changed)
    {
        emit(redraw());
    }
}

void Shape::setGrabbed(bool g)
{
    bool changed = grabbed != g;
    grabbed = g;
    if (changed)
    {
        emit(redraw());
    }
}

////////////////////////////////////////////////////////////////////////////////

Kernel::Evaluator* Shape::dragFrom(const QVector3D& v)
{
    auto e = new Kernel::Evaluator(tree, *vars);
    e->specialize({v.x(), v.y(), v.z()});
    return e;
}

void Shape::deleteLater()
{
    if (running)
    {
        next.second = MESH_DIV_ABORT;
        cancel.store(true);
    }
    else
    {
        QObject::deleteLater();
    }
}

void Shape::onFutureFinished()
{
    running = false;

    auto m = mesh_future.result();
    if (m != nullptr)
    {
        mesh.reset(mesh_future.result());
        gl_ready = false;
        emit(gotMesh());

        auto t = timer.elapsed();

        if (target_div == default_div)
        {
            if (t < 20 && default_div > 0)
            {
                default_div--;
            }
            else if (t > 50)
            {
                default_div++;
            }
        }
    }

    if (next.second == MESH_DIV_ABORT)
    {
        QObject::deleteLater();
    }
    else if (next.second >= 0 || next.second == MESH_DIV_NEW_VARS)
    {
        startRender(next);
    }
}

////////////////////////////////////////////////////////////////////////////////
// This function is called in a separate thread:
Kernel::Mesh* Shape::renderMesh(QPair<Settings, int> s)
{
    cancel.store(false);
    Kernel::Region<3> r({s.first.min.x(), s.first.min.y(), s.first.min.z()},
                        {s.first.max.x(), s.first.max.y(), s.first.max.z()});
    auto m = Kernel::Mesh::render(es.data(), r,
            1 / (s.first.res / (1 << s.second)),
            pow(10, -s.first.quality), cancel);
    return m.release();
}

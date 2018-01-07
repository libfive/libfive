/*
Studio: a simple GUI for the libfive CAD kernel
Copyright (C) 2017  Matt Keeter

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/
#include "studio/shape.hpp"
#include "studio/shader.hpp"

#include "libfive/solve/bounds.hpp"

const int Shape::MESH_DIV_EMPTY;
const int Shape::MESH_DIV_ABORT;
const int Shape::MESH_DIV_NEW_VARS;
const int Shape::MESH_DIV_NEW_VARS_SMALL;

Shape::Shape(Kernel::Tree t, std::map<Kernel::Tree::Id, float> vars)
    : tree(t), vars(vars), vert_vbo(QOpenGLBuffer::VertexBuffer),
      tri_vbo(QOpenGLBuffer::IndexBuffer)
{
    // Construct evaluators to run meshing (in parallel)
    es.reserve(8);
    for (unsigned i=0; i < es.capacity(); ++i)
    {
        es.emplace_back(Kernel::XTreeEvaluator(t, vars));
    }

    connect(this, &Shape::gotMesh, this, &Shape::redraw);
    connect(&mesh_watcher, &decltype(mesh_watcher)::finished,
            this, &Shape::onFutureFinished);
}

Shape::~Shape()
{
    if (mesh_future.isRunning())
    {
        mesh_future.waitForFinished();
    }
}

bool Shape::updateFrom(const Shape* other)
{
    assert(other->id() == id());
    return updateVars(other->vars);
}

bool Shape::updateVars(const std::map<Kernel::Tree::Id, float>& vs)
{
    bool changed = false;

    // If all of the variable changes are small (< 1e-6), then this is
    // probably an update caused by a script re-evaluation, where the
    // textual form of the float is slightly different from the dragged
    // value.  In this case, we want to render at div = 0; otherwise, we
    // see a visual glitch when the script re-evaluates after a drag.
    int div = MESH_DIV_NEW_VARS_SMALL;

    for (auto& v : vs)
    {
        auto va = vars.find(v.first);
        if (va != vars.end() && va->second != v.second)
        {
            if (fabs(va->second - v.second) > 1e-6)
            {
                div = MESH_DIV_NEW_VARS;
            }
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
        startRender({next.first, div});
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
        auto shade = (grabbed || hover) ? QVector3D(1, 1, 1)
                                        : QVector3D(0.9, 0.9, 0.9);

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
        glUniform4f(Shader::monochrome->uniformLocation("frag_color"),
                color.redF(), color.greenF(), color.blueF(), 1.0f);
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
            cancel.store(true);
            next = s;
        }
    }
    else
    {
        if (s.second == MESH_DIV_NEW_VARS ||
            s.second == MESH_DIV_NEW_VARS_SMALL)
        {
            for (auto& e : es)
            {
                e.updateVars(vars);
            }
            s.second = (s.second == MESH_DIV_NEW_VARS) ? default_div : 0;
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

void Shape::setGrabbed(bool g)
{
    bool changed = grabbed != g;
    grabbed = g;
    if (changed)
    {
        emit(redraw());
    }
}

void Shape::setHover(bool h)
{
    bool changed = hover != h;
    hover = h;
    if (changed)
    {
        emit(redraw());
    }
}

////////////////////////////////////////////////////////////////////////////////

Kernel::JacobianEvaluator* Shape::dragFrom(const QVector3D& v)
{
    auto e = new Kernel::JacobianEvaluator(
            std::make_shared<Kernel::Tape>(tree), vars);
    e->evalAndPush({v.x(), v.y(), v.z()});
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

    auto bm = mesh_future.result();
    if (bm.first != nullptr)
    {
        mesh.reset(bm.first);
        bounds = bm.second;

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
    else if (next.second >= 0 || next.second == MESH_DIV_NEW_VARS
                              || next.second == MESH_DIV_NEW_VARS_SMALL)
    {
        startRender(next);
    }
}

void Shape::freeGL()
{
    if (gl_ready)
    {
        vao.destroy();
        vert_vbo.destroy();
        tri_vbo.destroy();

        gl_ready = false;
    }
}

////////////////////////////////////////////////////////////////////////////////
// This function is called in a separate thread:
Shape::BoundedMesh Shape::renderMesh(QPair<Settings, int> s)
{
    cancel.store(false);

    // Use the global bounds settings by default, but try to solve for more
    // precise bounds if autobounds is true.
    Kernel::Region<3> r({s.first.min.x(), s.first.min.y(), s.first.min.z()},
                        {s.first.max.x(), s.first.max.y(), s.first.max.z()});
    if (s.first.autobounds)
    {
        auto r_ = Kernel::findBounds(&es[0].interval);
        if (!r_.lower.isNaN().any() &&
            !r_.upper.isNaN().any() &&
            ((r_.upper - r_.lower).array() / (r.upper - r.lower).array())
                .abs().maxCoeff() < 1000)
        {
            r = r_;

            // Add a little padding for numerical safety
            Kernel::Region<3>::Pt diff = r.upper - r.lower;
            r.lower -= diff / 10;
            r.upper += diff / 10;
        }
    }
    auto m = Kernel::Mesh::render(es.data(), r,
            1 / (s.first.res / (1 << s.second)),
            pow(10, -s.first.quality), cancel);
    return {m.release(), r};
}

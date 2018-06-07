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
#pragma once

#include <QObject>
#include <QtConcurrent>
#include <QOpenGLVertexArrayObject>
#include <QOpenGLBuffer>
#include <QOpenGLFunctions>

#include "libfive/tree/tree.hpp"
#include "libfive/render/brep/mesh.hpp"
#include "libfive/render/brep/pool.hpp"
#include "libfive/eval/eval_jacobian.hpp"

namespace Kernel { class Tape; /*  forward declaration */ }

#include "studio/settings.hpp"

class Shape : public QObject, QOpenGLFunctions
{
    Q_OBJECT
public:
    Shape(Kernel::Tree t, std::map<Kernel::Tree::Id, float> vars);

    /*
     *  In destructor, wait for computation to finish
     *  (otherwise XTreeEvaluators may be destroyed early)
     */
    ~Shape();

    /*  Constructs OpenGL objects as needed  */
    void draw(const QMatrix4x4& M);

    /*
     *  Draws with a monochrome shader (for pick buffer)
     *  This is a no-op if the mesh and OpenGL buffers aren't ready
     */
    void drawMonochrome(const QMatrix4x4& M, QColor color);

    /*
     *  Kicks off a mesh rendering operation in a separate thread
     */
    void startRender(Settings s);

    /*
     *  Checks whether the shape is done rendering
     */
    bool done() const;

    /*
     *  Returns the raw mesh object pointer
     */
    const Kernel::Mesh* getMesh() const { return mesh.data(); }

    /*
     *  Looks up the tree's ID
     */
    Kernel::Tree::Id id() const { return tree.id(); }

    /*
     *  Updates variables from another Shape
     *  (which must point to the same Tree)
     *
     *  Returns true if variable values have changed.
     */
    bool updateFrom(const Shape* other);

    /*
     *  Updates variables in the Evaluator, scheduling a new
     *  (min-resolution) render if things have changed
     *
     *  Returns true if variable values have changed.
     */
    bool updateVars(const std::map<Kernel::Tree::Id, float>& vs);

    /*
     *  Checks to see whether this shape has attached vars
     *  (which determines whether it's draggable)
     */
    bool hasVars() const { return vars.size(); }

    /*
     *  Returns a new evaluator specialized at the given drag position
     *
     *  Ownership is transfered, so the caller is responsible for deleting
     *  the evaluator (or storing it in an owned structure)
     */
    std::pair<Kernel::JacobianEvaluator*, std::shared_ptr<Kernel::Tape>>
    dragFrom(const QVector3D& pt);

    /*
     *  Returns another pointer to the solution map
     */
    const std::map<Kernel::Tree::Id, float>& getVars() const
    { return vars; }

    /*
     *  Looks up the shape's bounds
     */
    const Kernel::Region<3>& getRenderBounds() const { return render_bounds; }

    /*
     *  Sets grabbed and redraws as necessary
     */
    void setGrabbed(bool g);

    /*
     *  Sets hover and redraws as necessary
     */
    void setHover(bool h);

    /*
     *  Destroys all OpenGL objects associated with this shape
     *
     *  This must be called when the context is current, and must
     *  be called before the context is destroyed (otherwise, the Shape
     *  destructor may try to use a now-destroyed context)
     */
    void freeGL();

signals:
    void gotMesh();
    void redraw();

public slots:
    void deleteLater();

protected slots:
    void onFutureFinished();

protected:
    void startRender(QPair<Settings, int> s);

    bool grabbed=false;
    bool hover=false;

    typedef QPair<Kernel::Mesh*, Kernel::Region<3>> BoundedMesh;
    BoundedMesh renderMesh(QPair<Settings, int> s);
    QFuture<BoundedMesh> mesh_future;
    QFutureWatcher<BoundedMesh> mesh_watcher;

    Kernel::Tree tree;
    std::map<Kernel::Tree::Id, float> vars;
    Kernel::Pool pool;

    QScopedPointer<Kernel::Mesh> mesh;
    Kernel::Region<3> render_bounds;
    Kernel::Region<3> mesh_bounds;
    QPair<Settings, int> next;

    /*  running marks not just whether the future has finished, but whether
     *  the main thread has handled it.  This prevents situations where the
     *  mesh_future.isRunning() == false but onFutureFinished hasn't yet
     *  been called.  */
    bool running=false;

    bool gl_ready=false;
    QOpenGLVertexArrayObject vao;
    QOpenGLBuffer vert_vbo;
    QOpenGLBuffer tri_vbo;

    QTime timer;

    const static int MESH_DIV_EMPTY=-1;
    const static int MESH_DIV_ABORT=-2;
    const static int MESH_DIV_NEW_VARS=-3;
    const static int MESH_DIV_NEW_VARS_SMALL=-4;

    int default_div=MESH_DIV_EMPTY;
    int target_div=MESH_DIV_EMPTY;
};

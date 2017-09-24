#pragma once

#include <QObject>
#include <QtConcurrent>
#include <QOpenGLVertexArrayObject>
#include <QOpenGLBuffer>
#include <QOpenGLFunctions>

#include "ao/tree/tree.hpp"
#include "ao/render/brep/mesh.hpp"
#include "gui/settings.hpp"

class Shape : public QObject, QOpenGLFunctions
{
    Q_OBJECT
public:
    Shape(Kernel::Tree t, std::shared_ptr<std::map<Kernel::Tree::Id,
                                                   float>> vars);

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
    bool updateVars(const std::map<Kernel::Tree::Id, float>& vars);

    /*
     *  Checks to see whether this shape has attached vars
     *  (which determines whether it's draggable)
     */
    bool hasVars() const { return vars->size(); }

    /*
     *  Returns a new evaluator specialized at the given drag position
     *  Ownership is transfered, so the caller is responsible for deleting
     *  the evaluator (or storing it in an owned structure)
     */
    Kernel::Evaluator* dragFrom(const QVector3D& pt);

    /*
     *  Returns another pointer to the solution map
     */
    std::shared_ptr<std::map<Kernel::Tree::Id, float>> getVars() const
    { return vars; }

    /*
     *  Sets drag state and redraws as necessary
     */
    void setDragValid(bool happy);

    /*
     *  Sets grabbed and redraws as necessary
     */
    void setGrabbed(bool g);

    /*
     *  Sets hover and redraws as necessary
     */
    void setHover(bool h);

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
    bool drag_valid=true;
    bool hover=false;

    Kernel::Mesh* renderMesh(QPair<Settings, int> s);
    QFuture<Kernel::Mesh*> mesh_future;
    QFutureWatcher<Kernel::Mesh*> mesh_watcher;
    std::atomic_bool cancel;

    Kernel::Tree tree;
    std::shared_ptr<std::map<Kernel::Tree::Id, float>> vars;
    std::vector<Kernel::Evaluator,
                Eigen::aligned_allocator<Kernel::Evaluator>> es;

    QScopedPointer<Kernel::Mesh> mesh;
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

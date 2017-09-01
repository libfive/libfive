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
     */
    void updateFrom(const Shape* other);

    /*
     *  Updates variables in the Evaluator, scheduling a new
     *  (min-resolution) render if things have changed
     */
    void updateVars(const std::map<Kernel::Tree::Id, float>& vars);

signals:
    void gotMesh();

public slots:
    void deleteLater();

protected slots:
    void onFutureFinished();

protected:
    Kernel::Mesh* renderMesh(Settings s);
    QFuture<Kernel::Mesh*> mesh_future;
    QFutureWatcher<Kernel::Mesh*> mesh_watcher;
    std::atomic_bool cancel;

    Kernel::Tree tree;
    std::shared_ptr<std::map<Kernel::Tree::Id, float>> vars;
    std::vector<Kernel::Evaluator,
                Eigen::aligned_allocator<Kernel::Evaluator>> es;

    QScopedPointer<Kernel::Mesh> mesh;
    Settings next;

    bool gl_ready=false;
    QOpenGLVertexArrayObject vao;
    QOpenGLBuffer vert_vbo;
    QOpenGLBuffer tri_vbo;

    const static int MESH_RES_ABORT=-2;
    const static int MESH_RES_EMPTY=-1;
};

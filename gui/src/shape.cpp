#include "gui/shape.hpp"

Shape::Shape(Kernel::Tree t)
    : tree(t)
{
    connect(&mesh_watcher, &decltype(mesh_watcher)::finished,
            this, &Shape::onFutureFinished);
}

void Shape::draw(const QMatrix4x4& M)
{
    if (!gl_ready)
    {
        printf("Making OpenGL stuff!\n");
        gl_ready = true;
    }
    // Nothing to do here
}

void Shape::startRender()
{
    assert(mesh.data() == nullptr);
    mesh_future = QtConcurrent::run(this, &Shape::renderMesh);
    mesh_watcher.setFuture(mesh_future);
}

Kernel::Mesh* Shape::renderMesh()
{
    qDebug() << "Rendering in" << QThread::currentThread();
    Kernel::Region r({-1, 1}, {-1, 1}, {-1, 1}, 10);
    auto m = Kernel::Mesh::render(tree, r);
    return m.release();
}

void Shape::onFutureFinished()
{
    qDebug() << "Finished in" << QThread::currentThread();
    mesh.reset(mesh_future.result());
    gl_ready = false;
    emit(gotMesh());
}

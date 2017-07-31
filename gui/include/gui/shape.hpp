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
    Shape(Kernel::Tree t);

    /*  Constructs OpenGL objects as needed  */
    void draw(const QMatrix4x4& M);

    /*
     *  Kicks off a mesh rendering operation in a separate thread
     */
    void startRender(Settings s);

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

    Kernel::Tree tree;
    QScopedPointer<Kernel::Mesh> mesh;
    Settings next;

    bool gl_ready=false;
    QOpenGLVertexArrayObject vao;
    QOpenGLBuffer vert_vbo;
    QOpenGLBuffer tri_vbo;
};

#include <QApplication>
#include <QSurfaceFormat>

#include "gui/window.hpp"
#include "gui/settings.hpp"
#include "gui/shape.hpp"

int main(int argc, char** argv)
{
    {   // Configure default OpenGL as 3.3 Core
        QSurfaceFormat format;
        format.setVersion(3, 3);
        format.setProfile(QSurfaceFormat::CoreProfile);
        format.setSamples(4);
        QSurfaceFormat::setDefaultFormat(format);
    }

    // Register settings and shape meta-types to be sent across threads
    qRegisterMetaType<Settings>("Settings");
    qRegisterMetaType<QList<Shape*>>("QList<Shape*>");
    qRegisterMetaType<QPair<uint32_t,uint32_t>>("QPair<uint32_t,uint32_t>");

    QApplication app(argc, argv);
    Window w(argc > 1 ? argv[1] : ":/examples/tutorial.ao");
    app.exec();
}

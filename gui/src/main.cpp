#include <QSurfaceFormat>
#include "gui/app.hpp"
#include "gui/settings.hpp"

int main(int argc, char** argv)
{
    {   // Configure default OpenGL as 3.3 Core
        QSurfaceFormat format;
        format.setVersion(3, 3);
        format.setProfile(QSurfaceFormat::CoreProfile);
        format.setSamples(4);
        QSurfaceFormat::setDefaultFormat(format);
    }

    // Register settings meta-type to be sent across threads
    qRegisterMetaType<Settings>("Settings");

    App a(argc, argv);
    a.exec();
}

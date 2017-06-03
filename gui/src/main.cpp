#include <QSurfaceFormat>
#include "gui/app.hpp"

int main(int argc, char** argv)
{
    {   // Configure default OpenGL as 3.3 Core
        QSurfaceFormat format;
        format.setVersion(3, 3);
        format.setProfile(QSurfaceFormat::CoreProfile);
        format.setSamples(4);
        QSurfaceFormat::setDefaultFormat(format);
    }

    App a(argc, argv);
    a.exec();
}

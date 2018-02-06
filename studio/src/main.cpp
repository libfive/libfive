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
#include <QApplication>
#include <QSurfaceFormat>

#include "studio/app.hpp"
#include "studio/editor.hpp"
#include "studio/window.hpp"
#include "studio/settings.hpp"
#include "studio/shape.hpp"

int main(int argc, char** argv)
{
    {   // Configure default OpenGL as 3.2 Core
        QSurfaceFormat format;
        format.setVersion(3, 2);
        format.setProfile(QSurfaceFormat::CoreProfile);
        format.setSamples(8);
        QSurfaceFormat::setDefaultFormat(format);
    }

    // Register settings and shape meta-types to be sent across threads
    qRegisterMetaType<Settings>("Settings");
    qRegisterMetaType<QList<Shape*>>("QList<Shape*>");
    qRegisterMetaType<Editor::Range>("Editor::Range");
    qRegisterMetaType<QMap<Kernel::Tree::Id,Editor::Range>>(
            "QMap<Kernel::Tree::Id,Editor::Range>");

    App a(argc, argv);
    a.exec();
}

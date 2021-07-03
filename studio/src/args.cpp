/*
Studio: a simple GUI for the libfive CAD kernel
Copyright (C) 2018  Matt Keeter

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

#include <QCommandLineParser>

#include "studio/args.hpp"
#include "studio/editor.hpp"

namespace Studio {

Arguments::Arguments(QCoreApplication* app)
{
    QCommandLineParser parser;
    parser.setApplicationDescription("A simple GUI for the libfive kernel");
    parser.addHelpOption();
    parser.addPositionalArgument("filename", "File to load");

    QCommandLineOption vertical_layout("vertical", "Use vertical layout for editor / 3D view.");
    parser.addOption(vertical_layout);

    QCommandLineOption python_option(
            QStringList() << "p" << "python",
            "Boot the interpreter in Python mode");

    QCommandLineOption guile_option(
            QStringList() << "g" << "guile",
            "Boot the interpreter in Guile mode");

    if (Editor::supportsLanguage(Language::LANGUAGE_PYTHON)) {
        parser.addOption(python_option);
    }
    if (Editor::supportsLanguage(Language::LANGUAGE_GUILE)) {
        parser.addOption(guile_option);
    }

    parser.process(*app);

    const QStringList ps = parser.positionalArguments();
    filename = ps.isEmpty() ? "" : ps[0];

    vertical = parser.isSet(vertical_layout);

    if (parser.isSet(python_option) && parser.isSet(guile_option))
        std::cerr << "-p and -g cannot be set at the same time" << std::endl;
    else if (parser.isSet(python_option))
        language = Language::LANGUAGE_PYTHON;
    else if (parser.isSet(guile_option))
        language = Language::LANGUAGE_GUILE;
}

}   // namespace Studio

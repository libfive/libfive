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

Arguments::Arguments(QCoreApplication* app)
{
    QCommandLineParser parser;
    parser.setApplicationDescription("A simple GUI for the libfive kernel");
    parser.addHelpOption();
    parser.addPositionalArgument("filename", "File to load");

    QCommandLineOption no_syntax("no-syntax", "Turn off syntax highlighting");
    parser.addOption(no_syntax);

    parser.process(*app);

    const QStringList ps = parser.positionalArguments();
    filename = ps.isEmpty() ? "" : ps[0];

    do_syntax = !parser.isSet(no_syntax);
}

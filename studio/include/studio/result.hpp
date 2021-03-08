/*
Studio: a simple GUI for the libfive CAD kernel
Copyright (C) 2021  Matt Keeter

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
#pragma once
#include <QList>

#include "studio/range.hpp"
#include "studio/settings.hpp"

#include "libfive/tree/tree.hpp"

namespace Studio {

class Shape;

struct Error {
    QString error;
    QString stack;
    Range range;
};

struct Result {
    /*  Sets whether result or error is valid */
    bool okay;

    /* A valid result, which should be shown in the GUI */
    QString result;

    /*  An error, which should be shown in the GUI and highlighted */
    Error error;

    /*  All of the other things which a valid script can produce */
    Settings settings;
    QList<Shape*> shapes;
    QMap<libfive::Tree::Id, Range> vars;

    /*  Warnings to be drawn in the GUI, along with quick-fixes.  This is used
     *  when the script does not define bounds, resolution, etc. */
    QList<QPair<QString, QString>> warnings;
};

} // namespace Studio

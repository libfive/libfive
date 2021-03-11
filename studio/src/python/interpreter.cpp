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
#include <Python.h>
#include <QApplication>

#include "studio/python/interpreter.hpp"
#include "studio/documentation.hpp"
#include "studio/shape.hpp"

#include "libfive.h"

namespace Studio {
namespace Python {

Interpreter::Interpreter() {
    // Nothing to do here
}

QString Interpreter::defaultScript() {
    return "print('hello, world')";
}

void Interpreter::init() {
    Py_Initialize();
    emit(ready({}, Documentation()));
}

void Interpreter::eval(QString script)
{
    (void)script;

    emit(busy());

    Result out;
    out.okay = true;
    out.result = "Not yet implemented";

    emit(done(out));
}

}   // namespace Python
}   // namespace Studio

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

Interpreter::~Interpreter() {
    Py_XDECREF(m_runFunc);
}

QString Interpreter::defaultScript() {
    return "print('hello, world')";
}

void Interpreter::init() {
    static bool initialized = false;
    if (!initialized) {
        Py_Initialize();
        auto s = PyUnicode_FromString("libfive/bind/python");
        PyList_Insert(PySys_GetObject("path"), 0, s);
        Py_DECREF(s);
        initialized = true;
    }

    const auto runner_mod_name = PyUnicode_FromString("libfive.runner");
    const auto runner_mod = PyImport_Import(runner_mod_name);
    Py_DECREF(runner_mod_name);
    if (runner_mod != NULL) {
        m_runFunc = PyObject_GetAttrString(runner_mod, "run");
        if (!m_runFunc) {
            PyErr_Print();
        }
        Py_DECREF(runner_mod);
    } else {
        PyErr_Print();
    }
    emit(ready({}, Documentation()));
}

// Use PyErr_SetInterrupt to interrupt running script

void Interpreter::eval(QString script)
{
    const auto tuple = PyTuple_New(1);
    PyTuple_SetItem(tuple, 0, PyUnicode_FromString(script.toStdString().c_str()));

    const auto ret = PyObject_CallObject(m_runFunc, tuple);
    PyErr_Print();

    emit(busy());

    Result out;
    out.okay = true;
    out.result = "Not yet implemented";

    emit(done(out));

    Py_DECREF(tuple);
    Py_DECREF(ret);
}

}   // namespace Python
}   // namespace Studio

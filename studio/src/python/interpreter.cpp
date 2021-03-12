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

    const auto runner_mod = PyImport_ImportModule("libfive.runner");
    if (runner_mod != NULL) {
        m_runFunc = PyObject_GetAttrString(runner_mod, "run");
        if (!m_runFunc) {
            PyErr_Print();
        }
        Py_DECREF(runner_mod);
    } else {
        PyErr_Print();
    }

    QStringList keywords;
    const auto keyword_mod = PyImport_ImportModule("keyword");
    const auto kwlist = PyObject_GetAttrString(keyword_mod, "kwlist");
    const auto kwlist_len = PyList_Size(kwlist);
    for (unsigned i=0; i < kwlist_len; ++i) {
        const auto s = PyObject_Str(PyList_GetItem(kwlist, i));
        const auto ws = PyUnicode_AsWideCharString(s, NULL);
        keywords << QString::fromWCharArray(ws);
        PyMem_Free(ws);
        Py_DECREF(s);
    }
    Py_DECREF(kwlist);
    Py_DECREF(keyword_mod);
    emit(ready(keywords, Documentation()));
}

// Use PyErr_SetInterrupt to interrupt running script

void Interpreter::eval(QString script)
{
    emit(busy());

    const auto tuple = PyTuple_New(1);
    PyTuple_SetItem(tuple, 0,
            PyUnicode_FromString(script.toStdString().c_str()));
    const auto ret = PyObject_CallObject(m_runFunc, tuple);
    Py_DECREF(tuple);

    Result out;
    out.okay = !PyErr_Occurred();

    if (out.okay) {
        const auto ret_size = PyList_Size(ret);
        if (ret_size) {
            const auto s = PyObject_Str(PyList_GetItem(ret, ret_size - 1));
            const auto ws = PyUnicode_AsWideCharString(s, NULL);
            out.result = QString::fromWCharArray(ws);
            PyMem_Free(ws);
            Py_DECREF(s);
        } else {
            out.result = "None";
        }
    } else {
        PyObject *type, *value, *tb;
        PyErr_Fetch(&type, &value, &tb);
        const auto s = PyObject_Str(value);
        const auto ws = PyUnicode_AsWideCharString(s, NULL);
        PyErr_Print();
        out.error = {QString::fromWCharArray(ws), "", QRect()};
        PyMem_Free(ws);
        Py_XDECREF(s);
    }

    emit(done(out));
    Py_XDECREF(ret);
}

}   // namespace Python
}   // namespace Studio

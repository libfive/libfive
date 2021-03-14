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

// We inject a dummy module named "studio" into the Python namespace, then
// use it to control settings (resolution, bounds, etc)
extern "C" {
static struct PyModuleDef studio_module = {
    PyModuleDef_HEAD_INIT,
    "studio",   /* name of module */
    "Global parameters to control the Studio GUI",
    -1,
    NULL, // m_methods
    NULL, // m_slots
    NULL, // m_traverse
    NULL, // m_clear
    NULL, // m_free
};
PyMODINIT_FUNC PyInit_studio(void) {
    return PyModule_Create(&studio_module);
}
}    // extern "C"

namespace Studio {
namespace Python {

Interpreter::Interpreter() {
    // Nothing to do here
}

Interpreter::~Interpreter() {
    // The destructor is called from the main thread after the interpreter
    // thread has finished.  We reclaim the thread state, so the GIL lives
    // in the main thread until another Interpreter is created.
    PyEval_RestoreThread(m_threadState);

    Py_XDECREF(m_runFunc);
    Py_XDECREF(m_shapeClass);
}

QString Interpreter::defaultScript() {
    return "from libfive.stdlib import *\n\nsphere(1)";
}

void Interpreter::halt() {
    // Restore the thread state and claim the GIL
    PyEval_RestoreThread(m_threadState);

    // Interrupt the worker thread by ID
    PyThreadState_SetAsyncExc(m_workerThreadId, PyExc_Exception);

    // Then release the thread state again, saving it locally
    m_threadState = PyEval_SaveThread();
}

void Interpreter::preinit() {
    if (!Py_IsInitialized()) {
        PyImport_AppendInittab("studio", PyInit_studio);
        Py_Initialize();
        auto s = PyUnicode_FromString("libfive/bind/python");
        PyList_Insert(PySys_GetObject("path"), 0, s);
        Py_DECREF(s);

        // Create a dummy module for settings
        PyModule_Create(&studio_module);
    }
    m_threadState = PyEval_SaveThread();
}

void Interpreter::init() {
    PyGILState_STATE gstate = PyGILState_Ensure();

    // TODO: do a first import check and raise a reasonable error instead of
    // crashing if the module isn't available? (it *should* always be available)

    const auto runner_mod = PyImport_ImportModule("libfive.runner");
    PyErr_Print();
    m_runFunc = PyObject_GetAttrString(runner_mod, "run");
    Py_DECREF(runner_mod);

    const auto shape_mod = PyImport_ImportModule("libfive.shape");
    m_shapeClass = PyObject_GetAttrString(shape_mod, "Shape");
    Py_DECREF(shape_mod);

    const auto threading_mod = PyImport_ImportModule("threading");
    const auto get_ident = PyObject_GetAttrString(threading_mod, "get_ident");
    const auto thread_id = PyObject_CallFunctionObjArgs(get_ident, NULL);
    m_workerThreadId = PyLong_AsLong(thread_id);
    Py_DECREF(thread_id);
    Py_DECREF(get_ident);
    Py_DECREF(threading_mod);

    QStringList keywords;

    // Equivalent to str(obj), returning a QString
    auto str = [](PyObject* obj) -> QString {
        const auto s = PyObject_Str(obj);
        const auto ws = PyUnicode_AsWideCharString(s, NULL);
        const auto out = QString::fromWCharArray(ws);
        PyMem_Free(ws);
        Py_DECREF(s);
        return out;
    };
    auto dump_keywords = [&](PyObject* list) {
        const auto list_len = PyList_Size(list);
        for (unsigned i=0; i < list_len; ++i) {
            keywords << str(PyList_GetItem(list, i));
        }
    };

    {
        const auto keyword_mod = PyImport_ImportModule("keyword");
        const auto kwlist = PyObject_GetAttrString(keyword_mod, "kwlist");
        dump_keywords(kwlist);
        Py_DECREF(kwlist);
        Py_DECREF(keyword_mod);
    }

    {
        const auto builtins_mod = PyImport_ImportModule("builtins");
        const auto builtins_list = PyObject_Dir(builtins_mod);
        dump_keywords(builtins_list);
        Py_DECREF(builtins_list);
        Py_DECREF(builtins_mod);
    }

    Documentation docs;

    {   // Load all the keywords from the stdlib, then build the docs
        const auto stdlib_mod = PyImport_ImportModule("libfive.stdlib");
        const auto stdlib_list = PyObject_Dir(stdlib_mod);
        const auto stdlib_len = PyList_Size(stdlib_list);
        PyErr_Print();

        const auto inspect_mod = PyImport_ImportModule("inspect");
        const auto sig = PyObject_GetAttrString(inspect_mod, "signature");
        const auto getdoc = PyObject_GetAttrString(inspect_mod, "getdoc");
        const auto getmod = PyObject_GetAttrString(inspect_mod, "getmodule");
        PyErr_Print();

        for (unsigned i=0; i < stdlib_len; ++i) {
            const auto item = PyList_GetItem(stdlib_list, i); // borrowed ref
            const auto item_str = str(item);
            if (item_str.startsWith("_")) {
                continue;
            }
            keywords << item_str;

            // Don't add classes to the standard library documentation
            const auto item_obj = PyObject_GetAttr(stdlib_mod, item);
            if (!PyFunction_Check(item_obj)) {
                Py_DECREF(item_obj);
                continue;
            }
            const auto item_name_obj = PyObject_GetAttrString(
                    item_obj, "__name__");
            const auto item_name = str(item_name_obj);
            Py_DECREF(item_name_obj);

            const auto item_signature = PyObject_CallFunctionObjArgs(
                    sig, item_obj, NULL);
            const auto item_docstring = PyObject_CallFunctionObjArgs(
                    getdoc, item_obj, NULL);

            if (!item_signature || !item_docstring) {
                std::cerr << "Warning: missing documentation for "
                    << item_str.toStdString()
                    << "; " << str(item_signature).toStdString()
                    << "; " << str(item_docstring).toStdString();
            }
            const auto mod = PyObject_CallFunctionObjArgs(
                    getmod, item_obj, NULL);
            const auto mod_name = PyObject_GetAttrString(mod, "__name__");
            const auto mod_name_str = str(mod_name);
            Py_DECREF(mod_name);
            Py_DECREF(mod);

            docs[mod_name_str][item_str] =
                (item_name + str(item_signature) + "\n" +
                 str(item_docstring)).trimmed();

            Py_DECREF(item_obj);
            Py_XDECREF(item_signature);
            Py_XDECREF(item_docstring);
        }

        Py_DECREF(getdoc);
        Py_DECREF(getmod);
        Py_DECREF(sig);
        Py_DECREF(inspect_mod);
        Py_DECREF(stdlib_list);
        Py_DECREF(stdlib_mod);
    }

    PyErr_Print();

    PyGILState_Release(gstate);
    emit(ready(keywords, docs));
}

void Interpreter::eval(QString script)
{
    emit(busy());

    PyGILState_STATE gstate = PyGILState_Ensure();

    const auto tuple = PyTuple_New(1);
    PyTuple_SetItem(tuple, 0,
            PyUnicode_FromString(script.toStdString().c_str()));
    const auto ret = PyObject_CallObject(m_runFunc, tuple);
    Py_DECREF(tuple);

    Result out;
    out.settings = Settings::defaultSettings();
    out.okay = !PyErr_Occurred();

    if (out.okay) {
        const auto ret_size = PyList_Size(ret);
        if (ret_size) {
            const auto s = PyObject_Repr(PyList_GetItem(ret, ret_size - 1));
            const auto ws = PyUnicode_AsWideCharString(s, NULL);
            out.result = QString::fromWCharArray(ws);
            PyMem_Free(ws);
            Py_DECREF(s);
        } else {
            out.result = "None";
        }
        for (unsigned i=0; i < ret_size; ++i) {
            const auto s = PyList_GetItem(ret, i);
            if (PyObject_IsInstance(s, m_shapeClass)) {
                const auto ptr_obj = PyObject_GetAttrString(s, "ptr");
                const auto ptr = PyLong_AsVoidPtr(ptr_obj);
                const auto tree = static_cast<libfive_tree>(ptr);
                const auto shape = new Shape(libfive::Tree(tree), {});
                shape->moveToThread(QApplication::instance()->thread());
                out.shapes.push_back(shape);

                Py_DECREF(ptr_obj);
                PyErr_Print();
            }
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
    Py_XDECREF(ret);
    PyGILState_Release(gstate);

    emit(done(out));
}

}   // namespace Python
}   // namespace Studio

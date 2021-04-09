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
static PyObject* set_resolution(PyObject* self, PyObject* args) {
    double res;
    if (!PyArg_ParseTuple(args, "d", &res)) {
        return NULL;
    }
    PyObject_SetAttrString(self, "__resolution", PyFloat_FromDouble(res));
    Py_RETURN_NONE;
}

static PyObject* set_quality(PyObject* self, PyObject* args) {
    double qua;
    if (!PyArg_ParseTuple(args, "d", &qua)) {
        return NULL;
    }
    PyObject_SetAttrString(self, "__quality", PyFloat_FromDouble(qua));
    Py_RETURN_NONE;
}

static PyObject* set_bounds(PyObject* self, PyObject* args) {
    double xmin, ymin, zmin, xmax, ymax, zmax;
    if (!PyArg_ParseTuple(args, "(ddd)(ddd)", &xmin, &ymin, &zmin,
                          &xmax, &ymax, &zmax))
    {
        // The error flag is set by PyArg_ParseTuple
        return NULL;
    }
    PyObject_SetAttrString(self, "__bounds", args);
    Py_RETURN_NONE;
}

static PyObject* var_func(PyObject* studio_mod, PyObject* args) {
    if (PyTuple_Size(args) == 1) {
        const char* err_str;
        if (!PyArg_ParseTuple(args, "s", &err_str)) {
            return NULL;
        } else {
            PyErr_SetString(PyExc_RuntimeError, err_str);
            return NULL;
        }
    }
    // Parse the argument, which should be patched in the AST by runner.run
    double value;
    int lineno, end_lineno, col_offset, end_col_offset;
    if (!PyArg_ParseTuple(args, "d(iiii)", &value,
        &lineno, &end_lineno, &col_offset, &end_col_offset))
    {
            return NULL;
    }

    const auto prev_vars = PyObject_GetAttrString(studio_mod, "__prev_vars");
    const auto vars = PyObject_GetAttrString(studio_mod, "__vars");
    const auto num_vars = PyList_Size(vars);
    PyErr_Print();

    PyObject* v;
    if (num_vars < PyList_Size(prev_vars)) {
        // Pull just the Shape object from the tuple
        auto item = PyList_GetItem(prev_vars, num_vars); // borrowed
        v = PyTuple_GetItem(item, 0); // borrowed
        Py_INCREF(v);
    } else {
        // Build a new free variable using Shape.var()
        const auto shape_mod = PyImport_ImportModule("libfive.shape");
        const auto Shape = PyObject_GetAttrString(shape_mod, "Shape");
        v = PyObject_CallMethod(Shape, "var", NULL); // new reference
        Py_DECREF(shape_mod);
        Py_DECREF(Shape);
    }

    const auto new_var = Py_BuildValue("Od(iiii)",
        v, value, lineno, end_lineno, col_offset, end_col_offset);
    PyList_Append(vars, new_var);

    Py_DECREF(new_var);
    Py_DECREF(vars);
    Py_DECREF(prev_vars);
    PyErr_Print();

    return v;
}

static PyMethodDef studio_methods[] = {
    {"set_resolution", set_resolution, METH_VARARGS, "Sets render resolution"},
    {"set_quality", set_quality, METH_VARARGS, "Sets render quality"},
    {"set_bounds", set_bounds, METH_VARARGS, "Sets render bounds"},
    {"__var", var_func, METH_VARARGS, "Constructs a free variable"},
    {NULL, NULL, 0, NULL}        /* Sentinel */
};

static struct PyModuleDef studio_module = {
    PyModuleDef_HEAD_INIT,
    "studio",   /* name of module */
    "Global parameters to control the Studio GUI",
    -1,
    studio_methods, // m_methods
    NULL, // m_slots
    NULL, // m_traverse
    NULL, // m_clear
    NULL, // m_free
};

PyMODINIT_FUNC PyInit_studio(void) {
    return PyModule_Create(&studio_module);
}

////////////////////////////////////////////////////////////////////////////////

namespace Studio {
namespace Python {

const static QString SET_QUALITY_STR = "studio.set_quality(%1)\n";
const static QString SET_RESOLUTION_STR = "studio.set_resolution(%1)\n";
const static QString SET_BOUNDS_STR = "studio.set_bounds([%1, %2, %3], "
                                                        "[%4, %5, %6])\n";

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
    Py_XDECREF(m_varFunc);
}

QString Interpreter::defaultScript() {
    auto default_settings = Settings::defaultSettings();
    return "import studio\n" +
        SET_BOUNDS_STR.arg(default_settings.min.x())
                      .arg(default_settings.min.y())
                      .arg(default_settings.min.z())
                      .arg(default_settings.max.x())
                      .arg(default_settings.max.y())
                      .arg(default_settings.max.z()) +
        SET_QUALITY_STR.arg(default_settings.quality) +
        SET_RESOLUTION_STR.arg(default_settings.res) +
        "\nfrom libfive.stdlib import *\nsphere(1)";
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
        QDir app_dir;

#ifdef Q_OS_WIN
        // On Windows, we also write the PYTHONHOME environment variable, so
        // that Python can find itself.  We assume that things were built per
        // the README, i.e. a directory named vcpkg above the build directory
        // (although this will also catch a global vcpkg in C:\vcpkg)
        const auto home_dir = QDir::toNativeSeparators(
                "vcpkg/installed/x64-windows/tools/python3");
        app_dir = QDir(QCoreApplication::applicationDirPath());
        do {
            if (app_dir.exists(home_dir)) {
                qputenv("PYTHONHOME",
                        app_dir.filePath(home_dir).toLocal8Bit());
                break;
            }
        } while (app_dir.cdUp());
#endif

        PyImport_AppendInittab("studio", PyInit_studio);
        Py_Initialize();

        // Walk up directories, looking for the libfive/bind/python subdir
        // (where the Python bindings live), then adding it to sys.path
        //
        // Depending on the OS, we'll have to walk up a different number of
        // directories, so we'll be flexible here.
        const auto bind_dir = QDir::toNativeSeparators("libfive/bind/python");
        app_dir = QDir(QCoreApplication::applicationDirPath());
        do {
            if (app_dir.exists(bind_dir)) {
                const auto s = PyUnicode_FromString(
                    app_dir.filePath(bind_dir)
                        .toLocal8Bit().data());
                PyList_Insert(PySys_GetObject("path"), 1, s);
                Py_DECREF(s);
                break;
            }
        } while (app_dir.cdUp());

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
    PyErr_Print();
    m_shapeClass = PyObject_GetAttrString(shape_mod, "Shape");
    Py_DECREF(shape_mod);

    const auto threading_mod = PyImport_ImportModule("threading");
    const auto get_ident = PyObject_GetAttrString(threading_mod, "get_ident");
    const auto thread_id = PyObject_CallFunctionObjArgs(get_ident, NULL);
    m_workerThreadId = PyLong_AsLong(thread_id);
    Py_DECREF(thread_id);
    Py_DECREF(get_ident);
    Py_DECREF(threading_mod);

    // Store the var() function locally for easy access
    const auto studio_mod = PyImport_ImportModule("studio");
    m_varFunc = PyObject_GetAttrString(studio_mod, "__var");

    // Reset studio.__vars to an empty list
    const auto empty_list = PyList_New(0);
    PyObject_SetAttrString(studio_mod, "__vars", empty_list);
    Py_DECREF(empty_list);
    Py_DECREF(studio_mod);

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

    {   // Highlight all Python keywords
        const auto keyword_mod = PyImport_ImportModule("keyword");
        const auto kwlist = PyObject_GetAttrString(keyword_mod, "kwlist");
        dump_keywords(kwlist);
        Py_DECREF(kwlist);
        Py_DECREF(keyword_mod);
    }

    {   // Highlight all Python builtins
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

    // Reset all settings
    const auto studio_mod = PyImport_ImportModule("studio");
    PyObject_SetAttrString(studio_mod, "__resolution", Py_None);
    PyObject_SetAttrString(studio_mod, "__quality", Py_None);
    PyObject_SetAttrString(studio_mod, "__bounds", Py_None);

    // Copy from __vars to __prev_vars, then reset __vars
    const auto prev_vars = PyObject_GetAttrString(studio_mod, "__vars");
    PyObject_SetAttrString(studio_mod, "__prev_vars", prev_vars);
    Py_DECREF(prev_vars);
    const auto vars_list = PyList_New(0);
    PyObject_SetAttrString(studio_mod, "__vars", vars_list);

    auto args = PyTuple_New(1);
    PyTuple_SetItem(args, 0,
            PyUnicode_FromString(script.toLocal8Bit().data()));
    auto kwargs = PyDict_New();
    PyDict_SetItemString(kwargs, "var", m_varFunc);
    const auto ret = PyObject_Call(m_runFunc, args, kwargs);
    Py_DECREF(args);
    Py_DECREF(kwargs);

    Result out;
    out.settings = Settings::defaultSettings();
    out.okay = !PyErr_Occurred();

    // Map storing vars and their desired values (with the mapping to positions
    // stored elsewhere in the Result structure)
    std::map<libfive::Tree::Id, float> vars;

    if (out.okay) {
        // Parse vars from studio.__vars
        const auto vars_size = PyList_Size(vars_list);
        for (unsigned i=0; i < vars_size; ++i) {
            PyObject* s;
            double value;
            int lineno, end_lineno, col_offset, end_col_offset;
            const auto item = PyList_GetItem(vars_list, i);

            PyArg_ParseTuple(item, "Od(iiii)", &s, &value,
                &lineno, &end_lineno, &col_offset, &end_col_offset);
            PyErr_Print();

            // Link up the ID with the value and position in the text
            const auto ptr_obj = PyObject_GetAttrString(s, "ptr");
            PyErr_Print();
            const auto ptr = PyLong_AsVoidPtr(ptr_obj);
            PyErr_Print();
            const auto id = static_cast<libfive::Tree::Id>(ptr);
            vars[id] = value;
            out.vars[id] = QRect(col_offset, lineno - 1,
                                 end_col_offset - col_offset,
                                 end_lineno - lineno);
            Py_DECREF(ptr_obj);
            PyErr_Print();
        }

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
                const auto shape = new Shape(libfive::Tree(tree), vars);
                shape->moveToThread(QApplication::instance()->thread());
                out.shapes.push_back(shape);

                Py_DECREF(ptr_obj);
                PyErr_Print();
            }
        }

        // Check whether settings were assigned in the script
        auto res = PyObject_GetAttrString(studio_mod, "__resolution");
        if (res != Py_None) {
            const double r = PyFloat_AsDouble(res);
            if (!PyErr_Occurred()) {
                out.settings.res = r;
            } else {
                res = NULL;
            }
        }
        if (res == Py_None || res == NULL) {
            out.warnings.append({
                    "<b>Warning:</b> Using default resolution for shapes.<br>"
                    "&nbsp;&nbsp;&nbsp;&nbsp;"
                    "Assign to <code>studio.resolution</code> to specify.",
                    SET_RESOLUTION_STR.arg(out.settings.res)});
        }
        Py_XDECREF(res);

        auto qua = PyObject_GetAttrString(studio_mod, "__quality");
        if (qua != Py_None) {
            const double q = PyFloat_AsDouble(qua);
            if (!PyErr_Occurred()) {
                out.settings.quality = q;
            } else {
                qua = NULL;
            }
        }
        if (qua == Py_None || qua == NULL) {
            out.warnings.append({
                    "<b>Warning:</b> Using default quality for shapes.<br>"
                    "&nbsp;&nbsp;&nbsp;&nbsp;"
                    "Assign to <code>studio.quality</code> to specify.",
                    SET_QUALITY_STR.arg(out.settings.quality)});
        }
        Py_XDECREF(qua);

        auto bounds = PyObject_GetAttrString(studio_mod, "__bounds");
        if (bounds != Py_None) {
            double xmin, ymin, zmin, xmax, ymax, zmax;
            if (PyArg_ParseTuple(bounds, "(ddd)(ddd)", &xmin, &ymin, &zmin,
                                  &xmax, &ymax, &zmax))
            {
                out.settings.min = QVector3D(xmin, ymin, zmin);
                out.settings.max = QVector3D(xmax, ymax, zmax);
            } else {
                PyErr_Clear();
                bounds = NULL;
            }
        }
        if (bounds == Py_None || bounds == NULL) {
            out.warnings.append(
                    {"<b>Warning:</b> Using default bounds for shapes<br>"
                     "&nbsp;&nbsp;&nbsp;&nbsp;"
                     "Assign to <code>studio.bounds</code> to specify.",
                    SET_BOUNDS_STR.arg(out.settings.min.x())
                                  .arg(out.settings.min.y())
                                  .arg(out.settings.min.z())
                                  .arg(out.settings.max.x())
                                  .arg(out.settings.max.y())
                                  .arg(out.settings.max.z())});
        }
        Py_XDECREF(bounds);

        if (!out.warnings.isEmpty() && !script.contains("import studio")) {
            out.warnings.insert(0, {
                "<b>Warning:</b> Missing import of <code>studio</code> module",
                "import studio\n"});
        }
    } else {
        // Stash the error, because it will be cleared by the code below
        PyObject *type, *value, *traceback;
        PyErr_Fetch(&type, &value, &traceback);

        // Replace sys.stderr with an io.StringIO object, so that we can capture
        // the printed traceback into a string.
        auto io_mod = PyImport_ImportModule("io");
        auto string_io = PyObject_GetAttrString(io_mod, "StringIO");
        auto new_stderr = PyObject_CallObject(string_io, NULL);
        PySys_SetObject("stderr", new_stderr);

        // Print the error to our StringIO, then decode it into a QString
        // and attach it to the returned value.
        PyErr_Restore(type, value, traceback);
        PyErr_Print();

        const auto s = PyObject_CallMethod(new_stderr, "getvalue", NULL);
        const auto ws = PyUnicode_AsWideCharString(s, NULL);
        out.error = {QString::fromWCharArray(ws), QRect()};

        // Restore previous stderr
        PySys_SetObject("stderr", PySys_GetObject("__stderr__"));

        // Cleanup!
        PyMem_Free(ws);
        Py_DECREF(s);
        Py_DECREF(new_stderr);
        Py_DECREF(string_io);
        Py_DECREF(io_mod);
    }

    Py_DECREF(vars_list);
    Py_XDECREF(ret);
    PyGILState_Release(gstate);

    emit(done(out));
}

}   // namespace Python
}   // namespace Studio

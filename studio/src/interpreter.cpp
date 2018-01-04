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

#include "studio/interpreter.hpp"
#include "studio/documentation.hpp"
#include "studio/shape.hpp"

#include "libfive-guile.h"

_Interpreter::_Interpreter()
{
    connect(&thread, &QThread::started, this, &_Interpreter::init);
    moveToThread(&thread);
}

void _Interpreter::init()
{
    // Modify environmental variables to use local Guile path
    auto path = QCoreApplication::applicationDirPath().toLocal8Bit() +
                "/../Resources/guile/";
    qputenv("GUILE_LOAD_COMPILED_PATH", path + "ccache/");
    qputenv("GUILE_LOAD_PATH", path + "scm/");

    scm_init_guile();

    scm_init_libfive_modules();
    scm_c_use_module("libfive kernel");

    scm_eval_sandboxed = scm_c_eval_string(R"(
(use-modules (libfive sandbox))
eval-sandboxed
)");

    scm_port_eof_p = scm_c_eval_string(R"(
(use-modules (rnrs io ports))
port-eof?
)");
    scm_syntax_error_sym = scm_from_utf8_symbol("syntax-error");
    scm_numerical_overflow_sym = scm_from_utf8_symbol("numerical-overflow");
    scm_valid_sym = scm_from_utf8_symbol("valid");
    scm_result_fmt = scm_from_locale_string("~S");
    scm_other_error_fmt = scm_from_locale_string("~A: ~A");
    scm_syntax_error_fmt = scm_from_locale_string("~A: ~A in form ~A");
    scm_numerical_overflow_fmt = scm_from_locale_string("~A: ~A in ~A");

    // Protect all of our interpreter vars from garbage collection
    for (auto s : {scm_eval_sandboxed, scm_port_eof_p, scm_valid_sym,
                   scm_syntax_error_sym, scm_numerical_overflow_sym,
                   scm_result_fmt, scm_syntax_error_fmt,
                   scm_numerical_overflow_fmt, scm_other_error_fmt})
    {
        scm_permanent_object(s);
    }

    //  Extract a list of keywords from our list of sandbox-safe symbols
    auto kws = scm_to_locale_string(scm_c_eval_string(R"(
(string-drop (string-drop-right
    (format #f "~A" (apply append (map cdr sandbox-bindings))) 1) 1)
)"));
    emit(keywords(kws));
    free(kws);

    // Extract a list of function names + docstrings
    QList<QString> modules = {"(libfive shapes)",
                              "(libfive csg)",
                              "(libfive transforms)"};
    Documentation* ds = new Documentation;
    for (auto mod : modules)
    {
        auto f = scm_c_eval_string((R"(
        (module-map (lambda (sym var)
            (cons (symbol->string sym)
                  (if (procedure? (variable-ref var))
                    (procedure-documentation (variable-ref var))
                    "")))
          (resolve-interface ')" + mod + "))").toLocal8Bit().constData());
        for (; !scm_is_null(f); f = scm_cdr(f))
        {
            auto name = scm_to_locale_string(scm_caar(f));
            auto doc = scm_to_locale_string(scm_cdar(f));
            if (strlen(doc))
            {
                ds->insert(mod, name, doc);
            }
            free(name);
            free(doc);
        }
    }
    emit(docs(ds));
}

void _Interpreter::eval()
{
    // Safely read in the text of the script
    QString script;
    {
        QMutexLocker lock(&mutex);
        script = _script;
    }

    auto result = scm_call_1(scm_eval_sandboxed,
            scm_from_locale_string(script.toLocal8Bit().data()));

    //  Loop through the whole result list, looking for an invalid clause
    bool valid = true;
    for (auto r = result; !scm_is_null(r) && valid; r = scm_cdr(r))
    {
        if (!scm_is_eq(scm_caar(r), scm_valid_sym))
        {
            valid = false;
        }
    }

    // If there is at least one result, then we'll convert the last one
    // into a string (with special cases for various error forms)
    auto last = scm_is_null(result) ? nullptr
                                    : scm_cdr(scm_car(scm_last_pair(result)));
    if (!valid)
    {
        /* last = '(before after key params) */
        auto before = scm_car(last);
        auto after = scm_cadr(last);
        auto key = scm_caddr(last);
        auto params = scm_cadddr(last);

        auto _stack = scm_car(scm_cddddr(last));
        SCM _str = nullptr;

        if (scm_is_eq(key, scm_syntax_error_sym))
        {
            _str = scm_simple_format(SCM_BOOL_F, scm_syntax_error_fmt,
                   scm_list_3(key, scm_cadr(params), scm_cadddr(params)));
        }
        else if (scm_is_eq(key, scm_numerical_overflow_sym))
        {
            _str = scm_simple_format(SCM_BOOL_F, scm_numerical_overflow_fmt,
                   scm_list_3(key, scm_cadr(params), scm_car(params)));
        }
        else
        {
            _str = scm_simple_format(SCM_BOOL_F, scm_other_error_fmt,
                   scm_list_2(key, scm_simple_format(
                        SCM_BOOL_F, scm_cadr(params), scm_caddr(params))));
        }
        auto str = scm_to_locale_string(_str);
        auto stack = scm_to_locale_string(_stack);
        emit(gotError(QString(str), QString(stack),
                    {scm_to_int(scm_car(before)),
                     scm_to_int(scm_car(after)),
                     scm_to_int(scm_cdr(before)),
                     scm_to_int(scm_cdr(after))}));
        free(str);
        free(stack);
    }
    else if (last)
    {
        char* str = nullptr;
        if (scm_to_int64(scm_length(last)) == 1)
        {
            auto str = scm_to_locale_string(
                    scm_simple_format(SCM_BOOL_F, scm_result_fmt,
                                      scm_list_1(scm_car(last))));
            emit(gotResult(QString(str)));
        }
        else
        {
            auto str = scm_to_locale_string(
                    scm_simple_format(SCM_BOOL_F, scm_result_fmt,
                                      scm_list_1(last)));
            emit(gotResult("(values " + QString(str) + ")"));
        }
        free(str);
    }
    else
    {
        emit(gotResult("#<eof>"));
    }

    // Then iterate over the results, picking out shapes
    if (valid)
    {
        QList<Shape*> shapes;

        // Initialize variables and their textual positions
        std::map<Kernel::Tree::Id, float> vars;
        QMap<Kernel::Tree::Id, Editor::Range> var_pos;

        {   // Walk through the global variable map
            auto vs = scm_c_eval_string(R"(
                (use-modules (libfive sandbox))
                (hash-map->list (lambda (k v) v) vars) )");

            for (auto v = vs; !scm_is_null(v); v = scm_cdr(v))
            {
                auto data = scm_cdar(v);
                auto id = static_cast<Kernel::Tree::Id>(
                        libfive_tree_id(scm_to_tree(scm_car(data))));
                auto value = scm_to_double(scm_cadr(data));
                vars[id] = value;

                auto vp = scm_caddr(data);
                var_pos[id] = {scm_to_int(scm_car(vp)), 0,
                               scm_to_int(scm_cadr(vp)),
                               scm_to_int(scm_caddr(vp))};
            }
        }

        // Then walk through the result list, picking out trees
        while (!scm_is_null(result))
        {
            for (auto r = scm_cdar(result); !scm_is_null(r); r = scm_cdr(r))
            {
                if (scm_is_tree(scm_car(r)))
                {
                    auto tree = scm_to_tree(scm_car(r));
                    auto shape = new Shape(*tree, vars);
                    shape->moveToThread(QApplication::instance()->thread());
                    shapes.push_back(shape);
                }
            }
            result = scm_cdr(result);
        }
        emit(gotShapes(shapes));
        emit(gotVars(var_pos));
    }
}


////////////////////////////////////////////////////////////////////////////////

Interpreter::Interpreter()
{
    // Start evaluation some amount of time after the most recent change
    // to the script (should feel responsive, but we don't need to re-evaluate
    // every single character change).
    eval_timer.setSingleShot(true);
    eval_timer.setInterval(250);
    connect(&eval_timer, &QTimer::timeout, &interpreter, &_Interpreter::eval);

    // After the interpreter is kicked off, then emit the busy signal after
    // a short delay.  This is cancelled by either of the interpreter's
    // "done" signals (gotResult and gotError), to avoid jittering the UI
    // during very short evaluations.
    busy_timer.setSingleShot(true);
    busy_timer.setInterval(100);
    connect(&eval_timer, &QTimer::timeout, &busy_timer,
            QOverload<>::of(&QTimer::start));
    connect(&busy_timer, &QTimer::timeout, this, &Interpreter::busy);
    connect(&interpreter, &_Interpreter::gotResult,
            &busy_timer, [&](QString){ busy_timer.stop(); });
    connect(&interpreter, &_Interpreter::gotError, &busy_timer,
            [&](QString, QString, Editor::Range){ busy_timer.stop(); });

    // Forward all signals from _Interpreter (running in its own thread)
    connect(&interpreter, &_Interpreter::gotResult,
            this, &Interpreter::gotResult);
    connect(&interpreter, &_Interpreter::gotError,
            this, &Interpreter::gotError);
    connect(&interpreter, &_Interpreter::keywords,
            this, &Interpreter::keywords);
    connect(&interpreter, &_Interpreter::docs,
            this, &Interpreter::docs);
    connect(&interpreter, &_Interpreter::gotShapes,
            this, &Interpreter::gotShapes);
    connect(&interpreter, &_Interpreter::gotVars,
            this, &Interpreter::gotVars);
}

void Interpreter::start()
{
    interpreter.thread.start();
}

void Interpreter::onScriptChanged(QString s)
{
    {   // Swap the script into the other thread
        QMutexLocker lock(&interpreter.mutex);
        interpreter._script = s;
    }
    eval_timer.start();
}

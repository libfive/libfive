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

static void init_studio_gui(void*)
{
    scm_c_eval_string(R"(
(use-modules (libfive vec) (oop goops))

(define (vec3? v)
    (eq? (class-name (class-of v)) '<vec3>))

(define-public global-bounds #f)
(define-public (set-bounds! lower upper)
  "set-bounds! [xmin ymin zmin] [xmax ymax zmax]
  Sets the global render bounds"
  (when (not (and (vec3? lower) (vec3? upper)))
    (error "Arguments must be vec3"))
  (when (not (and (number? (.x lower))
                  (number? (.y lower))
                  (number? (.z lower))
                  (number? (.x upper))
                  (number? (.y upper))
                  (number? (.z upper))))
    (error "vec3 values must be numbers"))
  (set! global-bounds (cons lower upper)))

(define-public global-resolution #f)
(define-public (set-resolution! res)
  "set-resolution! res
  Sets the global render resolution, which is the
  reciprocal of minimum feature size"
  (if (number? res)
    (set! global-resolution res)
    (error "resolution must be a number")))

(define-public global-quality #f)
(define-public (set-quality! q)
  "set-quality! q
  Sets the global render quality, which is a metric
  from 1 to 11 that determines how enthusiastically
  triangles are collapsed in the mesh"
  (when (not (number? q))
    (error "quality must be a number"))
  (when (< q 1)
    (error "quality must be >= 1"))
  (when (> q 11)
    (error "quality must be <= 11"))
  (set! global-quality q))
)");
}

void _Interpreter::init()
{
#ifdef Q_OS_MAC
    // Modify environmental variables to use local Guile path
    auto path = QCoreApplication::applicationDirPath().toLocal8Bit() +
                "/../Resources/guile/";
    qputenv("GUILE_LOAD_COMPILED_PATH", path + "ccache/");
    qputenv("GUILE_LOAD_PATH", path + "scm/");
#endif

    scm_init_guile();

    scm_init_libfive_modules();
    scm_c_define_module("studio gui", init_studio_gui, NULL);

    scm_c_eval_string(R"(
(use-modules (libfive sandbox))
(set! sandbox-bindings (append! sandbox-bindings
    '(((studio gui) set-bounds! set-resolution! set-quality!))))
    )");

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
    scm_in_function_fmt = scm_from_locale_string("In function ~A:\n~A");
    scm_syntax_error_fmt = scm_from_locale_string("~A: ~A in form ~A");
    scm_numerical_overflow_fmt = scm_from_locale_string("~A: ~A in ~A");

    // Protect all of our interpreter vars from garbage collection
    for (auto s : {scm_eval_sandboxed, scm_port_eof_p, scm_valid_sym,
                   scm_syntax_error_sym, scm_numerical_overflow_sym,
                   scm_result_fmt, scm_syntax_error_fmt,
                   scm_numerical_overflow_fmt, scm_other_error_fmt,
                   scm_in_function_fmt})
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

    // Clear global bounds, so we can detect if they were set in the script
    scm_c_eval_string(R"(
    (use-modules (studio gui))
    (set! global-bounds #f)
    )");

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
        if (!scm_is_false(scm_car(params)))
        {
            _str = scm_simple_format(SCM_BOOL_F, scm_in_function_fmt,
                                     scm_list_2(scm_car(params), _str));
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
        emit(gotWarnings({}));
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
                        libfive_tree_id(scm_get_tree(scm_car(data))));
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
                if (scm_is_shape(scm_car(r)))
                {
                    auto tree = scm_get_tree(scm_car(r));
                    auto shape = new Shape(*tree, vars);
                    shape->moveToThread(QApplication::instance()->thread());
                    shapes.push_back(shape);
                }
            }
            result = scm_cdr(result);
        }
        emit(gotShapes(shapes));
        emit(gotVars(var_pos));

        // Detect variables that should be set in the script but were not,
        // and emit warnings for them (as well as storing reasonable defaults)
        auto bounds = scm_c_eval_string(R"(
        (use-modules (studio gui)) global-bounds
        )");

        auto resolution = scm_c_eval_string(R"(
        (use-modules (studio gui)) global-resolution
        )");

        auto quality = scm_c_eval_string(R"(
        (use-modules (studio gui)) global-quality
        )");

        QList<QPair<QString, QString>> warnings;
        if (scm_is_false(bounds))
        {
            warnings.append({"<b>Warning:</b> Using default bounds for shapes<br>"
                             "    Use <code>set-bounds!</code> to specify.",
                    "(set-bounds! [-10 -10 -10] [10 10 10])\n"});
        }
        if (scm_is_false(resolution))
        {
            warnings.append({"<b>Warning:</b> Using default resolution for shapes.<br>"
                             "    Use <code>set-resolution!</code> to specify.",
                    "(set-resolution! 10)\n"});
        }
        if (scm_is_false(quality))
        {
            warnings.append({"<b>Warning:</b> Using default quality for shapes.<br>"
                             "    Use <code>set-quality!</code> to specify.",
                    "(set-quality! 8)\n"});
        }
        emit(gotWarnings(warnings));
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
            static_cast<void (QTimer::*)()>(&QTimer::start));
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
    connect(&interpreter, &_Interpreter::gotWarnings,
            this, &Interpreter::gotWarnings);
    connect(&interpreter, &_Interpreter::keywords,
            this, &Interpreter::keywords);
    connect(&interpreter, &_Interpreter::docs,
            this, &Interpreter::docs);
    connect(&interpreter, &_Interpreter::gotShapes,
            this, &Interpreter::gotShapes);
    connect(&interpreter, &_Interpreter::gotVars,
            this, &Interpreter::gotVars);
    connect(&interpreter, &_Interpreter::gotSettings,
            this, &Interpreter::gotSettings);
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

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
#include <QApplication>

#include "studio/guile/interpreter.hpp"
#include "studio/documentation.hpp"
#include "studio/shape.hpp"

#include "libfive.h"

namespace Studio {
namespace Guile {

const static QString SET_QUALITY = "(set-quality! %1)\n";
const static QString SET_RESOLUTION = "(set-resolution! %1)\n";
const static QString SET_BOUNDS = "(set-bounds! [%1 %2 %3] [%4 %5 %6])\n";

Interpreter::Interpreter() {
    // Nothing to do here
}

QString Interpreter::defaultScript() {
    QString script;
    auto default_settings = Settings::defaultSettings();
    script += SET_BOUNDS.arg(default_settings.min.x())
                        .arg(default_settings.min.y())
                        .arg(default_settings.min.z())
                        .arg(default_settings.max.x())
                        .arg(default_settings.max.y())
                        .arg(default_settings.max.z());
    script += SET_QUALITY.arg(default_settings.quality);
    script += SET_RESOLUTION.arg(default_settings.res);
    return script;
}

void Interpreter::init() {
#ifdef Q_OS_MAC
    const auto app_dir = QCoreApplication::applicationDirPath().toLocal8Bit();

    // We include a pre-compiled Guile standard library in the Mac app bundle,
    // then fall back to assuming we're in the the build directory, then
    // fall back to standard system library search paths
    qputenv("GUILE_LOAD_COMPILED_PATH", "/../Resources/guile/ccache/:libfive/bind/guile");

    // This hints at Guile where to find libfive.dylib
    qputenv("LIBFIVE_FRAMEWORK_DIR", app_dir + "/../Frameworks/");
#else
    // Assume that we're in the build directory, then fall back to Linux
    // system libraries (if the Studio executable was installed, then the Guile
    // precompiled files should be as well).
    qputenv("GUILE_LOAD_COMPILED_PATH", "libfive/bind/guile");
#endif

    scm_init_guile();

    scm_c_use_module("libfive kernel");
    scm_eval_sandboxed = scm_c_eval_string(R"(
(use-modules (libfive sandbox))
eval-sandboxed
)");
    scm_shape_to_ptr = scm_c_eval_string(R"(
(use-modules (libfive kernel))
shape->ptr
)");
    scm_is_shape = scm_c_eval_string(R"(
(use-modules (libfive kernel))
shape?
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
    for (auto s : {scm_eval_sandboxed, scm_shape_to_ptr, scm_is_shape,
                   scm_port_eof_p, scm_valid_sym,
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
    //emit(keywords(kws)); // TODO
    free(kws);

    // Extract a list of function names + docstrings
    QList<QString> modules = {"(libfive stdlib shapes)",
                              "(libfive stdlib csg)",
                              "(libfive stdlib transforms)"};
    Documentation* ds = new Documentation;
    for (auto mod : modules)
    {
        auto f = scm_c_eval_string((R"(
        (module-map (lambda (sym var)
            (cons (symbol->string sym)
                  (if (procedure? (variable-ref var))
                    (or (procedure-documentation (variable-ref var)) "")
                    #f)))
          (resolve-interface ')" + mod + "))").toLocal8Bit().constData());
        for (; !scm_is_null(f); f = scm_cdr(f))
        {
            // Filter out symbols in the module which aren't procedures
            // (and therefore had their cdr set to #f)
            if (scm_is_false(scm_cdar(f))) {
                continue;
            }
            auto name = scm_to_locale_string(scm_caar(f));
            auto doc = scm_to_locale_string(scm_cdar(f));
            if (strlen(doc))
            {
                ds->insert(mod, name, doc);
            } else {
                std::cerr << "Warning: missing documentation for "
                          << name << "\n";
            }
            free(name);
            free(doc);
        }
    }
    // emit(docs(ds)); // TODO
    emit(ready());
}

void Interpreter::eval(QString script)
{
    emit(busy());

    Result out;

    // Clear global bounds, so we can detect if they were set in the script
    scm_c_eval_string(R"(
    (use-modules (libfive sandbox))
    (set! global-bounds #f)
    (set! global-quality #f)
    (set! global-resolution #f)
    )");

    auto result = scm_call_1(scm_eval_sandboxed,
            scm_from_locale_string(script.toLocal8Bit().data()));

    //  Loop through the whole result list, looking for an invalid clause
    for (auto r = result; !scm_is_null(r) && out.okay; r = scm_cdr(r))
    {
        if (!scm_is_eq(scm_caar(r), scm_valid_sym))
        {
            out.okay = false;
        }
    }

    // If there is at least one result, then we'll convert the last one
    // into a string (with special cases for various error forms)
    auto last = scm_is_null(result) ? nullptr
                                    : scm_cdr(scm_car(scm_last_pair(result)));
    if (!out.okay)
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
        out.error = Error {
            QString(str), QString(stack),
            {scm_to_int(scm_car(before)),
             scm_to_int(scm_car(after)),
             scm_to_int(scm_cdr(before)),
             scm_to_int(scm_cdr(after))}};
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
            out.result = QString(str);
        }
        else
        {
            auto str = scm_to_locale_string(
                    scm_simple_format(SCM_BOOL_F, scm_result_fmt,
                                      scm_list_1(last)));
            out.result = "(values " + QString(str) + ")";
        }
        free(str);
    }
    else
    {
        out.result = "#<eof>";
    }

    // Then iterate over the results, picking out shapes
    if (out.okay)
    {
        // Initialize variables and their textual positions
        std::map<libfive::Tree::Id, float> vars;

        {   // Walk through the global variable map
            auto vs = scm_c_eval_string(R"(
                (use-modules (libfive sandbox))
                (hash-map->list (lambda (k v) v) vars) )");

            for (auto v = vs; !scm_is_null(v); v = scm_cdr(v))
            {
                auto data = scm_cdar(v);
                auto id = static_cast<libfive::Tree::Id>(
                    scm_to_pointer(scm_call_1(scm_shape_to_ptr, scm_car(data))));
                auto value = scm_to_double(scm_cadr(data));
                vars[id] = value;

                auto vp = scm_caddr(data);
                out.vars[id] = {scm_to_int(scm_car(vp)), 0,
                                scm_to_int(scm_cadr(vp)),
                                scm_to_int(scm_caddr(vp))};
            }
        }

        // Then walk through the result list, picking out trees
        while (!scm_is_null(result))
        {
            for (auto r = scm_cdar(result); !scm_is_null(r); r = scm_cdr(r))
            {
                if (scm_is_true(scm_call_1(scm_is_shape, scm_car(r))))
                {
                    auto tree = static_cast<libfive_tree>(
                        scm_to_pointer(scm_call_1(scm_shape_to_ptr, scm_car(r))));
                    auto shape = new Shape(libfive::Tree(tree), vars);
                    shape->moveToThread(QApplication::instance()->thread());
                    out.shapes.push_back(shape);
                }
            }
            result = scm_cdr(result);
        }

        // Detect variables that should be set in the script but were not,
        // and emit warnings for them (as well as storing reasonable defaults)
        auto bounds = scm_c_eval_string(R"(
        (use-modules (libfive sandbox)) global-bounds
        )");

        auto resolution = scm_c_eval_string(R"(
        (use-modules (libfive sandbox)) global-resolution
        )");

        auto quality = scm_c_eval_string(R"(
        (use-modules (libfive sandbox)) global-quality
        )");

        out.settings = Settings::defaultSettings();
        if (scm_is_false(bounds))
        {
            out.warnings.append(
                    {"<b>Warning:</b> Using default bounds for shapes<br>"
                     "&nbsp;&nbsp;&nbsp;&nbsp;"
                     "Use <code>set-bounds!</code> to specify.",
                    SET_BOUNDS.arg(out.settings.min.x())
                              .arg(out.settings.min.y())
                              .arg(out.settings.min.z())
                              .arg(out.settings.max.x())
                              .arg(out.settings.max.y())
                              .arg(out.settings.max.z())});
        }
        else
        {
            auto lower = bounds;
            out.settings.min = QVector3D(scm_to_double(scm_car(lower)),
                                     scm_to_double(scm_cadr(lower)),
                                     scm_to_double(scm_caddr(lower)));
            auto upper = scm_cdddr(bounds);
            out.settings.max = QVector3D(scm_to_double(scm_car(upper)),
                                     scm_to_double(scm_cadr(upper)),
                                     scm_to_double(scm_caddr(upper)));
        }

        if (scm_is_false(resolution))
        {
            out.warnings.append({
                    "<b>Warning:</b> Using default resolution for shapes.<br>"
                    "&nbsp;&nbsp;&nbsp;&nbsp;"
                    "Use <code>set-resolution!</code> to specify.",
                    SET_RESOLUTION.arg(out.settings.res)});
        }
        else
        {
            out.settings.res = scm_to_double(resolution);
        }

        if (scm_is_false(quality))
        {
            out.warnings.append({
                    "<b>Warning:</b> Using default quality for shapes.<br>"
                    "&nbsp;&nbsp;&nbsp;&nbsp;"
                    "Use <code>set-quality!</code> to specify.",
                    SET_QUALITY.arg(out.settings.quality)});
        }
        else
        {
            out.settings.quality = scm_to_double(quality);
        }
    }

    emit(done(out));
}

}   // namespace Guile
}   // namespace Studio

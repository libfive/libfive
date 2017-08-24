#include <QApplication>

#include "gui/interpreter.hpp"
#include "ao-guile.h"

Interpreter::Interpreter()
    : timer(this)
{
    timer.setSingleShot(true);
    connect(&timer, &QTimer::timeout,
            this, &Interpreter::evalScript,
            Qt::QueuedConnection);
    connect(&thread, &QThread::started,
            this, &Interpreter::init);

    moveToThread(&thread);
}

void Interpreter::start()
{
    thread.start();
}

void Interpreter::init()
{
    // Modify environmental variables to use local Guile path
    auto path = QCoreApplication::applicationDirPath().toLocal8Bit() +
                "/../Resources/guile/";
    qputenv("GUILE_LOAD_COMPILED_PATH", path + "ccache/");
    qputenv("GUILE_LOAD_PATH", path + "scm/");

    scm_init_guile();

    scm_init_ao_kernel_module();
    scm_c_use_module("ao kernel");
    scm_eval_sandboxed = scm_c_eval_string(R"(
(use-modules (ice-9 sandbox) (ao kernel))
(define my-bindings (append (list (cons '(ao kernel) ao-bindings)
    (cons '(ao shapes)
        (module-map (lambda (n . a) n) (resolve-interface '(ao shapes))))
    (cons '(ao csg)
        (module-map (lambda (n . a) n) (resolve-interface '(ao csg))))
    (cons '(ao transforms)
        (module-map (lambda (n . a) n) (resolve-interface '(ao transforms)))))
    all-pure-bindings))

(define (eval-sandboxed str)
  (let ((mod (make-sandbox-module my-bindings))
        (in (open-input-string str))
        (failed #f))
    (let loop ()
      ;; Attempt to read the next clause, storing text location
      (let ((before (cons (port-line in) (port-column in)))
            (clause (catch #t (lambda () (read in))
                              (lambda (key . params)
                                      (set! failed #t)
                                      (list key params))))
            (after (cons (port-line in) (port-column in))))

        (cond
          ;; If we've failed, then record a failed tag
          (failed (list (append (list 'error before after) clause)))

          ;; If we're at the end of the stream, then sever module
          ;; and return a dummy list
          ((eof-object? clause) (eval-in-sandbox #nil #:module mod))

          ;; Otherwise, attempt to evaluate
          (else
            (let ((result
              (catch #t
                ;; Evaluation thunk
                (lambda () (cons 'valid
                  (eval-in-sandbox clause #:module mod #:sever-module? #f
                                          #:time-limit 10
                                          #:allocation-limit #e10e8)))
                ;; Error handling thunk
                (lambda (key . params)
                  (set! failed #t)
                  (list 'error before after key params)))))
              (if (not failed)
                  (cons result (loop))
                  (list result)))))))))
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

    auto kws = scm_to_locale_string(scm_c_eval_string(R"(
(string-drop (string-drop-right
    (format #f "~A" (apply append (map cdr my-bindings))) 1) 1)
)"));
    emit(keywords(kws));
    free(kws);
}

void Interpreter::onScriptChanged(QString s)
{
    script = s;
    timer.start(10);
}

void Interpreter::evalScript()
{
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
    // into a string (with special cases for various erorr forms)
    auto last = scm_is_null(result) ? nullptr
                                    : scm_cdr(scm_car(scm_last_pair(result)));
    if (!valid)
    {
        /* last = '(before after key params) */
        auto before = scm_car(last);
        auto after = scm_cadr(last);
        auto key = scm_caddr(last);
        auto params = scm_cadddr(last);

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
        emit(gotError(QString(str),
                    {scm_to_uint32(scm_car(before)),
                     scm_to_uint32(scm_cdr(before))},
                    {scm_to_uint32(scm_car(after)),
                     scm_to_uint32(scm_cdr(after))}));
        free(str);
    }
    else if (last)
    {
        auto str = scm_to_locale_string(
                scm_simple_format(SCM_BOOL_F, scm_result_fmt,
                                  scm_list_1(last)));
        emit(gotResult(QString(str)));
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
        while (!scm_is_null(result))
        {
            if (scm_is_tree(scm_cdar(result)))
            {
                auto tree = scm_to_tree(scm_cdar(result));
                auto shape = new Shape(*tree);
                shape->moveToThread(QApplication::instance()->thread());
                shapes.push_back(shape);
            }
            result = scm_cdr(result);
        }
        emit(gotShapes(shapes));
    }
}

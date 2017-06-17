#include <QApplication>

#include "gui/interpreter.hpp"
#include "guile.h"
#include "lib.h"

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
(define (eval-sandboxed t) (eval-in-sandbox t #:bindings my-bindings))
eval-sandboxed
)");

    scm_begin = scm_from_utf8_symbol("begin");
    scm_port_eof_p = scm_c_eval_string(R"(
(use-modules (rnrs io ports))
port-eof?
)");
    scm_syntax_error_sym = scm_from_utf8_symbol("syntax-error");
    scm_result_fmt = scm_from_locale_string("~S");
    scm_other_error_fmt = scm_from_locale_string("~A: ~A");
    scm_syntax_error_fmt = scm_from_locale_string("~A: ~A in form ~A");

    // Protect all of our interpreter vars from garbage collection
    for (auto s : {scm_begin, scm_eval_sandboxed, scm_port_eof_p,
                   scm_syntax_error_sym, scm_result_fmt, scm_syntax_error_fmt,
                   scm_other_error_fmt})
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

SCM Interpreter::eval()
{
    valid = true;
    auto str = scm_from_locale_string(script.toLocal8Bit().data());
    auto in = scm_open_input_string(str);

    auto clauses = scm_list_1(scm_begin);
    while (scm_is_false(scm_call_1(scm_port_eof_p, in)))
    {
        clauses = scm_cons(scm_read(in), clauses);
    }
    clauses = scm_reverse(clauses);

    return scm_call_1(scm_eval_sandboxed, clauses);
}

SCM Interpreter::handler(SCM key, SCM args)
{
    valid = false;
    if (scm_is_eq(key, scm_syntax_error_sym))
    {
        return scm_simple_format(SCM_BOOL_F, scm_syntax_error_fmt,
               scm_list_3(key, scm_cadr(args), scm_cadddr(args)));
    }
    else
    {
        return scm_simple_format(SCM_BOOL_F, scm_other_error_fmt,
               scm_list_2(key, scm_simple_format(
                    SCM_BOOL_F, scm_cadr(args), scm_caddr(args))));
    }
}

SCM _eval(void* body)
{
    return ((Interpreter*)body)->eval();
}

SCM _handler(void* data, SCM key, SCM args)
{
    return ((Interpreter*)data)->handler(key, args);
}

void Interpreter::evalScript()
{
    auto result = scm_internal_catch(SCM_BOOL_T, _eval, this, _handler, this);

    auto str = valid ? scm_to_locale_string(
            scm_simple_format(SCM_BOOL_F, scm_result_fmt, scm_list_1(result)))
        : scm_to_locale_string(result);

    emit(resultChanged(valid, QString(str)));

    if (valid && scm_is_tree(result))
    {
        auto tree = scm_to_tree(result);
        auto shape = new Shape(*tree);
        shape->moveToThread(QApplication::instance()->thread());
        emit(gotShape(new Shape(*tree)));
    }
    free(str);
}

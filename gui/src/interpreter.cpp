#include "gui/interpreter.hpp"
#include "guile.h"

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
(define my-bindings (cons (cons '(ao kernel) ao-bindings) all-pure-bindings))
(define (eval-sandboxed t) (eval-in-sandbox t #:bindings my-bindings))
eval-sandboxed
)");

    scm_begin = scm_from_utf8_symbol("begin");
    scm_port_eof_p = scm_c_eval_string(R"(
(use-modules (rnrs io ports))
port-eof?
)");

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
    auto str = scm_from_locale_string(script.toLocal8Bit().data());
    auto in = scm_open_input_string(str);

    auto clauses = scm_list_1(scm_begin);
    while (scm_is_false(scm_call_1(scm_port_eof_p, in)))
    {
        clauses = scm_cons(scm_read(in), clauses);
    }
    clauses = scm_reverse(clauses);

    auto result = scm_call_1(scm_eval_sandboxed, clauses);
    return scm_simple_format(SCM_BOOL_F, scm_from_locale_string("~S"),
                             scm_list_1(result));
}

SCM _eval(void* body)
{
    return ((Interpreter*)body)->eval();
}

static SCM handler(void* data, SCM key, SCM args)
{
    *(bool*)data = false;
    return scm_simple_format(SCM_BOOL_F, scm_from_locale_string("~A: ~A"),
           scm_list_2(key, scm_simple_format(
                SCM_BOOL_F, scm_cadr(args), scm_caddr(args))));
}

void Interpreter::evalScript()
{
    bool success = true;
    auto str = scm_to_locale_string(
            scm_internal_catch(SCM_BOOL_T, _eval, this, handler, &success));
    emit(resultChanged(success, QString(str)));
    free(str);
}

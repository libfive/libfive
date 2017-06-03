#include <QDebug>
#include <libguile.h>
#include "gui/interpreter.hpp"

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
    thread.start();
}

void Interpreter::init()
{
    scm_init_guile();
}

void Interpreter::onScriptChanged(QString s)
{
    script = s;
    timer.start(200);
}

static SCM eval(void* body)
{
    auto str = scm_from_locale_string(((QString*)body)->toLocal8Bit().data());
    auto in = scm_open_input_string(str);
    auto parsed = scm_read(in);

    auto scm_eval_sandboxed = scm_c_eval_string(R"(
(use-modules (ice-9 sandbox))
eval-in-sandbox
)");

    return scm_simple_format(SCM_BOOL_F, scm_from_locale_string("~S"),
            scm_list_1(scm_call_1(scm_eval_sandboxed, parsed)));
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
            scm_internal_catch(SCM_BOOL_T, eval, &script, handler, &success));
    QString out(str);
    free(str);

    qDebug() << success << out;
}

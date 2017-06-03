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

static SCM eval(void* body) {
    QString& input = *(QString*)body;
    auto out = scm_c_eval_string(input.toLocal8Bit().data());
    return scm_simple_format(SCM_BOOL_F, scm_from_locale_string("~A"),
            scm_list_1(out));
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

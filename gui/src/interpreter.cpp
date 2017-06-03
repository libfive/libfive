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

void Interpreter::evalScript()
{
    auto out = scm_c_eval_string(script.toLocal8Bit().data());
    scm_simple_format(SCM_BOOL_T, scm_from_locale_string("~A"), scm_list_1(out));
}

#include <QDebug>
#include <chrono>
#include <thread>

#include "gui/interpreter.hpp"

Interpreter::Interpreter()
    : timer(this)
{
    timer.setSingleShot(true);
    connect(&timer, &QTimer::timeout,
            this, &Interpreter::evalScript,
            Qt::QueuedConnection);

    moveToThread(&thread);
    thread.start();
}

void Interpreter::onScriptChanged(QString s)
{
    script = s;
    timer.start(200);
}

void Interpreter::evalScript()
{
    qDebug() << "Evaluating script" << script;
}

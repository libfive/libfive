#include <QDebug>
#include "gui/interpreter.hpp"

Interpreter::Interpreter()
{
    // Nothing to do here
}

void Interpreter::onScriptChanged(QString s)
{
    qDebug() << "Got script " << s;
}

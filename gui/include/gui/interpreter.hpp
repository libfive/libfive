#pragma once

#include <QObject>

class Interpreter : QObject
{
    Q_OBJECT
public:
    Interpreter();
public slots:
    void onScriptChanged(QString s);
};

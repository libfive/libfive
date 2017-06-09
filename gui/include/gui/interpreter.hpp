#pragma once

#include <QObject>
#include <QThread>
#include <QTimer>
#include <libguile.h>

#include "gui/shape.hpp"

class Interpreter : public QObject
{
    Q_OBJECT
public:
    Interpreter();
    void start();

public slots:
    void onScriptChanged(QString s);

signals:
    void resultChanged(bool valid, QString result);
    void keywords(QString kws);
    void gotShape(Shape* s);

protected slots:
    void evalScript();
    void init();

protected:
    SCM eval();
    SCM handler(SCM key, SCM args);

    QString script;
    bool valid;
    QTimer timer;

    QThread thread;

    SCM scm_begin;
    SCM scm_eval_sandboxed;
    SCM scm_port_eof_p;
    SCM scm_syntax_error;

    friend SCM _eval(void* body);
    friend SCM _handler(void* body, SCM key, SCM args);
};

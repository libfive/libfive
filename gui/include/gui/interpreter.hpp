#pragma once

#include <QObject>
#include <QThread>
#include <QTimer>
#include <libguile.h>

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

protected slots:
    void evalScript();
    void init();

protected:
    SCM eval();

    QString script;
    QTimer timer;

    QThread thread;

    SCM scm_begin;
    SCM scm_eval_sandboxed;
    SCM scm_port_eof_p;

    friend SCM _eval(void* body);
};

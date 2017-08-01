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
    void gotShapes(QList<Shape*> s);

protected slots:
    void evalScript();
    void init();

protected:
    QString script;
    QTimer timer;

    QThread thread;

    SCM scm_eval_sandboxed;
    SCM scm_port_eof_p;
    SCM scm_valid_sym;
    SCM scm_syntax_error_sym;
    SCM scm_syntax_error_fmt;
    SCM scm_other_error_fmt;
    SCM scm_result_fmt;
};

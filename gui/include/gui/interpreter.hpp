#pragma once

#include <QObject>
#include <QThread>
#include <QTimer>

class Interpreter : public QObject
{
    Q_OBJECT
public:
    Interpreter();

public slots:
    void onScriptChanged(QString s);

signals:
    void resultChanged(bool valid, QString result);

protected slots:
    void evalScript();
    void init();

protected:
    QString script;
    QTimer timer;

    QThread thread;
};

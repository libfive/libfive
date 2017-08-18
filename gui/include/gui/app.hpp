#pragma once
#include <QApplication>

class App : public QApplication
{
    Q_OBJECT
public:
    App(int argc, char** argv);
protected:
    bool event(QEvent* event) override;
};

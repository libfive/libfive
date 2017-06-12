#pragma once

#include <QObject>
#include <QPointer>
#include <QVector3D>
#include <QWidget>

struct Settings
{
    Settings(QVector3D min, QVector3D max, float res)
        : min(min), max(max), res(res) {}
    Settings()
        : res(-1) {}

    QVector3D min;
    QVector3D max;
    float res;
};

class SettingsPane : public QWidget
{
    Q_OBJECT
public:
    SettingsPane(Settings s);
signals:
    void changed(Settings s);
};

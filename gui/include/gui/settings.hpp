#pragma once

#include <QObject>
#include <QPointer>
#include <QVector3D>
#include <QWidget>

struct Settings
{
    Settings(QVector3D min, QVector3D max, float res);
    Settings();

    Settings next() const;

    QVector3D min;
    QVector3D max;
    float res;
    int div;
protected:
    Settings(QVector3D min, QVector3D max, float res, int div);
};

class SettingsPane : public QWidget
{
    Q_OBJECT
public:
    SettingsPane(Settings s);
signals:
    void changed(Settings s);
};

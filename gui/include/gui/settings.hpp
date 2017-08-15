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

    // Used to read and write to scripts
    static QRegularExpression bounds_regex;
    static QRegularExpression res_regex;
    static QString bounds_fmt;
    static QString res_fmt;

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

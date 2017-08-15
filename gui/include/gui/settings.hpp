#pragma once

#include <QObject>
#include <QPointer>
#include <QVector3D>
#include <QWidget>
#include <QDoubleSpinBox>

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
    static QRegularExpression settings_regex;
    static QString settings_fmt;

protected:
    Settings(QVector3D min, QVector3D max, float res, int div);
};

////////////////////////////////////////////////////////////////////////////////

class SettingsPane : public QWidget
{
    Q_OBJECT
public:
    SettingsPane(Settings s);
    void set(Settings s);

signals:
    void changed(Settings s);

protected:
    QDoubleSpinBox* xmin;
    QDoubleSpinBox* xmax;
    QDoubleSpinBox* ymin;
    QDoubleSpinBox* ymax;
    QDoubleSpinBox* zmin;
    QDoubleSpinBox* zmax;
    QDoubleSpinBox* res;
};

#pragma once

#include <QObject>
#include <QPointer>
#include <QVector3D>
#include <QWidget>
#include <QDoubleSpinBox>

struct Settings
{
    /*
     *  Useful constructor
     */
    Settings(QVector3D min, QVector3D max, float res, float quality);

    /*
     *  Default constructor (produces an object that shouldn't be used)
     */
    Settings() : res(-1), quality(-1) { /* Nothing to do here */ }

    /*
     *  Estimates a reasonable resolution scale for incremental rendering
     */
    int defaultDiv() const;

    QVector3D min;
    QVector3D max;
    float res;
    float quality;

    // Used to read and write to scripts
    static QRegularExpression settings_regex;
    static QString settings_fmt;

    bool operator==(const Settings& other) const;
    bool operator!=(const Settings& other) const;

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
    QDoubleSpinBox* quality;
};

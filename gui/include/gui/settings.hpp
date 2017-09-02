#pragma once

#include <QObject>
#include <QPointer>
#include <QVector3D>
#include <QWidget>
#include <QDoubleSpinBox>

struct Settings
{
    /*
     *  Default constructor estimates initial div using heuristic
     */
    Settings(QVector3D min, QVector3D max, float res, float quality);

    /*
     *  The empty Settings object has div = -1
     */
    Settings();

    /*
     *  Construct a new Setting object with a different division
     */
    Settings(const Settings& other, int div);

    /*
     *  Returns an object with the same bounds, res, and quality
     *  but a smaller div.  This must only be called with div >= 0
     */
    Settings next() const;

    /*
     *  Returns the original (higher-div) Settings object
     */
    Settings base() const;

    QVector3D min;
    QVector3D max;
    float res;
    float quality;
    int div;

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

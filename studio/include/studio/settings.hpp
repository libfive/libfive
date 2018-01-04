/*
Studio: a simple GUI for the libfive CAD kernel
Copyright (C) 2017  Matt Keeter

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/
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

    void disable();
    void enable();

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

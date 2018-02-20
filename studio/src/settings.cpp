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
#include <cassert>
#include <cmath>

#include <QGridLayout>
#include <QLabel>

#include "studio/settings.hpp"

QRegularExpression Settings::settings_regex(
        "#! RENDER (.*) (.*) (.*) / (.*) (.*) (.*) / (.*) / (.*) / ([01]) !#");
QString Settings::settings_fmt(
        "#! RENDER %1 %2 %3 / %4 %5 %6 / %7 / %8 / %9 !#");

Settings::Settings(QVector3D min, QVector3D max, float res,
                   float quality, bool autobounds)
    : min(min), max(max), res(res), quality(quality), autobounds(autobounds)
{
    // Nothing to do here
}

Settings Settings::defaultSettings()
{
    return Settings({-10, -10, -10}, {10, 10, 10}, 10, 8, false);
}

int Settings::defaultDiv() const
{
    const float volume = (max.x() - min.x()) *
                         (max.y() - min.y()) *
                         (max.z() - min.z());

    // Magic numbers from Antimony's mesh export dialog
    const float target_res = pow((1 << 22) / volume, 0.33) / 2.52;

    // More magic numbers
    return (target_res > res) ? 0
        : int(log(res / target_res) / log(2) + 0.5);
}

QString Settings::toString() const
{
    return settings_fmt
        .arg(min.x()).arg(min.y()).arg(min.z())
        .arg(max.x()).arg(max.y()).arg(max.z())
        .arg(res).arg(quality).arg(autobounds ? 1 : 0);
}

Settings Settings::fromString(QString txt, bool* okay)
{
    auto match = Settings::settings_regex.match(txt);
    *okay = match.hasMatch();

    const QVector3D min(match.captured(1).toFloat(okay),
                        match.captured(2).toFloat(okay),
                        match.captured(3).toFloat(okay));
    const QVector3D max(match.captured(4).toFloat(okay),
                        match.captured(5).toFloat(okay),
                        match.captured(6).toFloat(okay));

    const float res = match.captured(7).toFloat(okay);
    const float quality = match.captured(8).toFloat(okay);
    const float autobounds = (match.captured(9) == "1");

    if (*okay)
    {
        return Settings(min, max, res, quality, autobounds);
    }
    else
    {
        return Settings();
    }
}

bool Settings::operator==(const Settings& other) const
{
    return min == other.min && max == other.max &&
           res == other.res && quality == other.quality &&
           autobounds == other.autobounds;
}

bool Settings::operator!=(const Settings& other) const
{
    return !(*this == other);
}

////////////////////////////////////////////////////////////////////////////////

SettingsPane::SettingsPane(Settings s)
    : xmin(new QDoubleSpinBox),
      xmax(new QDoubleSpinBox),
      ymin(new QDoubleSpinBox),
      ymax(new QDoubleSpinBox),
      zmin(new QDoubleSpinBox),
      zmax(new QDoubleSpinBox),
      res(new QDoubleSpinBox),
      quality(new QDoubleSpinBox),
      autobounds(new QCheckBox)
{
    auto layout = new QGridLayout();

    layout->addWidget(new QLabel("<b>Bounds:</b>"), 0, 1);
    layout->addWidget(new QLabel("X:"), 1, 0);
    layout->addWidget(xmin, 1, 1);
    layout->addWidget(xmax, 1, 2);
    layout->addWidget(new QLabel("Y:"), 2, 0);
    layout->addWidget(ymin, 2, 1);
    layout->addWidget(ymax, 2, 2);
    layout->addWidget(new QLabel("Z:"), 3, 0);
    layout->addWidget(zmin, 3, 1);
    layout->addWidget(zmax, 3, 2);

    layout->addWidget(new QLabel("Auto-bounding:"), 4, 0, 1, 2);
    layout->addWidget(autobounds, 4, 2);

    QFrame* line;
    line = new QFrame();
    line->setFrameShape(QFrame::HLine);
    line->setFrameShadow(QFrame::Raised);
    layout->addWidget(line, 5, 0, 1, 3);

    layout->addWidget(new QLabel("Resolution:"), 6, 0, 1, 2, Qt::AlignCenter);
    layout->addWidget(res, 6, 2);

    layout->addWidget(new QLabel("Quality:"), 7, 0, 1, 2, Qt::AlignCenter);
    layout->addWidget(quality, 7, 2);

    layout->setMargin(10);
    layout->setSpacing(4);

    for (auto t : {xmin, xmax, ymin, ymax, zmin, zmax})
    {
        t->setMinimum(-100);
        t->setMaximum(100);
    }
    res->setMaximum(100);
    quality->setMaximum(11);

    set(s);

    for (auto t : {xmin, xmax, ymin, ymax, zmin, zmax, res, quality})
    {
        connect(t, static_cast<void (QDoubleSpinBox::*)(double)>(&QDoubleSpinBox::valueChanged),
                this, [=](double){ emit(this->changed(settings())); });
        connect(this, &SettingsPane::enable,
                t, [=](){ t->setReadOnly(false); });
        connect(this, &SettingsPane::disable,
                t, [=](){ t->setReadOnly(true); });
    }

    // Set up similar connections for the autobounds checkbox
    connect(autobounds, &QCheckBox::clicked,
            this, [=](bool){ emit(this->changed(settings())); });
    connect(this, &SettingsPane::enable,
            autobounds, [=](){ autobounds->setEnabled(true); });
    connect(this, &SettingsPane::disable,
            autobounds, [=](){ autobounds->setEnabled(false); });

    setLayout(layout);
    setWindowFlags(Qt::Tool | Qt::CustomizeWindowHint |
                   Qt::WindowTitleHint | Qt::WindowCloseButtonHint);
    setAttribute(Qt::WA_DeleteOnClose);
}

void SettingsPane::set(Settings s)
{
    xmin->setValue(s.min.x());
    ymin->setValue(s.min.y());
    zmin->setValue(s.min.z());
    xmax->setValue(s.max.x());
    ymax->setValue(s.max.y());
    zmax->setValue(s.max.z());
    res->setValue(s.res);
    quality->setValue(s.quality);
    autobounds->setChecked(s.autobounds);
}

Settings SettingsPane::settings() const
{
    return Settings(QVector3D(xmin->value(), ymin->value(), zmin->value()),
                    QVector3D(xmax->value(), ymax->value(), zmax->value()),
                    res->value(), quality->value(), autobounds->isChecked());
}

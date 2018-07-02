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

Settings::Settings(QVector3D min, QVector3D max, float res, float quality)
    : min(min), max(max), res(res), quality(quality)
{
    // Nothing to do here
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

Settings Settings::defaultSettings()
{
    return Settings({-10, -10, -10}, {10, 10, 10}, 10, 8);
}

bool Settings::operator==(const Settings& other) const
{
    return min == other.min && max == other.max &&
           res == other.res && quality == other.quality;
}

bool Settings::operator!=(const Settings& other) const
{
    return !(*this == other);
}

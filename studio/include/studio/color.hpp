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

#include <QColor>

namespace Studio {
namespace Color
{
extern const QColor base03;
extern const QColor base02;
extern const QColor base01;
extern const QColor base00;
extern const QColor base0;
extern const QColor base1;
extern const QColor base2;
extern const QColor base3;

extern const QColor yellow;
extern const QColor orange;
extern const QColor red;
extern const QColor magenta;
extern const QColor violet;
extern const QColor blue;
extern const QColor cyan;
extern const QColor green;
}
} // namespace Studio

#define COLOR(c)     Color::c.red()/255.0f, Color::c.green()/255.0f, Color::c.blue()/255.0f

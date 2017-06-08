#pragma once

#include <QColor>

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

#define COLOR(c)     Color::c.red()/255.0f, Color::c.green()/255.0f, Color::c.blue()/255.0f

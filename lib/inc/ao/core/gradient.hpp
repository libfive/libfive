#pragma once

struct Gradient
{
    Gradient()
        : Gradient(0) {}
    Gradient(double v)
        : v(v), dx(0), dy(0), dz(0) {}
    double v, dx, dy, dz;
};

#pragma once

struct Gradient
{
    Gradient(double v)
        : v(v), dx(0), dy(0), dz(0) {}
    double v, dx, dy, dz;
};

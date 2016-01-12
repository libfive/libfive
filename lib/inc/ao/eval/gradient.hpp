#pragma once

struct Gradient
{
    Gradient()
        : Gradient(0) {}
    Gradient(float v)
        : v(v), dx(0), dy(0), dz(0) {}
    Gradient(float v, float dx, float dy, float dz)
        : v(v), dx(dx), dy(dy), dz(dz) {}

    float v, dx, dy, dz;
};

Gradient operator+(const Gradient& a, const Gradient& b);
Gradient operator-(const Gradient& a, const Gradient& b);
Gradient operator*(const Gradient& a, const Gradient& b);
Gradient operator/(const Gradient& a, const Gradient& b);
Gradient _min(const Gradient& a, const Gradient& b);
Gradient _max(const Gradient& a, const Gradient& b);
Gradient pow(const Gradient& a, const Gradient& b);
Gradient sqrt(const Gradient& a);
Gradient _abs(const Gradient& a);
Gradient operator-(const Gradient& a);

#pragma once

struct Gradient
{
    Gradient()
        : Gradient(0) {}
    Gradient(double v)
        : v(v), dx(0), dy(0), dz(0) {}
    Gradient(double v, double dx, double dy, double dz)
        : v(v), dx(dx), dy(dy), dz(dz) {}

    double v, dx, dy, dz;
};

Gradient operator+(const Gradient& a, const Gradient& b);
Gradient operator-(const Gradient& a, const Gradient& b);
Gradient operator*(const Gradient& a, const Gradient& b);
Gradient operator/(const Gradient& a, const Gradient& b);
Gradient _min(const Gradient& a, const Gradient& b);
Gradient _max(const Gradient& a, const Gradient& b);
Gradient pow(const Gradient& a, const Gradient& b);
Gradient sqrt(const Gradient& a);
Gradient operator-(const Gradient& a);

bool operator<(const Gradient& a, const int& b);

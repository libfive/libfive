#pragma once

#include <glm/vec3.hpp>

struct Gradient
{
    Gradient()
        : Gradient(0) {}
    Gradient(float v)
        : v(v), d(0, 0, 0) {}
    Gradient(float v, const glm::vec3& d)
        : v(v), d(d) {}

    float v;
    glm::vec3 d;
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

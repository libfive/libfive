#pragma once

#include <string>

#include "ao/gl/core.hpp"

class Shaders
{
public:
    explicit Shaders();

    /*
     *  Activate the default program
     */
    void Use() const;

protected:
    GLuint prog;
};

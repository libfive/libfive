#pragma once

#include <string>

#include <GLFW/glfw3.h>

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

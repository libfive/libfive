#pragma once

#include <string>

#include <GLFW/glfw3.h>

class Shaders
{
public:
    Shaders();

    /*
     *  Activate the default program
     */
    void Use() const;

protected:
    GLuint prog;
};

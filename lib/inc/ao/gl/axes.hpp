#pragma once

#include <glm/mat4x4.hpp>

#include "ao/gl/core.hpp"

class Axes
{
public:
    Axes();
    ~Axes();

    void draw(const glm::mat4& m) const;

protected:
    GLuint vs;  // Vertex shader
    GLuint fs;  // Fragment shader
    GLuint prog;    // Shader program

    GLuint vbo; // Vertex buffer
    GLuint vao; // Vertex array

    // Shader source strings
    static const std::string vert;
    static const std::string frag;
};

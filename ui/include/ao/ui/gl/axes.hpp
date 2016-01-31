#pragma once

#include <glm/mat4x4.hpp>

#include "ao/ui/gl/core.hpp"

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

    GLuint vbo_solid; // Vertex buffer for solid axes
    GLuint vao_solid; // Vertex array for drawing shaded

    GLuint vbo_wire;  // Vertex buffer for wireframe axes
    GLuint vao_wire;  // Vertex array for drawing wireframe

    // Shader source strings
    static const std::string vert;
    static const std::string frag;
};

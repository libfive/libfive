#include "ao/gl/frame.hpp"

////////////////////////////////////////////////////////////////////////////////

std::string base_vert = R"EOF(
#version 330

layout(location=0) in vec3 vertex_position;

void main()
{
    gl_Position = vec4(vertex_position, 1.0f);
}
)EOF";

////////////////////////////////////////////////////////////////////////////////

std::string base_frag = R"EOF(
#version 330

out vec4 fragColor;

void main()
{
    fragColor = vec4(1.0f, 0.0f, 0.0f, 1.0f);
}
)EOF";

////////////////////////////////////////////////////////////////////////////////

Frame::Frame()
{
    glGenBuffers(1, &vbo);
    glGenVertexArrays(1, &vao);

    glBindVertexArray(vao);
    {
        GLfloat vertices[] = {0.0f, 0.0f, 0.0f,
                              1.0f, 0.0f, 0.0f,
                              1.0f, 1.0f, 0.0f,
                              0.0f, 1.0f, 0.0f};
        glBindBuffer(GL_ARRAY_BUFFER, vbo);
        glBufferData(GL_ARRAY_BUFFER, sizeof(vertices),
                     vertices, GL_STATIC_DRAW);
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE,
                              3 * sizeof(GLfloat), (GLvoid*)0);
        glEnableVertexAttribArray(0);
    }
    glBindVertexArray(0);
}

Frame::~Frame()
{
    glDeleteBuffers(1, &depth);
    glDeleteBuffers(1, &normal);

    glDeleteBuffers(1, &vbo);
    glDeleteVertexArrays(1, &vao);
}

void Frame::Draw()
{
    glBindVertexArray(vao);
    glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
    glBindVertexArray(0);
}

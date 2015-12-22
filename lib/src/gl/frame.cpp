#include "ao/gl/frame.hpp"

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

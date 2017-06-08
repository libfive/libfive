#include "gui/background.hpp"
#include "gui/color.hpp"

Background::Background()
{
    // Nothing to do here
}

void Background::initializeGL()
{
    initializeOpenGLFunctions();

    shader.addShaderFromSourceFile(
            QOpenGLShader::Vertex, ":/gl/basic.vert");
    shader.addShaderFromSourceFile(
            QOpenGLShader::Fragment, ":/gl/flat.frag");
    shader.link();

    {
#define COLOR(c)     Color::c.red()/255.0f, Color::c.green()/255.0f, Color::c.blue()/255.0f
        // Data is arranged  x   y   z   r   g   b
        GLfloat data[] = {   /********************/
                             /*    X axis        */
                            -1, -1,  0.9,  COLOR(base03),
                            -1,  1,  0.9,  COLOR(base02),
                             1, -1,  0.9,  COLOR(base02),
                             1,  1,  0.9,  COLOR(base02),
        };
        vbo.create();
        vbo.bind();
        vbo.allocate(data, sizeof(data));

        vao.create();
        vao.bind();

        // Data stored in VAO
        glVertexAttribPointer(
                0, 3, GL_FLOAT, GL_FALSE,
                6 * sizeof(GLfloat), (GLvoid*)0);
        glVertexAttribPointer(
                1, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(GLfloat),
                (GLvoid*)(3 * sizeof(GLfloat)));
        glEnableVertexAttribArray(0);
        glEnableVertexAttribArray(1);

        vbo.release();
        vao.release();
    }
}

void Background::draw()
{
    shader.bind();
    QMatrix4x4 M;
    glUniformMatrix4fv(shader.uniformLocation("M"), 1, GL_FALSE, M.data());

    vao.bind();
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
    vao.release();

    shader.release();
}

#include "gui/axes.hpp"
#include "gui/color.hpp"

Axes::Axes()
{
    // Nothing to do here
}

void Axes::initializeGL()
{
    initializeOpenGLFunctions();

    shader.addShaderFromSourceFile(
            QOpenGLShader::Vertex, ":/gl/basic.vert");
    shader.addShaderFromSourceFile(
            QOpenGLShader::Fragment, ":/gl/flat.frag");
    shader.link();

    {   // In-line data for axes model
        const float o = 0.05;

#define COLOR(c)     Color::c.red()/255.0f, Color::c.green()/255.0f, Color::c.blue()/255.0f
        // Data is arranged  x   y   z   r   g   b
        GLfloat data[] = {   /********************/
                             /*    X axis        */
                             o,  o,  o,  COLOR(red),
                             o, -o,  o,  COLOR(red),
                             1,  0,  0,  COLOR(red),

                             o,  o, -o,  COLOR(red),
                             o, -o, -o,  COLOR(red),
                             1,  0,  0,  COLOR(red),

                             o,  o,  o,  COLOR(red),
                             o,  o, -o,  COLOR(red),
                             1,  0,  0,  COLOR(red),

                             o, -o,  o,  COLOR(red),
                             o, -o, -o,  COLOR(red),
                             1,  0,  0,  COLOR(red),
                             /********************/
                             /*    Y axis        */
                             o,  o,  o,  COLOR(green),
                            -o,  o,  o,  COLOR(green),
                             0,  1,  0,  COLOR(green),

                             o,  o, -o,  COLOR(green),
                            -o,  o, -o,  COLOR(green),
                             0,  1,  0,  COLOR(green),

                             o,  o,  o,  COLOR(green),
                             o,  o, -o,  COLOR(green),
                             0,  1,  0,  COLOR(green),

                            -o,  o,  o,  COLOR(green),
                            -o,  o, -o,  COLOR(green),
                             0,  1,  0,  COLOR(green),
                             /********************/
                             /*    Z axis        */
                             o,  o,  o,  COLOR(blue),
                            -o,  o,  o,  COLOR(blue),
                             0,  0,  1,  COLOR(blue),

                             o, -o,  o,  COLOR(blue),
                            -o, -o,  o,  COLOR(blue),
                             0,  0,  1,  COLOR(blue),

                             o,  o,  o,  COLOR(blue),
                             o, -o,  o,  COLOR(blue),
                             0,  0,  1,  COLOR(blue),

                            -o,  o,  o,  COLOR(blue),
                            -o, -o,  o,  COLOR(blue),
                             0,  0,  1,  COLOR(blue),
                             /********************/
                             /*    Base cube     */
                             o,  o, -o,  1,  1,  1,
                             o, -o, -o,  1,  1,  1,
                            -o,  o, -o,  1,  1,  1,

                            -o, -o,  o,  1,  1,  1,
                            -o,  o,  o,  1,  1,  1,
                            -o,  o, -o,  1,  1,  1,

                            -o, -o,  o,  1,  1,  1,
                             o, -o, -o,  1,  1,  1,
                             o, -o,  o,  1,  1,  1,

                            -o, -o, -o,  1,  1,  1,
                             o, -o, -o,  1,  1,  1,
                            -o,  o, -o,  1,  1,  1,

                            -o, -o,  o,  1,  1,  1,
                            -o, -o, -o,  1,  1,  1,
                            -o,  o, -o,  1,  1,  1,

                            -o, -o,  o,  1,  1,  1,
                             o, -o, -o,  1,  1,  1,
                            -o, -o, -o,  1,  1,  1,
        };
        solid_vbo.create();
        solid_vbo.bind();
        solid_vbo.allocate(data, sizeof(data));

        solid_vao.create();
        solid_vao.bind();

        // Data stored in VAO
        glVertexAttribPointer(
                0, 3, GL_FLOAT, GL_FALSE,
                6 * sizeof(GLfloat), (GLvoid*)0);
        glVertexAttribPointer(
                1, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(GLfloat),
                (GLvoid*)(3 * sizeof(GLfloat)));
        glEnableVertexAttribArray(0);
        glEnableVertexAttribArray(1);

        solid_vbo.release();
        solid_vao.release();
    }

    {   // In-line data for wireframe model
        // Data is arranged  x   y   z   r   g   b
        GLfloat data[] = {   0,  0,  0,  COLOR(red),
                             1,  0,  0,  COLOR(red),
                             0,  0,  0,  COLOR(green),
                             0,  1,  0,  COLOR(green),
                             0,  0,  0,  COLOR(blue),
                             0,  0,  1,  COLOR(blue),
        };
        wire_vbo.create();
        wire_vbo.bind();
        wire_vbo.allocate(data, sizeof(data));

        wire_vao.create();
        wire_vao.bind();

        // Data stored in VAO
        glVertexAttribPointer(
                0, 3, GL_FLOAT, GL_FALSE,
                6 * sizeof(GLfloat), (GLvoid*)0);
        glVertexAttribPointer(
                1, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(GLfloat),
                (GLvoid*)(3 * sizeof(GLfloat)));
        glEnableVertexAttribArray(0);
        glEnableVertexAttribArray(1);

        wire_vbo.release();
        wire_vao.release();
    }
}

void Axes::drawSolid(QMatrix4x4 M)
{
    shader.bind();
    glUniformMatrix4fv(shader.uniformLocation("M"), 1, GL_FALSE, M.data());

    solid_vao.bind();
    glDrawArrays(GL_TRIANGLES, 0, 54);
    solid_vao.release();

    shader.release();
}

void Axes::drawWire(QMatrix4x4 M)
{
    shader.bind();
    glUniformMatrix4fv(shader.uniformLocation("M"), 1, GL_FALSE, M.data());

    glDisable(GL_DEPTH_TEST);
    wire_vao.bind();
    glDrawArrays(GL_LINES, 0, 6);
    wire_vao.release();
    glEnable(GL_DEPTH_TEST);

    shader.release();
}

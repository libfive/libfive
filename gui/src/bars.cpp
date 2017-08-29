#include <QPropertyAnimation>
#include <QTime>

#include "gui/bars.hpp"
#include "gui/shader.hpp"

Bars::Bars()
{
    // Nothing to do here
}

void Bars::initializeGL()
{
    initializeOpenGLFunctions();

    // Data is arranged  x   y
    GLfloat data[] = {  -1, -1,
                        -1,  1,
                         1, -1,
                         1,  1,
    };
    vbo.create();
    vbo.bind();
    vbo.allocate(data, sizeof(data));

    vao.create();
    vao.bind();

    // Data stored in VAO
    glVertexAttribPointer(
            0, 2, GL_FLOAT, GL_FALSE,
            2 * sizeof(GLfloat), (GLvoid*)0);
    glEnableVertexAttribArray(0);

    vbo.release();
    vao.release();
}

bool Bars::hover(bool over)
{
    bool changed = (over != hovered);
    hovered = over;
    return changed;
}

void Bars::draw(const QSize& size)
{
    Shader::bars->bind();

    QMatrix4x4 M;
    M.translate({1 - side / float(size.width()),
                 1 - side / float(size.height()), 0});
    if (size.width() > size.height())
    {
        M.scale(size.height() / float(size.width()), 1, 1);
        M.scale(side / float(size.height()));
    }
    else
    {
        M.scale(1, size.width() / float(size.height()), 1);
        M.scale(side / float(size.width()));
    }

    glUniformMatrix4fv(Shader::bars->uniformLocation("M"),
                1, GL_FALSE, M.data());
    glUniform1f(Shader::bars->uniformLocation("fade"), hovered ? 0.8 : 0.4);

    vao.bind();
    glDisable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
    glEnable(GL_DEPTH_TEST);
    glDisable(GL_BLEND);
    vao.release();

    Shader::bars->release();
}

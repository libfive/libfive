#include <QPropertyAnimation>
#include <QTime>

#include "gui/busy.hpp"
#include "gui/shader.hpp"

Busy::Busy()
{
    connect(&timer, &QTimer::timeout, this, &Busy::redraw);
}

void Busy::initializeGL()
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

void Busy::show()
{
    if (!timer.isActive())
    {
        timer.start(1000/60);
        auto anim = new QPropertyAnimation(this, "fade");
        anim->setDuration(100);
        anim->setStartValue(0.0);
        anim->setEndValue(1.0);
        anim->start(QPropertyAnimation::DeleteWhenStopped);
    }
}

void Busy::hide()
{
    auto anim = new QPropertyAnimation(this, "fade");
    anim->setDuration(100);
    anim->setStartValue(fade);
    anim->setEndValue(0.0);

    connect(anim, &QPropertyAnimation::finished, &timer, &QTimer::stop);
    connect(anim, &QPropertyAnimation::finished, this, &Busy::redraw);
    anim->start(QPropertyAnimation::DeleteWhenStopped);
}

void Busy::draw(const QSize& size)
{
    if (timer.isActive())
    {
        Shader::busy->bind();

        QMatrix4x4 M;
        const float side = 50;
        M.translate({1 - side / float(size.width()),
                    -1 + side / float(size.height()), 0});
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

        glUniform1f(Shader::busy->uniformLocation("time"),
                    QTime::currentTime().msec() / 1000.0f);
        glUniform1f(Shader::busy->uniformLocation("fade"), fade);
        glUniformMatrix4fv(Shader::busy->uniformLocation("M"),
                    1, GL_FALSE, M.data());

        vao.bind();
        glDisable(GL_DEPTH_TEST);
        glEnable(GL_BLEND);
        glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
        glEnable(GL_DEPTH_TEST);
        glDisable(GL_BLEND);
        vao.release();

        Shader::busy->release();
    }
}

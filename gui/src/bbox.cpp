#include <cmath>

#include "gui/bbox.hpp"
#include "gui/camera.hpp"
#include "gui/shader.hpp"

void BBox::initializeGL()
{
    initializeOpenGLFunctions();

    QVector<GLfloat> data;
    auto push = [&](GLfloat x, GLfloat y){
        data.push_back(x);
        data.push_back(y);
    };

    // Build a capsule shape out of triangles
    const int res = 32;
    for (unsigned i=0; i < 32; ++i)
    {
        push(cos(M_PI * i / (res - 1.0)), -sin(M_PI * i / (res - 1.0)));
        push(cos(M_PI * (i + 1) / (res - 1.0)), -sin(M_PI * (i + 1) / (res - 1.0)));
        push(0, 0);
    }

    push(1, 0);
    push(1, 1);
    push(-1, 0);

    push(-1, 0);
    push(1, 1);
    push(-1, 1);

    for (unsigned i=0; i < 32; ++i)
    {
        push(cos(M_PI * i / (res - 1.0)), 1 + sin(M_PI * i / (res - 1.0)));
        push(cos(M_PI * (i + 1) / (res - 1.0)), 1 + sin(M_PI * (i + 1) / (res - 1.0)));
        push(0, 1);
    }

    vbo.create();
    vbo.bind();
    vbo.allocate(data.data(), data.size() * sizeof(GLfloat));

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

void BBox::draw(const QVector3D& min, const QVector3D& max,
                const Camera& camera)
{
    auto M = camera.M();

    Shader::line->bind();
    glUniform1f(Shader::line->uniformLocation("thickness"), 0.01);
    glUniform1f(Shader::line->uniformLocation("aspect"), camera.getAspect());

    vao.bind();

    QList<QPair<QVector3D, QVector3D>> edges = {
        {min, {min.x(), min.y(), max.z()}},
        {min, {min.x(), max.y(), min.z()}},
        {min, {max.x(), min.y(), min.z()}},

        {max, {max.x(), max.y(), min.z()}},
        {max, {max.x(), min.y(), max.z()}},
        {max, {min.x(), max.y(), max.z()}},

        {{min.x(), min.y(), max.z()}, {min.x(), max.y(), max.z()}},
        {{min.x(), min.y(), max.z()}, {max.x(), min.y(), max.z()}},

        {{max.x(), max.y(), min.z()}, {min.x(), max.y(), min.z()}},
        {{max.x(), max.y(), min.z()}, {max.x(), min.y(), min.z()}},

        {{max.x(), min.y(), min.z()}, {max.x(), min.y(), max.z()}},
        {{min.x(), max.y(), min.z()}, {min.x(), max.y(), max.z()}},
    };

    auto a_loc = Shader::line->uniformLocation("a");
    auto b_loc = Shader::line->uniformLocation("b");
    for (auto& e : edges)
    {
        auto a = M * e.first;
        auto b = M * e.second;

        glUniform3f(a_loc, a.x(), a.y(), a.z());
        glUniform3f(b_loc, b.x(), b.y(), b.z());

        glDrawArrays(GL_TRIANGLES, 0, 198);
    }
    vao.release();

    Shader::flat->release();
}


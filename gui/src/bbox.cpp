#include "gui/bbox.hpp"
#include "gui/shader.hpp"

void BBox::initializeGL()
{
    initializeOpenGLFunctions();

    {
        // Data is arranged  x   y   z   i
        GLfloat data[] = {   /********************/
                            0, 0, 0, 1, // XY
                            1, 0, 0, 0,
                            1, 1, 0, 2,

                            0, 0, 0, 1,
                            1, 1, 0, 2,
                            0, 1, 0, 0,

                            0, 0, 1, 1,
                            1, 1, 1, 2,
                            1, 0, 1, 0,

                            0, 0, 1, 1,
                            0, 1, 1, 0,
                            1, 1, 1, 2,

                            0, 0, 0, 1, // XZ
                            1, 0, 0, 0,
                            1, 0, 1, 2,

                            0, 0, 0, 1,
                            1, 0, 1, 2,
                            0, 0, 1, 0,

                            0, 1, 0, 1,
                            1, 1, 1, 2,
                            1, 1, 0, 0,

                            0, 1, 0, 1,
                            0, 1, 1, 0,
                            1, 1, 1, 2,

                            0, 0, 0, 1, // YZ
                            0, 1, 0, 0,
                            0, 1, 1, 2,

                            0, 0, 0, 1,
                            0, 1, 1, 2,
                            0, 0, 1, 0,

                            1, 0, 0, 1,
                            1, 1, 1, 2,
                            1, 1, 0, 0,

                            1, 0, 0, 1,
                            1, 0, 1, 0,
                            1, 1, 1, 2,
        };
        vbo.create();
        vbo.bind();
        vbo.allocate(data, sizeof(data));

        vao.create();
        vao.bind();

        // Data stored in VAO
        glVertexAttribPointer(
                0, 3, GL_FLOAT, GL_FALSE,
                4 * sizeof(GLfloat), (GLvoid*)0);
        glVertexAttribPointer(
                1, 1, GL_FLOAT, GL_FALSE, 4 * sizeof(GLfloat),
                (GLvoid*)(3 * sizeof(GLfloat)));
        glEnableVertexAttribArray(0);
        glEnableVertexAttribArray(1);

        vbo.release();
        vao.release();
    }
}

void BBox::draw(const QVector3D& min, const QVector3D& max,
                const QMatrix4x4& M, float scale)
{
    Shader::bbox->bind();
    glUniformMatrix4fv(Shader::bbox->uniformLocation("M"), 1, GL_FALSE, M.data());
    glUniform3f(Shader::bbox->uniformLocation("corner_min"), min.x(), min.y(), min.z());
    glUniform3f(Shader::bbox->uniformLocation("corner_max"), max.x(), max.y(), max.z());
    glUniform1f(Shader::bbox->uniformLocation("scale"), scale);

    vao.bind();
    glEnable(GL_BLEND);
    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDrawArrays(GL_TRIANGLES, 0, 12*3);
    glDisable(GL_BLEND);
    vao.release();

    Shader::flat->release();
}


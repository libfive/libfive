#include <glm/gtc/type_ptr.hpp>

#include "ao/gl/axes.hpp"
#include "ao/gl/shader.hpp"

////////////////////////////////////////////////////////////////////////////////
// Vertex shader
const std::string Axes::vert = R"(
#version 330

layout(location=0) in vec3 vertex_position;
layout(location=1) in vec3 color;

uniform mat4 m;

out vec3 frag_color;

void main()
{
    gl_Position = m * vec4(vertex_position, 1.0f);
    frag_color = color;
}
)";

// Fragment shader
const std::string Axes::frag = R"(
#version 330

in vec3 frag_color;
in float frac;

out vec4 fragColor;

void main()
{
    fragColor = vec4(frag_color, 1.0f);
}
)";

////////////////////////////////////////////////////////////////////////////////

Axes::Axes()
    : vs(Shader::compile(vert, GL_VERTEX_SHADER)),
      fs(Shader::compile(frag, GL_FRAGMENT_SHADER)),
      prog(Shader::link(vs, fs))
{
    assert(vs);
    assert(fs);
    assert(prog);

    glGenBuffers(1, &vbo_solid);
    glGenBuffers(1, &vbo_wire);
    glGenVertexArrays(1, &vao_solid);
    glGenVertexArrays(1, &vao_wire);

    glBindVertexArray(vao_solid);
    {
        const float o = 0.05;

        // Data is arranged  x   y   z   r   g   b
        GLfloat data[] = {   /********************/
                             /*    X axis        */
                             o,  o,  o,  1,  0,  0,
                             o, -o,  o,  1,  0,  0,
                             1,  0,  0,  1,  0,  0,

                             o,  o, -o,  1,  0,  0,
                             o, -o, -o,  1,  0,  0,
                             1,  0,  0,  1,  0,  0,

                             o,  o,  o,  1,  0,  0,
                             o,  o, -o,  1,  0,  0,
                             1,  0,  0,  1,  0,  0,

                             o, -o,  o,  1,  0,  0,
                             o, -o, -o,  1,  0,  0,
                             1,  0,  0,  1,  0,  0,
                             /********************/
                             /*    Y axis        */
                             o,  o,  o,  0,  1,  0,
                            -o,  o,  o,  0,  1,  0,
                             0,  1,  0,  0,  1,  0,

                             o,  o, -o,  0,  1,  0,
                            -o,  o, -o,  0,  1,  0,
                             0,  1,  0,  0,  1,  0,

                             o,  o,  o,  0,  1,  0,
                             o,  o, -o,  0,  1,  0,
                             0,  1,  0,  0,  1,  0,

                            -o,  o,  o,  0,  1,  0,
                            -o,  o, -o,  0,  1,  0,
                             0,  1,  0,  0,  1,  0,
                             /********************/
                             /*    Z axis        */
                             o,  o,  o,  0,  0,  1,
                            -o,  o,  o,  0,  0,  1,
                             0,  0,  1,  0,  0,  1,

                             o, -o,  o,  0,  0,  1,
                            -o, -o,  o,  0,  0,  1,
                             0,  0,  1,  0,  0,  1,

                             o,  o,  o,  0,  0,  1,
                             o, -o,  o,  0,  0,  1,
                             0,  0,  1,  0,  0,  1,

                            -o,  o,  o,  0,  0,  1,
                            -o, -o,  o,  0,  0,  1,
                             0,  0,  1,  0,  0,  1,
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
        glBindBuffer(GL_ARRAY_BUFFER, vbo_solid);
        glBufferData(GL_ARRAY_BUFFER, sizeof(data),
                     data, GL_STATIC_DRAW);
        glVertexAttribPointer(
                0, 3, GL_FLOAT, GL_FALSE,
                6 * sizeof(GLfloat), (GLvoid*)0);
        glVertexAttribPointer(
                1, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(GLfloat),
                (GLvoid*)(3 * sizeof(GLfloat)));
        glEnableVertexAttribArray(0);
        glEnableVertexAttribArray(1);
    }

    glBindVertexArray(vao_wire);
    {
        // Data is arranged  x   y   z   r   g   b
        GLfloat data[] = {   0,  0,  0,  1,  0,  0,
                             1,  0,  0,  1,  0,  0,
                             0,  0,  0,  0,  1,  0,
                             0,  1,  0,  0,  1,  0,
                             0,  0,  0,  0,  0,  1,
                             0,  0,  1,  0,  0,  1,
        };
        glBindBuffer(GL_ARRAY_BUFFER, vbo_wire);
        glBufferData(GL_ARRAY_BUFFER, sizeof(data),
                     data, GL_STATIC_DRAW);
        glVertexAttribPointer(
                0, 3, GL_FLOAT, GL_FALSE,
                6 * sizeof(GLfloat), (GLvoid*)0);
        glVertexAttribPointer(
                1, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(GLfloat),
                (GLvoid*)(3 * sizeof(GLfloat)));
        glEnableVertexAttribArray(0);
        glEnableVertexAttribArray(1);
    }
    glBindVertexArray(0);
}

Axes::~Axes()
{
    glDeleteShader(vs);
    glDeleteShader(fs);
    glDeleteProgram(prog);

    glDeleteBuffers(1, &vbo_solid);
    glDeleteBuffers(1, &vbo_wire);

    glDeleteVertexArrays(1, &vao_solid);
    glDeleteVertexArrays(1, &vao_wire);
}

void Axes::draw(const glm::mat4& m) const
{
    glUseProgram(prog);
    glUniformMatrix4fv(glGetUniformLocation(prog, "m"),
                       1, GL_FALSE, glm::value_ptr(m));

    glBindVertexArray(vao_solid);
    glDrawArrays(GL_TRIANGLES, 0, 54);

    glDisable(GL_DEPTH_TEST);
    glBindVertexArray(vao_wire);
    glDrawArrays(GL_LINES, 0, 6);
    glEnable(GL_DEPTH_TEST);

    glBindVertexArray(0);
}

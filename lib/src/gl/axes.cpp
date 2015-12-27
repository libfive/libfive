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
out float frac;

void main()
{
    gl_Position = m * vec4(vertex_position, 1.0f);
    frag_color = color;
    frac = vertex_position.x + vertex_position.y + vertex_position.z;
}
)";

// Fragment shader
const std::string Axes::frag = R"(
#version 330

in vec3 frag_color;
in float frac;
uniform int dotted;

out vec4 fragColor;

void main()
{
    if (dotted == 1 && mod(frac * 20.0f, 1.0f) >= 0.8f)
    {
        discard;
    }
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

    glGenBuffers(1, &vbo);
    glGenVertexArrays(1, &vao);

    glBindVertexArray(vao);
    {
        // Data is arranged  x    y     z     r     g     b
        GLfloat data[] = {0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f,
                          1.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f,
                          0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f,
                          0.0f, 1.0f, 0.0f, 0.0f, 1.0f, 0.0f,
                          0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f,
                          0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 1.0f};
        glBindBuffer(GL_ARRAY_BUFFER, vbo);
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

    glDeleteBuffers(1, &vbo);
    glDeleteVertexArrays(1, &vao);
}

void Axes::draw(const glm::mat4& m) const
{
    glUseProgram(prog);
    GLint m_loc = glGetUniformLocation(prog, "m");
    glUniformMatrix4fv(m_loc, 1, GL_FALSE, glm::value_ptr(m));

    glBindVertexArray(vao);

    // Draw once with depth test and non-dotted lines
    glEnable(GL_DEPTH_TEST);
    glUniform1i(glGetUniformLocation(prog, "dotted"), 0);
    glDrawArrays(GL_LINES, 0, 6);

    // Then draw again without depth test and with dotted lines
    glDisable(GL_DEPTH_TEST);
    glUniform1i(glGetUniformLocation(prog, "dotted"), 1);
    glDrawArrays(GL_LINES, 0, 6);

    glBindVertexArray(0);
}

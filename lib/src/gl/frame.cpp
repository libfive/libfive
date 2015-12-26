#include <glm/gtc/type_ptr.hpp>

#include "ao/gl/frame.hpp"
#include "ao/gl/shader.hpp"

////////////////////////////////////////////////////////////////////////////////
// Vertex shader
const std::string Frame::vert = R"(
#version 330

layout(location=0) in vec3 vertex_position;

uniform mat4 m;

void main()
{
    gl_Position = m * vec4(vertex_position, 1.0f);
}
)";

// Fragment shader
const std::string Frame::frag = R"(
#version 330

out vec4 fragColor;

void main()
{
    fragColor = vec4(1.0f, 0.0f, 0.0f, 1.0f);
}
)";

////////////////////////////////////////////////////////////////////////////////

Frame::Frame()
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
    for (auto t :texs)
    {
        glDeleteBuffers(1, &t.second.depth);
        glDeleteBuffers(1, &t.second.normal);
    }

    glDeleteBuffers(1, &vbo);
    glDeleteVertexArrays(1, &vao);
}

void Frame::draw(const glm::mat4& m) const
{
    (void)m;

    glBindVertexArray(vao);

    glUseProgram(prog);
    GLint m_loc = glGetUniformLocation(prog, "m");
    glUniformMatrix4fv(m_loc, 1, GL_FALSE, glm::value_ptr(m));

    for (auto t : texs)
    {
        (void)t;
        glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
        glBindVertexArray(0);
    }
}

void Frame::push(const glm::mat4& m)
{
    texs.push_back({m, {0,0}});
    glGenTextures(1, &texs.back().second.depth);
    glGenTextures(1, &texs.back().second.normal);
}

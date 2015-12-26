#include <glm/gtx/transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#include "ao/gl/frame.hpp"
#include "ao/gl/shader.hpp"

#include "ao/core/region.hpp"
#include "ao/core/tree.hpp"

#include "ao/render/heightmap.hpp"

////////////////////////////////////////////////////////////////////////////////
// Vertex shader
const std::string Frame::vert = R"(
#version 330

layout(location=0) in vec3 vertex_position;

uniform mat4 m;
out vec2 tex_coord;

void main()
{
    tex_coord = (vertex_position.xy + 1.0f) / 2.0f;
    gl_Position = m * vec4(vertex_position, 1.0f);
}
)";

// Fragment shader
const std::string Frame::frag = R"(
#version 330

in vec2 tex_coord;
uniform sampler2D tex;

out vec4 fragColor;

void main()
{
    float t = texture(tex, tex_coord).r;
    if (isinf(t))
    {
        discard;
    }
    else
    {
        float z = (t + 1) / 2;
        fragColor = vec4(t, t, t, 1.0f);
    }
}
)";

////////////////////////////////////////////////////////////////////////////////

Frame::Frame(Tree* tree)
    : tree(tree), vs(Shader::compile(vert, GL_VERTEX_SHADER)),
      fs(Shader::compile(frag, GL_FRAGMENT_SHADER)), prog(Shader::link(vs, fs))
{
    assert(vs);
    assert(fs);
    assert(prog);

    glGenBuffers(1, &vbo);
    glGenVertexArrays(1, &vao);

    glBindVertexArray(vao);
    {
        GLfloat vertices[] = {-1.0f, -1.0f, 0.0f,
                               1.0f, -1.0f, 0.0f,
                               1.0f,  1.0f, 0.0f,
                              -1.0f,  1.0f, 0.0f};
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

    // Active the shader
    glUseProgram(prog);

    // Bind the vertex array (which sets up VBO bindings)
    glBindVertexArray(vao);

    // Get the uniform location for the transform matrix
    GLint m_loc = glGetUniformLocation(prog, "m");

    // Bind the "tex" sampler to TEXTURE0
    glActiveTexture(GL_TEXTURE0);
    glUniform1i(glGetUniformLocation(prog, "tex"), 0);

    glEnable(GL_DEPTH_TEST);
    for (auto t : texs)
    {
        auto mat = m * glm::inverse(t.first);
        glUniformMatrix4fv(m_loc, 1, GL_FALSE, glm::value_ptr(mat));

        glBindTexture(GL_TEXTURE_2D, t.second.depth);
        glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
    }
    glDisable(GL_DEPTH_TEST);
    glBindVertexArray(0);
}

void Frame::push(const glm::mat4& m)
{
    texs.push_back({m, {0,0}});
    glGenTextures(1, &texs.back().second.depth);
    glGenTextures(1, &texs.back().second.normal);

    // Render the frame to an Eigen matrix and cast to float
    Region r({-1, 1}, {-1, 1}, {-1, 1}, 10);
    tree->setMatrix(glm::inverse(m));
    Eigen::ArrayXXf out = Heightmap::Render(tree, r).cast<float>().transpose();

    // Pack the Eigen matrix into an OpenGL texture
    glPixelStorei(GL_UNPACK_ALIGNMENT, 4); // Floats are 4-byte aligned
    glBindTexture(GL_TEXTURE_2D, texs.back().second.depth);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, out.cols(), out.rows(),
            0, GL_RED, GL_FLOAT, out.data());
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
}

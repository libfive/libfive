#include <glm/gtx/transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#include "ao/gl/frame.hpp"
#include "ao/gl/shader.hpp"

#include "ao/core/region.hpp"
#include "ao/core/tree.hpp"

#include "ao/ui/worker.hpp"

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

uniform mat4 m;

in vec2 tex_coord;
uniform sampler2D depth;
uniform sampler2D norm;

out vec4 fragColor;

vec4 shade(vec3 norm)
{
    vec3 light = vec3(0.99f, 0.96f, 0.89f);
    vec3 dark  = vec3(0.20f, 0.25f, 0.30f);

    // a is a light pointed directly down
    float a = dot(norm, vec3(0.0f,   0.0f, 1.0f));

    // b is a light coming in from an angle
    float b = dot(norm, vec3(0.57f, 0.57f, 0.57f));

    return vec4((a*light + (1.0f - a)*dark)*0.35f +
                (b*light + (1.0f - b)*dark)*0.65f, 1.0f);
}

void main()
{
    float d = texture(depth, tex_coord).r;
    if (isinf(d))
    {
        discard;
    }
    else
    {
        float z = (1.0f - d) / 2.0f;
        gl_FragDepth = z;
        fragColor = shade(2.0f * (texture(norm, tex_coord).rgb - 0.5f));
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

    tree->parent = this;
    tree->buildAccelerator();

    glGenTextures(1, &depth);
    glGenTextures(1, &norm);

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
    glDeleteShader(vs);
    glDeleteShader(fs);
    glDeleteProgram(prog);

    glDeleteTextures(1, &depth);
    glDeleteTextures(1, &norm);
    glDeleteBuffers(1, &vbo);
    glDeleteVertexArrays(1, &vao);
}

void Frame::draw(const glm::mat4& m) const
{
    if (!current.valid())
    {
        return;
    }

    // Active the shader
    glUseProgram(prog);

    // Bind the vertex array (which sets up VBO bindings)
    glBindVertexArray(vao);

    // Get the uniform location for the transform matrix
    GLint m_loc = glGetUniformLocation(prog, "m");

    // Calculate the appropriate transform matrix
    auto mat = m * glm::inverse(current.mat);
    glUniformMatrix4fv(m_loc, 1, GL_FALSE, glm::value_ptr(mat));

    // Bind depth and normal texture
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, depth);
    glUniform1i(glGetUniformLocation(prog, "depth"), 0);
    glActiveTexture(GL_TEXTURE0 + 1);
    glBindTexture(GL_TEXTURE_2D, norm);
    glUniform1i(glGetUniformLocation(prog, "norm"), 1);

    // Draw the quad!
    glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

    glBindVertexArray(0);
}

////////////////////////////////////////////////////////////////////////////////

void Frame::render(const glm::mat4& m, size_t ni, size_t nj, size_t nk)
{
    const float DEFAULT_LEVEL = 8;

    next = Task(tree.get(), m, ni, nj, nk, DEFAULT_LEVEL);

    // If a task is running and isn't a min-resolution render action,
    // set the abort flag so that it stops early
    if (worker)
    {
        if (pending.level < DEFAULT_LEVEL)
        {
            worker->halt();
        }
    }
    else
    {
        startRender();
    }
}

void Frame::startRender()
{
    assert(!pending.valid());
    assert(worker == nullptr);

    // Start up the next render task
    if (next.valid())
    {
        // Swap around render tasks and start an async worker
        pending = next;
        worker.reset(next.start());
        next.reset();
    }
    // Schedule a refinement of the current render task
    else if (current.level > 1)
    {
        next = current;
        next.level /= 2;
        startRender();
    }
}

bool Frame::poll()
{
    if (worker == nullptr)
    {
        return false;
    }
    else if (worker->poll(depth, norm))
    {
        // Swap tasks objects
        current = pending;
        pending.reset();

        // Release the worker (which joins the thread)
        worker.reset();

        // Attempt to kick off a new render
        startRender();

        return true;
    }
    else
    {
        return false;
    }
}

bool Frame::running() const
{
    return worker && worker->running();
}

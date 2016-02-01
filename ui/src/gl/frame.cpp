#include <glm/gtx/transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#include "ao/ui/gl/frame.hpp"
#include "ao/ui/gl/shader.hpp"

#include "ao/ui/worker.hpp"

#include "ao/kernel/render/heightmap.hpp"
#include "ao/kernel/render/region.hpp"
#include "ao/kernel/tree/tree.hpp"

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
    if (d == 1.0f)
    {
        discard;
    }
    else
    {
        // Map a depth in the range [1, -1] to the depth buffer's [0, 1] range
        gl_FragDepth = d;
        fragColor = shade(2.0f * (texture(norm, tex_coord).rgb - 0.5f));
    }
}
)";

////////////////////////////////////////////////////////////////////////////////

Frame::Frame(Tree* tree)
    : tree(tree),
      vs(Shader::compile(vert, GL_VERTEX_SHADER)),
      fs(Shader::compile(frag, GL_FRAGMENT_SHADER)),
      prog(Shader::link(vs, fs))
{
    assert(vs);
    assert(fs);
    assert(prog);

    tree->parent = this;

    glGenTextures(2, depth);
    glGenTextures(2, norm);

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

    glDeleteTextures(2, depth);
    glDeleteTextures(2, norm);
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
    glBindTexture(GL_TEXTURE_2D, depth[ping]);
    glUniform1i(glGetUniformLocation(prog, "depth"), 0);
    glActiveTexture(GL_TEXTURE0 + 1);
    glBindTexture(GL_TEXTURE_2D, norm[ping]);
    glUniform1i(glGetUniformLocation(prog, "norm"), 1);

    // Draw the quad!
    glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

    glBindVertexArray(0);
}

////////////////////////////////////////////////////////////////////////////////

void Frame::render(const glm::mat4& m, size_t ni, size_t nj, size_t nk)
{
    next = Task(m, ni, nj, nk, default_level);

    // If a task is running and isn't a min-resolution render action,
    // set the abort flag so that it stops early
    if (worker)
    {
        if (pending.level < default_level)
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
        worker.reset(new Worker(tree.get(), pending));
        next.reset();
    }
    // Schedule a refinement of the current render task
    else if (current.level > 1)
    {
        next = current;
        next.level--;
        startRender();
    }
}

bool Frame::poll()
{
    bool success = false;

    if (worker != nullptr)
    {
        // If the worker has successfully finished, swap task objects,
        // swap textures, and mark success as true
        Worker::State state = worker->poll(depth[!ping], norm[!ping]);

        if (state == Worker::DONE)
        {
            // Adjust the default level based on performance, trying to keep
            // framerate between 20 and 40 fps
            if (pending.level == default_level)
            {
                if (worker->elapsed < std::chrono::milliseconds(25))
                {
                    if (default_level > 1)
                    {
                        default_level--;
                    }
                }
                else if (worker->elapsed > std::chrono::milliseconds(50))
                {
                    default_level++;
                }
            }

            // Swap tasks objects
            current = pending;

            ping = !ping;
            success = true;
        }

        // If the worker has finished (either DONE or ABORTED), clear the
        // pending task and worker pointer (which joins the thread)
        if (state != Worker::RUNNING)
        {
            pending.reset();
            worker.reset();

            // Attempt to kick off a new render
            startRender();
        }
    }
    return success;
}

bool Frame::running() const
{
    return worker && worker->running();
}

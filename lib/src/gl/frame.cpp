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
    if (thread.joinable())
    {
        thread.join();
    }

    delete tree;

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
    next = Task(m, ni, nj, nk, 8);
    if (!future.valid())
    {
        startRender();
    }
}

void Frame::startRender()
{
    assert(!future.valid());
    assert(!pending.valid());

    // Start up the next render task
    if (next.valid())
    {
        // Create the target region for rendering
        // (allocated on the heap so it can persist)
        double div = 2.0 * next.level;
        Region* r = new Region({-1, 1}, {-1, 1}, {-1, 1},
                               next.ni/div, next.nj/div, next.nk/div);

        // Compensate for the fact that OpenGL is a left-handed system
        // then apply the matrix to the tree
        auto m_ = glm::scale(glm::inverse(next.mat), glm::vec3(1, 1, -1));
        tree->setMatrix(m_);

        // Swap around render tasks
        pending = next;
        next.reset();

        // Then kick off an async render operation which sets a future and
        // sends an empty glfw event when it is complete (also responsible
        // for cleaning up the Region allocated above)
        promise = decltype(promise)();
        future = promise.get_future();
        thread = std::thread([=](){
                auto depth = Heightmap::Render(tree, *r);
                auto shaded = Heightmap::Shade(tree, *r, depth);
                delete r;
                this->promise.set_value(std::make_pair(depth, shaded));
                glfwPostEmptyEvent(); });
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
    if(!future.valid())
    {
        return false;
    }

    std::future_status status = future.wait_for(std::chrono::seconds(0));
    if (status == std::future_status::ready)
    {
        // Get the resulting matrix
        auto out = future.get();
        Eigen::ArrayXXf d = out.first.cast<float>().transpose();
        Image s = out.second.transpose();

        // Pack the Eigen matrices into an OpenGL texture
        glPixelStorei(GL_UNPACK_ALIGNMENT, 4); // Floats are 4-byte aligned
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, depth);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, d.rows(), d.cols(),
                0, GL_RED, GL_FLOAT, d.data());
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

        glActiveTexture(GL_TEXTURE0 + 1);
        glBindTexture(GL_TEXTURE_2D, norm);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, s.rows(), s.cols(),
                0, GL_RGBA, GL_UNSIGNED_BYTE, s.data());
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

        // Swap tasks objects
        current = pending;
        pending.reset();

        // Join the task
        thread.join();

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
    return future.valid() && future.wait_for(std::chrono::seconds(0)) ==
                             std::future_status::timeout;
}

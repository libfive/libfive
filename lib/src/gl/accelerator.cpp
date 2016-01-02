#include <cassert>

#include "ao/gl/accelerator.hpp"
#include "ao/gl/shader.hpp"

#include "ao/core/atom.hpp"
#include "ao/core/region.hpp"
#include "ao/core/tree.hpp"

////////////////////////////////////////////////////////////////////////////////
// Vertex shader
const std::string Accelerator::vert = R"(
#version 330

layout(location=0) in vec2 vertex_position;

out vec2 pos;

uniform vec2 xbounds;
uniform vec2 ybounds;

void main()
{
    // Normalized (0-1) xy coordinates
    vec2 norm = (vertex_position.xy + 1.0f) / 2.0f;

    // Position of the fragment in region space
    pos = vec2(norm.x * (xbounds[1]- xbounds[0]) + xbounds[0],
               norm.y * (ybounds[1]- ybounds[0]) + ybounds[0]);

    gl_Position = vec4(vertex_position, 0.0f, 1.0f);
}
)";

const std::string Accelerator::frag = R"(
#version 330

in vec2 pos;
out float frag_depth;

// Generic matrix transform
uniform float mat[12];

uniform int nk;
uniform vec2 zbounds;

// Forward declaration of f-rep function
float f(float x, float y, float z);

////////////////////////////////////////////////////////////////////////////////
// Gradient math
vec4 add_g(vec4 a, vec4 b)
{
    return a + b;
}

vec4 sub_g(vec4 a, vec4 b)
{
    return a - b;
}

vec4 mul_g(vec4 a, vec4 b)
{
    // Product rule
    return vec4(a.w * b.w,
            a.w * b.x + b.w * a.x,
            a.w * b.y + b.w * a.y,
            a.w * b.z + b.w * a.z);
}

vec4 div_g(vec4 a, vec4 b)
{
    // Quotient rule
    float p = pow(b.w, 2.0f);
    return vec4(a.w / b.w,
            (b.w * a.x - a.w * b.x) / p,
            (b.w * a.y - a.w * b.y) / p,
            (b.w * a.z - a.w * b.z) / p);
}

vec4 min_g(vec4 a, vec4 b)
{
    return (a.w < b.w) ? a : b;
}

vec4 max_g(vec4 a, vec4 b)
{
    return (a.w < b.w) ? b : a;
}

vec4 cond_nz_g(vec4 cond, vec4 a, vec4 b)
{
    return (cond.w < 0.0f) ? a : b;
}

vec4 pow_g(vec4 a, vec4 b)
{
    float p = pow(a.w, b.w - 1.0f);
    float m = a.w * log(a.w);

    // If a.w is negative, then m will be NaN (because of log's domain).
    // We work around this by checking if d/d{xyz}(B) == 0 and using a
    // simplified expression if that's true.
    return vec4(pow(a.w, b.w),
        p * (b.w*a.x + (b.x != 0.0f ? m*b.x : 0.0f)),
        p * (b.w*a.y + (b.y != 0.0f ? m*b.y : 0.0f)),
        p * (b.w*a.z + (b.z != 0.0f ? m*b.z : 0.0f)));
}

vec4 sqrt_g(vec4 a)
{
    if (a.w < 0.0f)
    {
        return vec4(0.0f);
    }
    else
    {
        float v = sqrt(a.w);
        return vec4(v, a.x / (2.0f * v),
                       a.y / (2.0f * v),
                       a.z / (2.0f * v));
    }
}

vec4 neg_g(vec4 a)
{
    return -a;
}

////////////////////////////////////////////////////////////////////////////////

void main()
{
    float x = pos.x;
    float y = pos.y;

    // Set the default depth to a value below zmin
    // (as GLSL doesn't support -inf directly)
    frag_depth = zbounds[0] - 1;
    for (int i=0; i < nk; ++i)
    {
        float frac = (i + 0.5f) / nk;
        float z = zbounds[1] * (1 - frac) + zbounds[0] * frac;

        if (f(x, y, z) < 0)
        {
            frag_depth = z;
            break;
        }
    }
}
)";
////////////////////////////////////////////////////////////////////////////////

Accelerator::Accelerator(const Tree* tree)
    : mat({{1, 0, 0, 0,
            0, 1, 0, 0,
            0, 0, 1, 0}})
{
    GLFWwindow* prev = glfwGetCurrentContext();

    context = makeContext();
    assert(context);

    vs = Shader::compile(vert, GL_VERTEX_SHADER);
    fs = Shader::compile(toShader(tree), GL_FRAGMENT_SHADER);
    prog = Shader::link(vs, fs);

    assert(vs);
    assert(fs);
    assert(prog);

    glGenBuffers(1, &vbo);
    glGenVertexArrays(1, &vao);

    // Generate and bind a simple quad shape
    glBindVertexArray(vao);
    {
        GLfloat vertices[] = {-1.0f, -1.0f,
                               1.0f, -1.0f,
                               1.0f,  1.0f,
                              -1.0f,  1.0f};
        glBindBuffer(GL_ARRAY_BUFFER, vbo);
        glBufferData(GL_ARRAY_BUFFER, sizeof(vertices),
                     vertices, GL_STATIC_DRAW);
        glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE,
                              2 * sizeof(GLfloat), (GLvoid*)0);
        glEnableVertexAttribArray(0);
    }
    glBindVertexArray(0);

    glGenTextures(1, &tex);
    glGenFramebuffers(1, &fbo);

    // Restore the previous context
    glfwMakeContextCurrent(prev);
}

Accelerator::~Accelerator()
{
    // Save the previous context then make this Accelerator's context current
    GLFWwindow* prev = glfwGetCurrentContext();
    glfwMakeContextCurrent(context);

    // Delete all the things!
    glDeleteShader(vs);
    glDeleteShader(fs);
    glDeleteProgram(prog);

    glDeleteBuffers(1, &vbo);
    glDeleteVertexArrays(1, &vao);

    glDeleteFramebuffers(1, &fbo);
    glDeleteTextures(1, &tex);

    // Restore previous context
    glfwMakeContextCurrent(prev);

    // Destroy our context
    glfwDestroyWindow(context);
}

////////////////////////////////////////////////////////////////////////////////

void Accelerator::makeContextCurrent() const
{
    glfwMakeContextCurrent(context);
}

////////////////////////////////////////////////////////////////////////////////

void Accelerator::setMatrix(const glm::mat4& m)
{
    size_t k = 0;
    for (int i=0; i < 3; ++i)
    {
        for (int j=0; j < 4; ++j)
        {
            mat[k++] = m[j][i];
        }
    }
}

Eigen::ArrayXXd Accelerator::Render(const Region& r)
{
    Eigen::ArrayXXd out(r.Y.size, r.X.size);
    Render(r, out);
    return out;
}

void Accelerator::Render(const Region& r, Eigen::ArrayXXd& img)
{
    // Generate a texture of the appropriate size
    glBindTexture(GL_TEXTURE_2D, tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_R32F, r.X.size, r.Y.size,
                 0, GL_RED, GL_FLOAT, 0);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

    // Bind the desired texture to the framebuffer
    glBindFramebuffer(GL_FRAMEBUFFER, fbo);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
                           GL_TEXTURE_2D, tex, 0);

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    // Set the viewport to the appropriate size
    glViewport(0, 0, r.X.size, r.Y.size);

    glUseProgram(prog);
    glBindVertexArray(vao);

    // Load various uniforms defining the render bounds
    glUniform2f(glGetUniformLocation(prog, "xbounds"),
                r.X.lower(), r.X.upper());
    glUniform2f(glGetUniformLocation(prog, "ybounds"),
                r.Y.lower(), r.Y.upper());
    glUniform2f(glGetUniformLocation(prog, "zbounds"),
                r.Z.lower(), r.Z.upper());
    glUniform1i(glGetUniformLocation(prog, "nk"), r.Z.size);

    // Load the generic transform matrix into the shader
    glUniform1fv(glGetUniformLocation(prog, "mat"), 12, &mat[0]);

    // Draw the full rectangle into the FBO
    glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
    glBindVertexArray(0);

    // Switch back to the default framebuffer.
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    // Copy matrix to an Eigen array
    Eigen::ArrayXXf out(r.X.size, r.Y.size);
    glBindTexture(GL_TEXTURE_2D, tex);
    glGetTexImage(GL_TEXTURE_2D, 0, GL_RED, GL_FLOAT, out.data());

    // Mask all of the lower points with -infinity
    out = (out < r.Z.lower()).select(
            -std::numeric_limits<float>::infinity(), out);

    // Assign into the output depth image
    img.block(r.Y.min, r.X.min, r.Y.size, r.X.size) =
        out.transpose().cast<double>();
}

////////////////////////////////////////////////////////////////////////////////

std::string Accelerator::toShaderFunc(const Tree* tree, Mode mode)
{
    std::string out = (mode == DEPTH)
        ? "float f(float x, float y, float z) {\n"
        : "vec4 g(vec4 x, vec4 y, vec4 z) {\n";

    // Reset the atoms and index objects
    atoms.clear();
    index = 0;

    // Hard-code the tree's root as atom 0
    atoms[tree->root] = index++;

    // Hard-code the matrix atoms as atoms 0-11
    // (in a separately-indexed array named 'mat')
    for (int i=0; i < 12; ++i)
    {
        atoms[tree->matrix[i]] = i;
    }

    // Build shader line-by-line from the active atoms
    for (const auto& row : tree->rows)
    {
        for (size_t i=0; i < row.active; ++i)
        {
            out += toShader(row[i], mode);
        }
    }

    out += "return m0;}";
    return out;
}

std::string Accelerator::toShader(const Tree* tree)
{
    return frag + toShaderFunc(tree, DEPTH) + "\n\n"
                + toShaderFunc(tree, NORMAL);
}

std::string Accelerator::toShader(const Atom* m, Accelerator::Mode mode)
{
    // Each atom should be stored into the hashmap only once.
    // There's a special case for the tree's root, which is pre-emptively
    // inserted into the hashmap at index 0 (to make the end of the shader
    // easy to write).
    assert(atoms.count(m) == 0 || atoms[m] == 0);

    // Store this atom in the array if it is not already present;
    // otherwise, update the index from the hashmap
    if (!atoms.count(m))
    {
        atoms[m] = index++;
    }
    size_t i = atoms[m];

    std::string out = ((mode == DEPTH) ? "    float m"
                                       : "    vec4 m") + std::to_string(i)
                                                       + " = ";
    auto get = [&](Atom* m){
        if (m)
        {
            auto itr = atoms.find(m);

            // Special-case for mutable atoms, which are assumed to be part
            // of the generic transform matrix and are GLSL uniforms
            if (m->op == OP_MUTABLE)
            {
                assert(itr != atoms.end());
                assert(itr->second < 12);
                return (mode == DEPTH)
                    ? ("mat[" + std::to_string(itr->second) + "]")
                    : ("vec4(mat[" + std::to_string(itr->second)
                                   + "], 0.0f, 0.0f, 0.0f)");
            }
            // If the atom is already stored, save it
            else if (itr != atoms.end())
            {
                return "m" + std::to_string(itr->second);
            }
            // Otherwise, the atom must be something hard-coded!
            else switch (m->op)
            {
                case OP_X:       return std::string("x");
                case OP_Y:       return std::string("y");
                case OP_Z:       return std::string("z");
                case OP_CONST:   return (mode == DEPTH)
                                 ? (std::to_string(m->value) + "f")
                                 : ("vec4(" + std::to_string(m->value)
                                            + "f, 0.0f, 0.0f, 0.0f)");
                default: assert(false);
            }
        }
        return std::string(); };
    std::string sa = get(m->a);
    std::string sb = get(m->b);
    std::string sc = get(m->cond);

    if (mode == DEPTH)
    {
        switch (m->op)
        {
            case OP_ADD:    out += "(" + sa + " + " + sb + ")";     break;
            case OP_MUL:    out += "(" + sa + " * " + sb + ")";     break;
            case OP_MIN:    out += "min(" + sa + ", " + sb + ")";   break;
            case OP_MAX:    out += "max(" + sa + ", " + sb + ")";   break;
            case OP_SUB:    out += "(" + sa + " - " + sb + ")";     break;
            case OP_DIV:    out += "(" + sa + " / " + sb + ")";     break;
            case OP_SQRT:   out += "sqrt(" + sa + ")";  break;
            case OP_NEG:    out += "(-" + sa + ")";     break;

            case COND_LZ:   out += "(" + sc + " < 0 ? " + sa + " : " + sb + ")";
                            break;

            case OP_X:  // Fallthrough!
            case OP_Y:
            case OP_Z:
            case LAST_OP:
            case OP_CONST:
            case OP_MUTABLE:
            case INVALID:   assert(false);
        }
    }
    else if (mode == NORMAL)
    {
        switch (m->op)
        {
            case OP_ADD:    out += "add_g(" + sa + ", " + sb + ")";     break;
            case OP_MUL:    out += "mul_g(" + sa + ", " + sb + ")";     break;
            case OP_MIN:    out += "min_g(" + sa + ", " + sb + ")";   break;
            case OP_MAX:    out += "max_g(" + sa + ", " + sb + ")";   break;
            case OP_SUB:    out += "sub_g(" + sa + ", " + sb + ")";     break;
            case OP_DIV:    out += "div_g(" + sa + ", " + sb + ")";     break;
            case OP_SQRT:   out += "sqrt_g(" + sa + ")";  break;
            case OP_NEG:    out += "neg_g(" + sa + ")";     break;

            case COND_LZ:   out += "cond_nz_g(" + sc + ", " + sa + ", " + sb + ")";
                            break;

            case OP_X:  // Fallthrough!
            case OP_Y:
            case OP_Z:
            case LAST_OP:
            case OP_CONST:
            case OP_MUTABLE:
            case INVALID:   assert(false);
        }
    }

    return out + ";\n";
}

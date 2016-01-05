#include <cassert>

#include "ao/gl/accelerator.hpp"
#include "ao/gl/shader.hpp"
#include "ao/gl/texture.hpp"

#include "ao/tree/atom.hpp"
#include "ao/tree/tree.hpp"

#include "ao/render/region.hpp"

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
layout(location=0) out vec4 frag_norm;

// Generic matrix transform
uniform float mat[12];

// Bounds within this raycast region
uniform vec2 zbounds;

// Global Z render bounds
uniform vec2 zglobal;

// Number of raycast voxels
uniform int nk;

// Forward declaration of f-rep function and normal function
float f(float x, float y, float z);
vec4 g(vec4 x, vec4 y, vec4 z);

void main()
{
    float x = pos.x;
    float y = pos.y;

    // Set the default depth to the back value
    gl_FragDepth = 1.0f;
    frag_norm = vec4(0.0f);

    for (int i=0; i < nk; ++i)
    {
        float frac = (i + 0.5f) / nk;
        float z = zbounds[1] * (1 - frac) + zbounds[0] * frac;

        if (f(x, y, z) < 0.0f)
        {
            // Map the z coordinate to the 0 - 1 depth buffer
            gl_FragDepth = (zglobal[1] - z) / (zglobal[1] - zglobal[0]);

            // Calculate normal
            vec4 n = g(vec4(1.0f, 0.0f, 0.0f, x),
                       vec4(0.0f, 1.0f, 0.0f, y),
                       vec4(0.0f, 0.0f, 1.0f, z));

            // Pack normal into 0-255 range
            if (i == 0)
            {
                frag_norm = vec4(0.5f, 0.5f, 1.0f, 1.0f);
            }
            else
            {
                frag_norm = vec4(normalize(n.xyz) / 2.0f + 0.5f, 1.0f);
            }
            return;
        }
    }

    // Discard fragments that aren't found in raycasting
    discard;
}

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
    return vec4(a.w * b.x + b.w * a.x,
                a.w * b.y + b.w * a.y,
                a.w * b.z + b.w * a.z,
                a.w * b.w);
}

vec4 div_g(vec4 a, vec4 b)
{
    // Quotient rule
    float p = pow(b.w, 2.0f);
    return vec4((b.w * a.x - a.w * b.x) / p,
                (b.w * a.y - a.w * b.y) / p,
                (b.w * a.z - a.w * b.z) / p,
                a.w / b.w);
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
    return vec4(
        p * (b.w*a.x + (b.x != 0.0f ? m*b.x : 0.0f)),
        p * (b.w*a.y + (b.y != 0.0f ? m*b.y : 0.0f)),
        p * (b.w*a.z + (b.z != 0.0f ? m*b.z : 0.0f)),
        pow(a.w, b.w));
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
        return vec4(a.x / (2.0f * v),
                    a.y / (2.0f * v),
                    a.z / (2.0f * v), v);
    }
}

vec4 neg_g(vec4 a)
{
    return -a;
}
)";

////////////////////////////////////////////////////////////////////////////////

Accelerator::Accelerator(const Tree* tree)
    : mat({{1, 0, 0, 0,
            0, 1, 0, 0,
            0, 0, 1, 0}})
{
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

    glGenFramebuffers(1, &fbo);
}

Accelerator::~Accelerator()
{
    // Delete all the things!
    glDeleteShader(vs);
    glDeleteShader(fs);
    glDeleteProgram(prog);

    glDeleteBuffers(1, &vbo);
    glDeleteVertexArrays(1, &vao);

    glDeleteFramebuffers(1, &fbo);
}

////////////////////////////////////////////////////////////////////////////////

void Accelerator::init(const Region& r, GLuint depth, GLuint norm)
{
    // Generate a depth texture of the appropriate size
    glBindTexture(GL_TEXTURE_2D, depth);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT32F, r.X.size, r.Y.size,
                 0, GL_DEPTH_COMPONENT, GL_FLOAT, 0);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

    // Generate a normal texture of the appropriate size
    glBindTexture(GL_TEXTURE_2D, norm);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, r.X.size, r.Y.size,
                 0, GL_RGBA, GL_UNSIGNED_BYTE, 0);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

    // Enable depth buffer
    glEnable(GL_DEPTH_TEST);

    // Bind the target textures to the framebuffer and clear them
    glBindFramebuffer(GL_FRAMEBUFFER, fbo);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT,
                           GL_TEXTURE_2D, depth, 0);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
                           GL_TEXTURE_2D, norm, 0);

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    // Select the active program
    glUseProgram(prog);

    // Bind the VAO
    glBindVertexArray(vao);

    // Load the global Z bounds
    glUniform2f(glGetUniformLocation(prog, "zglobal"),
                r.Z.lower(), r.Z.upper());

    // Load the generic transform matrix into the shader
    glUniform1fv(glGetUniformLocation(prog, "mat"), 12, &mat[0]);
}

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

std::pair<DepthImage, NormalImage> Accelerator::Render(const Region& r)
{
    GLuint depth, norm;
    glGenTextures(1, &depth);
    glGenTextures(1, &norm);

    init(r, depth, norm);
    RenderSubregion(r);

    auto out = std::make_pair(fromDepthTexture(depth, r),
                              fromNormalTexture(norm, r));

    glDeleteTextures(1, &depth);
    glDeleteTextures(1, &norm);

    return out;
}

void Accelerator::RenderSubregion(const Region& r)
{
    {   // Assert that our program is running
        GLint p;
        glGetIntegerv(GL_CURRENT_PROGRAM, &p);
        assert(p == prog);
    }

    // Set the viewport to the appropriate size
    glViewport(r.X.min, r.Y.min, r.X.size, r.Y.size);

    // Load various uniforms defining the render bounds
    glUniform2f(glGetUniformLocation(prog, "xbounds"),
                r.X.lower(), r.X.upper());
    glUniform2f(glGetUniformLocation(prog, "ybounds"),
                r.Y.lower(), r.Y.upper());
    glUniform2f(glGetUniformLocation(prog, "zbounds"),
                r.Z.lower(), r.Z.upper());
    glUniform1i(glGetUniformLocation(prog, "nk"), r.Z.size);

    // Draw the full rectangle into the FBO
    glDrawArrays(GL_TRIANGLE_FAN, 0, 4);
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

    // Hard-code the matrix atoms as atoms 0-11
    // (in a separately-indexed array named 'mat')
    for (int i=0; i < 12; ++i)
    {
        atoms[tree->matrix[i]] = i;
    }

    // Build shader line-by-line from the active atoms
    for (const auto& row : tree->rows)
    {
        for (size_t i=0; i < row.size(); ++i)
        {
            if (row[i] != tree->root)
            {
                out += toShader(row[i], mode);
            }
        }
    }
    out += toShader(tree->root, mode);
    out += "return m" + std::to_string(atoms[tree->root]) + ";}";
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
    auto get = [&](const Atom* n){
        if (n)
        {
            auto itr = atoms.find(n);

            // Special-case for mutable atoms, which are assumed to be part
            // of the generic transform matrix and are GLSL uniforms
            if (n->op == OP_MUTABLE)
            {
                assert(itr != atoms.end());
                assert(itr->second < 12);

                return (mode == DEPTH)
                    ? ("mat[" + std::to_string(itr->second) + "]")
                    : ("vec4(0.0f, 0.0f, 0.0f, mat["
                            + std::to_string(itr->second) + "])");
            }
            // If the atom is already stored, save it
            else if (itr != atoms.end() && n != m)
            {
                return "m" + std::to_string(itr->second);
            }
            // Otherwise, the atom must be something hard-coded!
            else switch (n->op)
            {
                case OP_X:       return std::string("x");
                case OP_Y:       return std::string("y");
                case OP_Z:       return std::string("z");
                case OP_CONST:   return (mode == DEPTH)
                                 ? (std::to_string(n->value) + "f")
                                 : ("vec4(0.0f, 0.0f, 0.0f, "
                                         + std::to_string(n->value) + ")");
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
            case OP_CONST:
            case OP_MUTABLE: out += get(m); break;

            case LAST_OP:   // Fallthrough
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
            case OP_CONST:
            case OP_MUTABLE: out += get(m); break;

            case LAST_OP:
            case INVALID:   assert(false);
        }
    }
    return out + ";\n";
}

#include <cassert>

#include <glm/vec3.hpp>
#include <glm/gtx/string_cast.hpp>

#include "ao/gl/accelerator.hpp"
#include "ao/gl/shader.hpp"
#include "ao/gl/texture.hpp"

#include "ao/tree/atom.hpp"
#include "ao/tree/tree.hpp"
#include "ao/eval/evaluator.hpp"

#include "ao/render/region.hpp"

////////////////////////////////////////////////////////////////////////////////
// Vertex shader
const std::string Accelerator::vert = R"(
#version 330

// Vertex values are i, j, kmax, nk
layout (location=0) in ivec4 vert;

// min, max bounds of target region
uniform vec3 bounds[2];
uniform ivec3 dimensions;

// Outputs are fragment position [x, y, zfront, zback] and nk
out vec4 pos;
flat out int nk;

void main()
{
    // Fractions of the various positions in the global region
    float fx  = float(vert.x) / float(dimensions.x);
    float fy  = float(vert.y) / float(dimensions.y);
    float fz0 = float(vert.z) / float(dimensions.z);
    float fz1 = float(vert.z + vert.w) / float(dimensions.z);

    // Fragment position in region space
    pos = vec4(bounds[0].x * (1.0f - fx) + bounds[1].x * fx,
               bounds[0].y * (1.0f - fy) + bounds[1].y * fy,
               bounds[0].z * (1.0f - fz1) + bounds[1].z * fz1,
               bounds[0].z * (1.0f - fz0) + bounds[1].z * fz0);

    // Number of voxels for raymarching
    nk = vert.w;

    // Position in screen space
    gl_Position = vec4(2.0f * fx - 1.0f,
                       2.0f * fy - 1.0f,
                       2.0f * fz1 - 1.0f, 1.0f);
}
)";

const std::string Accelerator::frag = R"(
#version 330

// Raycast position [x, y, zfront, zback]
in vec4 pos;

// Number of voxels to walk through
flat in int nk;

// Our output buffer is the fragment's normal and depth
layout (location=0) out vec4 norm;

// Generic matrix transform
uniform float mat[12];

// Global Z render bounds
uniform vec3 bounds[2];

// Instruction tape and scratch memory area
#define MAX_CLAUSE 128
uniform ivec3 tape[MAX_CLAUSE];
vec4 mem[MAX_CLAUSE];
uniform int ops;

// Tape evaluation function for f-rep value
float f(float x, float y, float z)
{
    int i;
    for (i=0; i < ops; ++i)
    {
        // Find arguments (checking for immediate flags in opcode)
        float a = ((tape[i][0] & 0x0800) != 0) ? mat[tape[i][1]]
                : ((tape[i][0] & 0x0400) != 0) ? mem[tape[i][1]].w
                                               : intBitsToFloat(tape[i][1]);
        float b = ((tape[i][0] & 0x0200) != 0) ? mat[tape[i][2]]
                : ((tape[i][0] & 0x0100) != 0) ? mem[tape[i][2]].w
                                               : intBitsToFloat(tape[i][2]);

        switch(tape[i][0])
        {
            case 3:  mem[i].w = x; break;
            case 4:  mem[i].w = y; break;
            case 5:  mem[i].w = z; break;

            case 6:  mem[i].w = sqrt(a); break;
            case 7:  mem[i].w = -a; break;
            case 8:  mem[i].w = abs(a); break;

            case 9:  mem[i].w = a + b; break;
            case 10: mem[i].w = a * b; break;
            case 11: mem[i].w = min(a, b); break;
            case 12: mem[i].w = max(a, b); break;
            case 13: mem[i].w = a - b; break;
            case 14: mem[i].w = a / b; break;
        }
    }
    return mem[i - 1].w;
}

// Tape evaluation function for f-rep derivatives
vec4 g(vec4 x, vec4 y, vec4 z)
{
    return mem[10];
}

void main()
{
    float x = pos.x;
    float y = pos.y;

    // Set the default depth to the back value
    gl_FragDepth = 1.0f;
    norm = vec4(0.0f);

    for (int i=0; i < nk; ++i)
    {
        // Fraction along which we've marched
        float frac = (i + 0.5f) / float(nk);

        // Coordinate in region space
        float z = pos.z * (1 - frac) + pos.w * frac;

        if (f(x, y, z) < 0.0f)
        {
            // Map the z coordinate to the 0 - 1 depth buffer
            gl_FragDepth = (bounds[1].z == bounds[0].z)
                ? 0.5f : (bounds[1].z - z) / (bounds[1].z - bounds[0].z);

            // Calculate normal
            vec4 n = g(vec4(1.0f, 0.0f, 0.0f, x),
                       vec4(0.0f, 1.0f, 0.0f, y),
                       vec4(0.0f, 0.0f, 1.0f, z));

            // Pack normal into 0-255 range, clipping if we're at the
            // top of the Z range
            if (i == 0 && pos.w == bounds[1].z)
            {
                norm = vec4(0.5f, 0.5f, 1.0f, 1.0f);
            }
            else
            {
                norm = vec4(normalize(n.xyz) / 2.0f + 0.5f, 1.0f);
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

vec4 abs_g(vec4 a)
{
    if (a.w < 0)
    {
        return -a;
    }
    else
    {
        return a;
    }
}
)";

////////////////////////////////////////////////////////////////////////////////

Accelerator::Accelerator()
    : mat({{1, 0, 0, 0,
            0, 1, 0, 0,
            0, 0, 1, 0}})
{
    vs = Shader::compile(vert, GL_VERTEX_SHADER);
    fs = Shader::compile(frag, GL_FRAGMENT_SHADER);
    prog = Shader::link(vs, fs);

    assert(vs);
    assert(fs);
    assert(prog);

    glGenBuffers(1, &vbo);
    glGenVertexArrays(1, &vao);

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
    // Assert that we're being initialized with a top-level Region
    assert(r.X.min == 0 && r.Y.min == 0 && r.Z.min == 0);

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

    // Clear the buffers
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    // Select the active program
    glUseProgram(prog);

    // Bind the VAO
    glBindVertexArray(vao);

    // Load the global voxel dimensions
    glUniform3i(glGetUniformLocation(prog, "dimensions"),
                r.X.size, r.Y.size, r.Z.size);

    // Load the global bounds
    glm::vec3 bounds[2] = {glm::vec3(r.X.lower(), r.Y.lower(), r.Z.lower()),
                           glm::vec3(r.X.upper(), r.Y.upper(), r.Z.upper())};

    glUniform3fv(glGetUniformLocation(prog, "bounds"), 2,
                 reinterpret_cast<GLfloat*>(bounds));

    // Load the generic transform matrix into the shader
    glUniform1fv(glGetUniformLocation(prog, "mat"), 12, &mat[0]);

    // Set the viewport to the appropriate size
    glViewport(r.X.min, r.Y.min, r.X.size, r.Y.size);
}

void Accelerator::flush(Tree* tree)
{
    Evaluator e(tree);
    flush(&e);
}

void Accelerator::flush(Evaluator* eval)
{
    // Generate and bind a simple quad shape
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBufferData(GL_ARRAY_BUFFER, data.size() * sizeof(data[0]),
                 &data[0], GL_STREAM_DRAW);

    // Set up pointer to vertex attributes
    glVertexAttribIPointer(0, 4, GL_INT, 0, (GLvoid*)0);
    glEnableVertexAttribArray(0);

    // Load our instruction tape and tape length into the shader
    populateTape(eval);
    size_t i=0;
    for (auto t : tape)
        std::cout << i++ << ": " << std::hex << t.x <<' ' <<  t.y << ' ' << t.z << '\n';
    //tape.clear();
    //ktape.push_back(glm::ivec3(3, 0, 0));
    glUniform3iv(glGetUniformLocation(prog, "tape"), tape.size(),
                 reinterpret_cast<GLint*>(&tape[0]));
    glUniform1i(glGetUniformLocation(prog, "ops"), tape.size());

    // Assert that we're drawing quads (2 triangles x 3 points)
    assert(data.size() % 6 == 0);

    // Actually draw the quads
    glDrawArrays(GL_TRIANGLES, 0, data.size());

    data.clear();
}

void Accelerator::populateTape(Evaluator* eval)
{
    tape.clear();

    // Store the indices of clauses in the shader tape
    std::unordered_map<const Clause*, GLint> clauses;
    size_t index = 0;

    // Hard-code the matrix clauses as numbers 0 through 11
    // in a separately-indexed array named 'mat'
    for (int i=0; i < 12; ++i)
    {
        assert(eval->matrix[i]->op == OP_MUTABLE);
        clauses[eval->matrix[i]] = i;
    }

    auto get = [&](const Clause* c){
        // If the clause is null, don't return anything
        if (c == nullptr)
        {
            return std::pair<GLint, uint8_t>(0, 0);
        }
        // Special-case for mutable atoms, which are assumed to be part
        // of the generic transform matrix and are GLSL uniforms
        else if (c->op == OP_MUTABLE)
        {
            assert(clauses.find(c) != clauses.end());
            assert(clauses[c] < 12);

            return std::pair<GLint, uint8_t>(clauses[c], 2);
        }
        // Store the clause's value if it is disabled
        else if (c->flags & CLAUSE_FLAG_DISABLED)
        {
            float v = c->mutable_value;
            return std::pair<GLint, uint8_t>(
                    *reinterpret_cast<GLint*>(&v), 0);
        }
        else if (c->op == OP_CONST)
        {
            float v = c->value;
            return std::pair<GLint, uint8_t>(
                    *reinterpret_cast<GLint*>(&v), 0);
        }
        // Otherwise, add the clause to the memory array if not present
        else if (clauses.find(c) == clauses.end())
        {
            clauses[c] = index++;
        }
        // Return a reference to the generic memory array
        return std::pair<GLint, uint8_t>(clauses[c], 1);
    };

    auto push = [&](Clause* c)
    {
        auto a = get(c->a);
        auto b = get(c->b);

        GLint op = static_cast<GLint>(c->op);
        op |= b.second << 8;
        op |= a.second << 10;

        tape.push_back(glm::ivec3(op, a.first, b.first));
    };

    push(eval->X);
    push(eval->Y);
    push(eval->Z);

    for (const auto& row : eval->rows)
    {
        for (size_t i=0; i < row.active; ++i)
        {
            if (row[i] != eval->root)
            {
                push(row[i]);
            }
        }
    }
    std::cout << tape.size() << "\n";
    push(eval->root);
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

std::pair<DepthImage, NormalImage> Accelerator::Render(Tree* t, const Region& r)
{
    GLuint depth, norm;
    glGenTextures(1, &depth);
    glGenTextures(1, &norm);

    init(r, depth, norm);
    RenderSubregion(r);
    flush(t);

    auto out = std::make_pair(fromDepthTexture(depth, r),
                              fromNormalTexture(norm, r));

    glDeleteTextures(1, &depth);
    glDeleteTextures(1, &norm);

    return out;
}

void Accelerator::RenderSubregion(const Region& r)
{
    data.push_back(glm::ivec4(r.X.min, r.Y.min, r.Z.min, r.Z.size));
    data.push_back(glm::ivec4(r.X.min + r.X.size, r.Y.min, r.Z.min, r.Z.size));
    data.push_back(glm::ivec4(r.X.min, r.Y.min + r.Y.size, r.Z.min, r.Z.size));

    data.push_back(glm::ivec4(r.X.min, r.Y.min + r.Y.size, r.Z.min, r.Z.size));
    data.push_back(glm::ivec4(r.X.min + r.X.size,
                              r.Y.min + r.Y.size, r.Z.min, r.Z.size));
    data.push_back(glm::ivec4(r.X.min + r.X.size, r.Y.min, r.Z.min, r.Z.size));
}

void Accelerator::FillSubregion(const Region& r)
{
    const size_t z = r.Z.min + r.Z.size - 1;

    data.push_back(glm::ivec4(r.X.min, r.Y.min, z, 1));
    data.push_back(glm::ivec4(r.X.min + r.X.size - 1, r.Y.min, z, 1));
    data.push_back(glm::ivec4(r.X.min, r.Y.min + r.Y.size - 1, z, 1));

    data.push_back(glm::ivec4(r.X.min, r.Y.min + r.Y.size - 1, z, 1));
    data.push_back(glm::ivec4(r.X.min + r.X.size - 1, r.Y.min, z, 1));
    data.push_back(glm::ivec4(r.X.min + r.X.size - 1,
                              r.Y.min + r.Y.size - 1, z, 1));
}

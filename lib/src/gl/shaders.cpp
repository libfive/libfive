#include <iostream>
#include <cassert>

#include "ao/gl/shaders.hpp"

////////////////////////////////////////////////////////////////////////////////

std::string base_vert = R"EOF(
#version 330

layout(location=0) in vec3 vertex_position;

void main()
{
    gl_Position = vec4(vertex_position, 1.0f);
}
)EOF";

////////////////////////////////////////////////////////////////////////////////

std::string base_frag = R"EOF(
#version 330

out vec4 fragColor;

void main()
{
    fragColor = vec4(1.0f, 0.0f, 0.0f, 1.0f);
}
)EOF";

////////////////////////////////////////////////////////////////////////////////

/*
 *  Compile a single shader of the given type
 *  Returns the shader's value on success, 0 on failure
 */
static GLuint compile(std::string s, GLenum type)
{
    assert(type == GL_VERTEX_SHADER || type == GL_FRAGMENT_SHADER);

    GLuint shader = glCreateShader(type);

    const char* txt = s.c_str();
    glShaderSource(shader, 1, &txt, NULL);
    glCompileShader(shader);

    GLint status;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
    if (status == GL_FALSE)
    {
        GLint log_length;
        glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &log_length);

        GLchar* info_log = new GLchar[log_length + 1];
        glGetShaderInfoLog(shader, log_length, NULL, info_log);

        std::cerr << "Compile failure in "
                  << (type == GL_VERTEX_SHADER ? "vertex" : "fragment")
                  << " shader:\n" << info_log << std::endl;
        delete [] info_log;
        return 0;
    }
    return shader;
}

static GLuint link(const GLuint vert, const GLuint frag)
{
    GLuint program = glCreateProgram();
    glAttachShader(program, vert);
    glAttachShader(program, frag);
    glLinkProgram(program);

    GLint status;
    glGetProgramiv(program, GL_LINK_STATUS, &status);
    if (status == GL_FALSE)
    {
        GLint infoLogLength;
        glGetProgramiv(program, GL_INFO_LOG_LENGTH, &infoLogLength);

        GLchar* strInfoLog = new GLchar[infoLogLength + 1];
        glGetProgramInfoLog(program, infoLogLength, NULL, strInfoLog);

        std::cerr << "Linker failure: " << strInfoLog << std::endl;
        delete [] strInfoLog;
    }

    return program;
}

////////////////////////////////////////////////////////////////////////////////

Shaders::Shaders()
{
    auto vert = compile(base_vert, GL_VERTEX_SHADER);
    auto frag = compile(base_frag, GL_FRAGMENT_SHADER);
    prog = link(vert, frag);
}

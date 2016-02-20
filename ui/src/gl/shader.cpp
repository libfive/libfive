/*
 *  Copyright (C) 2016 Matthew Keeter
 *
 *  This file is part of Ao.
 *
 *  Ao is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <iostream>
#include <cassert>

#include "ao/ui/gl/shader.hpp"

// Tricky macro to print a shader or program error
#define CHECK_ERROR(GET, INFO, DEL, STATUS, TARGET, CONTEXT) \
{                                                               \
    GLint status;                                               \
    GET(TARGET, STATUS, &status);                               \
    if (status == GL_FALSE)                                     \
    {                                                           \
        GLint log_length;                                       \
        GET(TARGET, GL_INFO_LOG_LENGTH, &log_length);           \
                                                                \
        GLchar* info_log = new GLchar[log_length + 1];          \
        INFO(TARGET, log_length, NULL, info_log);               \
                                                                \
        std::cerr << "Compile failure (" << (CONTEXT) << "):\n" \
                  << info_log << std::endl;                     \
        delete [] info_log;                                     \
                                                                \
        DEL(TARGET);                                            \
        return 0;                                               \
    }                                                           \
}

namespace Shader
{

/*
 *  Compile a single shader of the given type
 *  Returns the shader's value on success, 0 on failure
 */
GLuint compile(std::string s, GLenum type)
{
    assert(type == GL_VERTEX_SHADER || type == GL_FRAGMENT_SHADER);

    GLuint shader = glCreateShader(type);

    const char* txt = s.c_str();
    glShaderSource(shader, 1, &txt, NULL);
    glCompileShader(shader);

    CHECK_ERROR(glGetShaderiv, glGetShaderInfoLog, glDeleteShader,
                GL_COMPILE_STATUS, shader, (type == GL_VERTEX_SHADER)
                                            ? "vertex" : "fragment");
    return shader;
}

GLuint link(const GLuint vert, const GLuint frag)
{
    GLuint program = glCreateProgram();
    glAttachShader(program, vert);
    glAttachShader(program, frag);
    glLinkProgram(program);

    CHECK_ERROR(glGetProgramiv, glGetProgramInfoLog, glDeleteProgram,
                GL_LINK_STATUS, program, "link");
    return program;
}

} // namespace Shader

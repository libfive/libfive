#pragma once

#include <string>

#include "ao/ui/gl/core.hpp"

namespace Shader
{
/*
 *  Compile a single shader of the given type
 *
 *  type must be GL_VERTEX_SHADER or GL_FRAGMENT_SHADER
 *
 *  Returns the shader's value on success, 0 on failure
 *  On failure, prints the compile error to stderr
 */
GLuint compile(std::string s, GLenum type);

/*
 *  Links multiple shaders into a new program
 *
 *  Returns the program's value on success, 0 on failure
 *  On failure, prints the link error to stderr
 */
GLuint link(GLuint vert, GLuint frag);

} // namespace Shader

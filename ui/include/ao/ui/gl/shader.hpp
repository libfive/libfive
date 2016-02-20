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

/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#pragma once

#include <glm/mat4x4.hpp>

#include "ao/ui/gl/core.hpp"

class Axes
{
public:
    Axes();
    ~Axes();

    void draw(const glm::mat4& m) const;

protected:
    GLuint vs;  // Vertex shader
    GLuint fs;  // Fragment shader
    GLuint prog;    // Shader program

    GLuint vbo_solid; // Vertex buffer for solid axes
    GLuint vao_solid; // Vertex array for drawing shaded

    GLuint vbo_wire;  // Vertex buffer for wireframe axes
    GLuint vao_wire;  // Vertex array for drawing wireframe

    // Shader source strings
    static const std::string vert;
    static const std::string frag;
};

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
#include <memory>

#include <catch/catch.hpp>

#include "ao/ui/gl/core.hpp"
#include "ao/ui/gl/shader.hpp"

////////////////////////////////////////////////////////////////////////////////

typedef std::unique_ptr<GLFWwindow, std::function<void(GLFWwindow*)>> WindowPtr;

TEST_CASE("Context creation")
{
    WindowPtr window(makeContext(), glfwDestroyWindow);
    REQUIRE(window != nullptr);
}

TEST_CASE("OpenGL version")
{
    WindowPtr window(makeContext(), glfwDestroyWindow);

    auto version = glGetString(GL_VERSION);
    REQUIRE(version);

    int major = version[0] - '0';
    int minor = version[2] - '0';

    CAPTURE(version);
    REQUIRE((major * 10 + minor) >= 33);
}

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
#include "ao/ui/gl/core.hpp"

////////////////////////////////////////////////////////////////////////////////

static bool glfw_initialized = false;

// Attemp to call glfwInit() if the library is not already initialized
// Returns true on success, otherwise false.
static bool initialize()
{
    if (glfw_initialized || glfwInit() == GL_TRUE)
    {
        glfw_initialized = true;
    }
    return glfw_initialized;
}

////////////////////////////////////////////////////////////////////////////////

static void setWindowHints()
{
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, GL_VERSION_MAJOR);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, GL_VERSION_MINOR);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
}

////////////////////////////////////////////////////////////////////////////////

GLFWwindow* makeWindow(int width, int height, std::string title)
{
    if (!initialize())
    {
        return nullptr;
    }

    glfwDefaultWindowHints();
    setWindowHints();
    glfwWindowHint(GLFW_FLOATING, GL_TRUE);
    glfwWindowHint(GLFW_FOCUSED, GL_FALSE);
    //glfwWindowHint(GLFW_DECORATED, GL_FALSE);

    auto out = glfwCreateWindow(width, height, title.c_str(), nullptr, nullptr);
    glfwMakeContextCurrent(out);

    return out;
}

GLFWwindow* makeContext()
{
    if (!initialize())
    {
        return nullptr;
    }

    setWindowHints();
    glfwWindowHint(GLFW_VISIBLE, GL_FALSE);

    auto out = glfwCreateWindow(1, 1, "", nullptr, nullptr);
    glfwMakeContextCurrent(out);

    return out;
}

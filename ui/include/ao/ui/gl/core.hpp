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

#define GL_VERSION_MAJOR 3
#define GL_VERSION_MINOR 3

#include <mutex>
#include <string>

#include <epoxy/gl.h>
#include <GLFW/glfw3.h>

/*
 *  Creates a visible window and returns a pointer
 *  (or nullptr if something failed)
 *
 *  glfwInitialize is called if not already initialized.
 *  The returned context is set to current.
 *  Caller takes ownership of window.
 */
GLFWwindow* makeWindow(int width, int height, std::string title);

/*
 *  Creates an OpenGL context attached to a hidden window,
 *  returning the window (or nullptr if something failed)
 *
 *  Same caveats apply as for makeWindow
 */
GLFWwindow* makeContext();

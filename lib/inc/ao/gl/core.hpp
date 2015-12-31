#pragma once

#define GL_VERSION_MAJOR 3
#define GL_VERSION_MINOR 3

/*
 *  If USE_ACCELERATOR is set to 1, Frames will construct an Accelerator
 *  object to speed up rendering of Trees.
 */
#define USE_ACCELERATOR 1

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

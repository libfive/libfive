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

/*  Mutex used to ensure that only one thread does OpenGL stuff at a time  */
namespace gl
{
extern std::mutex mutex;
}

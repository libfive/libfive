#pragma once

#define GL_VERSION_MAJOR 3
#define GL_VERSION_MINOR 3

#include <epoxy/gl.h>
#include <GLFW/glfw3.h>
#include <oglplus/all.hpp>

/*
 *  Creates a visible window and returns a pointer
 *  (or nullptr if something failed)
 */
GLFWwindow* makeWindow(int width, int height, std::string title);

/*
 *  Creates an OpenGL context attached to a hidden window,
 *  returning the window (or nullptr if something failed)
 */
GLFWwindow* makeContext();

/*
 *  Destroys the window (if not a nullptr) and calls glfwTerminate
 */
void finish(GLFWwindow* window);

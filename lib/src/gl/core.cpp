#include "ao/gl/core.hpp"

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

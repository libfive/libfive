#include "ao/gl/core.hpp"

static void setWindowHints()
{
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, GL_VERSION_MAJOR);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, GL_VERSION_MINOR);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
}

GLFWwindow* makeWindow(int width, int height, std::string title)
{
    if (!glfwInit())
    {
        return nullptr;
    }

    glfwDefaultWindowHints();
    setWindowHints();

    auto out = glfwCreateWindow(width, height, title.c_str(), nullptr, nullptr);
    glfwMakeContextCurrent(out);

    return out;
}

GLFWwindow* makeContext()
{
    if (!glfwInit())
    {
        return nullptr;
    }

    setWindowHints();
    glfwWindowHint(GLFW_VISIBLE, GL_FALSE);    // multisampling!

    auto out = glfwCreateWindow(1, 1, "", nullptr, nullptr);
    glfwMakeContextCurrent(out);

    return out;
}

void finish(GLFWwindow* window)
{
    if (window)
    {
        glfwDestroyWindow(window);
    }
    glfwTerminate();
}

#include <iostream>

#include <glm/gtc/matrix_transform.hpp>

#include "ao/ui/window.hpp"
#include "ao/core/tree.hpp"

#include "ao/gl/core.hpp"
#include "ao/gl/frame.hpp"

////////////////////////////////////////////////////////////////////////////////

bool Window::Show(Tree* tree)
{
    (void)tree;

    auto glfw_window = makeWindow(640, 480, "Hello!");
    if (glfw_window == nullptr)
    {
        return false;
    }
    std::unique_ptr<Window> win(new Window(glfw_window));
    win->run();

    return true;
}

////////////////////////////////////////////////////////////////////////////////

// Callback wrappers
static void _resized(GLFWwindow* window, int w, int h)
{
    static_cast<Window*>(glfwGetWindowUserPointer(window))->resized(w, h);
}

////////////////////////////////////////////////////////////////////////////////

Window::Window(GLFWwindow* window)
    : window(window)
{
    glfwGetFramebufferSize(window, &width, &height);
    glfwSetWindowUserPointer(window, this);
    glfwSetFramebufferSizeCallback(window, _resized);
}

void Window::resized(int w, int h)
{
    width = w;
    height = h;
    draw();
}

glm::mat4 Window::proj() const
{
    glm::mat4 m;
    if (width > height)
    {
        m = glm::scale(m, {height/float(width), 1.0, 1.0});
    }
    else
    {
        m = glm::scale(m, {1.0, width/float(height), 1.0});
    }
    return m;
}

void Window::draw() const
{
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    glClear(GL_COLOR_BUFFER_BIT);
    axes.draw(proj());

    glfwSwapBuffers(window);
}

void Window::run()
{
    while (!glfwWindowShouldClose(window))
    {
        // Poll for and process events
        glfwPollEvents();

        // Redraw the window
        draw();
    }
}

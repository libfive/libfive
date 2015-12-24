#include <iostream>

#include "ao/ui/window.hpp"
#include "ao/core/tree.hpp"

#include "ao/gl/core.hpp"
#include "ao/gl/shaders.hpp"
#include "ao/gl/frame.hpp"

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

Window::Window(GLFWwindow* window)
    : window(window)
{
    // Nothing to do here
}

void Window::run()
{
    while (!glfwWindowShouldClose(window))
    {
        glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
        glClear(GL_COLOR_BUFFER_BIT);

        shaders.Use();
        frame.Draw();

        // Swap front and back buffers
        glfwSwapBuffers(window);

        // Poll for and process events
        glfwPollEvents();
    }

    glfwTerminate();
}

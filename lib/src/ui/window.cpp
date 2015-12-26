#include <iostream>

#include <glm/gtx/transform.hpp>
#include <glm/gtx/string_cast.hpp>

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
    std::unique_ptr<Window> win(new Window(tree, glfw_window));
    win->run();

    return true;
}

////////////////////////////////////////////////////////////////////////////////

// Callback wrappers
static void _resized(GLFWwindow* window, int w, int h)
{
    static_cast<Window*>(glfwGetWindowUserPointer(window))->resized(w, h);
}

static void _mouseMove(GLFWwindow* window, double x, double y)
{
    static_cast<Window*>(glfwGetWindowUserPointer(window))->mouseMove(x, y);
}

static void _mouseButton(GLFWwindow* window, int b, int a, int m)
{
    static_cast<Window*>(glfwGetWindowUserPointer(window))->mouseButton(b, a, m);
}

static void _mouseScroll(GLFWwindow* window, double sx, double sy)
{
    static_cast<Window*>(glfwGetWindowUserPointer(window))->mouseScroll(sx, sy);
}

////////////////////////////////////////////////////////////////////////////////

Window::Window(Tree* tree, GLFWwindow* window)
    : window(window), frames()
{
    if (tree != nullptr)
    {
        frames.push_back(new Frame(tree));
    }

    glfwGetFramebufferSize(window, &width, &height);
    glfwSetWindowUserPointer(window, this);

    glfwSetFramebufferSizeCallback(window, _resized);
    glfwSetCursorPosCallback(window, _mouseMove);
    glfwSetMouseButtonCallback(window, _mouseButton);
    glfwSetScrollCallback(window, _mouseScroll);

    render();
}

Window::~Window()
{
    for (auto f : frames)
    {
        delete f;
    }
}

void Window::resized(int w, int h)
{
    width = w;
    height = h;
    draw();
}

////////////////////////////////////////////////////////////////////////////////

void Window::mouseMove(double x, double y)
{
    int w;
    int h;
    glfwGetWindowSize(window, &w, &h);

    // Scale coordinates to the range -1, 1
    auto new_pos = glm::vec2((2*x)/w - 1, 1 - (2*y)/h);

    // If we're panning, adjust the center position
    if (drag_mode == WINDOW_DRAG_PAN)
    {
        // Find the starting position in world coordinates
        auto inv = glm::inverse(M());
        auto diff = inv * glm::vec4(new_pos, 0.0, 1.0) -
                    inv * glm::vec4(mouse_pos, 0.0, 1.0);

        center += glm::vec3(diff.x, diff.y, diff.z);
        render();
    }
    else if (drag_mode == WINDOW_DRAG_ROTATE)
    {
        roll += mouse_pos.x - new_pos.x;
        pitch += new_pos.y - mouse_pos.y;
        render();
    }

    mouse_pos = new_pos;

    if (drag_mode != WINDOW_DRAG_NONE)
    {
        draw();
    }
}

void Window::mouseButton(int button, int action, int mods)
{
    (void)mods;

    if (action == GLFW_PRESS)
    {
        if (button == GLFW_MOUSE_BUTTON_1)
        {
            drag_mode = WINDOW_DRAG_PAN;
        }
        else if (button == GLFW_MOUSE_BUTTON_2)
        {
            drag_mode = WINDOW_DRAG_ROTATE;
        }
    }
    else
    {
        drag_mode = WINDOW_DRAG_NONE;
    }
}

void Window::mouseScroll(double sx, double sy)
{
    (void)sx; /* unused */

    const float delta = 1.1;
    scale *= sy < 0 ? delta : 1/delta;

    render();
    draw();
}

////////////////////////////////////////////////////////////////////////////////

void Window::render()
{
    for (auto f : frames)
    {
        f->render(M(), width, height);
    }
}

////////////////////////////////////////////////////////////////////////////////

glm::mat4 Window::proj() const
{
    if (width > height)
    {
        const float frac = height/float(width);
        return glm::scale(glm::vec3(frac, 1.0, frac));
    }
    else
    {
        const float frac = width/float(height);
        return glm::scale(glm::vec3(1.0, frac, frac));
    }
}

glm::mat4 Window::view() const
{
    glm::mat4 m = glm::scale(glm::vec3(scale, scale, scale));;
    m = glm::rotate(m, pitch,  {1.0, 0.0, 0.0});
    m = glm::rotate(m, roll, {0.0, 1.0, 0.0});
    m = glm::translate(m, center);

    return m;
}

////////////////////////////////////////////////////////////////////////////////

void Window::draw() const
{
    auto m = M();

    glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    glClearDepth(1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    axes.draw(m);
    for (auto f : frames)
    {
        f->draw(m);
    }

    glfwSwapBuffers(window);
}

void Window::run()
{
    // Draw the initial frame (other draw calls are triggered as-needed)
    draw();

    while (!glfwWindowShouldClose(window))
    {
        // Poll for and process events
        glfwPollEvents();
    }
}

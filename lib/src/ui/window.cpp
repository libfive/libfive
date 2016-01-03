#include <iostream>

#include <glm/gtx/transform.hpp>
#include <glm/gtx/string_cast.hpp>

#include "ao/ui/window.hpp"
#include "ao/tree/tree.hpp"

#include "ao/gl/core.hpp"
#include "ao/gl/frame.hpp"

Window* Window::singleton = nullptr;
Window* Window::instance()
{
    if (!singleton)
    {
        singleton = new Window();
    }
    return singleton;
}

////////////////////////////////////////////////////////////////////////////////

Window::Window()
    : window(makeWindow(640, 480, "Hello!")), incoming(nullptr), clear(false)
{
    assert(window != nullptr);

    glfwGetFramebufferSize(window, &width, &height);
    glfwSetWindowUserPointer(window, this);

    glfwSetFramebufferSizeCallback(window, _resized);
    glfwSetCursorPosCallback(window, _mouseMove);
    glfwSetMouseButtonCallback(window, _mouseButton);
    glfwSetScrollCallback(window, _mouseScroll);
}

Window::~Window()
{
    for (auto f : frames)
    {
        delete f.second;
    }
    glfwDestroyWindow(window);
}

void Window::resized(int w, int h)
{
    width = w;
    height = h;
    draw();
}

void Window::addTree(std::string name, Tree* t)
{
    auto ptr = new std::pair<std::string, Tree*>(name, t);

    // Loop waiting for the incoming tree to be claimed
    while (incoming.load() != nullptr);

    incoming.store(ptr);
    glfwPostEmptyEvent();
}

void Window::clearFrames()
{
    clear.store(true);
    glfwPostEmptyEvent();
}

////////////////////////////////////////////////////////////////////////////////

glm::vec2 Window::scaledMousePos(double x, double y) const
{
    int w;
    int h;
    glfwGetWindowSize(window, &w, &h);

    return glm::vec2((2*x)/w - 1, 1 - (2*y)/h);
}

void Window::mouseMove(double x, double y)
{
    // Scale coordinates to the range -1, 1
    auto new_pos = scaledMousePos(x, y);

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
        roll -= mouse_pos.x - new_pos.x;
        pitch -= new_pos.y - mouse_pos.y;
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

    double x;
    double y;
    glfwGetCursorPos(window, &x, &y);
    mouse_pos = scaledMousePos(x, y);
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
        f.second->render(M(), width, height, (width+height) / 2);
    }
}

////////////////////////////////////////////////////////////////////////////////

glm::mat4 Window::proj() const
{
    if (width > height)
    {
        const float frac = height/float(width);
        return glm::scale(glm::vec3(frac, 1.0, -frac));
    }
    else
    {
        const float frac = width/float(height);
        return glm::scale(glm::vec3(1.0, frac, -frac));
    }
}

glm::mat4 Window::view() const
{
    glm::mat4 m = glm::scale(glm::vec3(scale, scale, scale));;
    m = glm::rotate(m, pitch,  {1.0, 0.0, 0.0});
    m = glm::rotate(m, roll, {0.0, 0.0, 1.0});
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
    glEnable(GL_DEPTH_TEST);

    for (auto f : frames)
    {
        f.second->poll();
        f.second->draw(m);
    }
    axes.draw(m);

    glfwSwapBuffers(window);
}

void Window::run()
{
    while (!glfwWindowShouldClose(window))
    {
        poll();
    }
}

void Window::poll()
{
    // Flag used to detect if a redraw is needed
    bool needs_draw = false;

    // Block until a GLFW event occurs
    glfwWaitEvents();

    // Grab an incoming tree from the atomic pointer
    std::pair<std::string, Tree*>* in = incoming.exchange(nullptr);
    if (in)
    {
        // If the name is already claimed, replace it
        if (frames.count(in->first) != 0)
        {
            stale.push_back(frames[in->first]);
        }

        // Construct a new frame
        frames[in->first] = new Frame(in->second);

        // Delete the string, Tree* pair (which was allocated on the heap)
        delete in;

        // Kick off a render operation for the new tree
        render();

        // Mark that a redraw is needed
        needs_draw = true;
    }

    // Poll for render tasks that have finished
    for (auto f : frames)
    {
        needs_draw |= f.second->poll();
    }

    // If the clear flag is set, mark all frames as stale
    if (clear.load())
    {
        clear.store(false);
        for (auto f : frames)
        {
            stale.push_back(f.second);
        }
        frames.clear();
        needs_draw = true;
    }

    if (needs_draw)
    {
        draw();
    }

    // Check all stale frames to see if they can be pruned
    auto itr = stale.begin();
    while (itr != stale.end())
    {
        if (!(*itr)->running())
        {
            delete *itr;
            itr = stale.erase(itr);
        }
        else
        {
            itr++;
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

// Static callback wrappers
void Window::_resized(GLFWwindow* window, int w, int h)
{
    static_cast<Window*>(glfwGetWindowUserPointer(window))->resized(w, h);
}

void Window::_mouseMove(GLFWwindow* window, double x, double y)
{
    static_cast<Window*>(glfwGetWindowUserPointer(window))->mouseMove(x, y);
}

void Window::_mouseButton(GLFWwindow* window, int b, int a, int m)
{
    static_cast<Window*>(glfwGetWindowUserPointer(window))->mouseButton(b, a, m);
}

void Window::_mouseScroll(GLFWwindow* window, double sx, double sy)
{
    static_cast<Window*>(glfwGetWindowUserPointer(window))->mouseScroll(sx, sy);
}

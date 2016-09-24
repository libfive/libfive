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
#include <glm/gtx/transform.hpp>
#include <glm/gtx/string_cast.hpp>

#include "ao/ui/window.hpp"
#include "ao/ui/gl/core.hpp"
#include "ao/ui/gl/frame.hpp"

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
    : window(makeWindow(640, 480, "Ao")), incoming(nullptr),
      clear(false), halt(false)
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
        for (auto g : f.second)
        {
            delete g.second;
        }
    }
    glfwDestroyWindow(window);
}

void Window::resized(int w, int h)
{
    width = w;
    height = h;
    draw();
}

void Window::addTree(std::string filename, std::string name, Token t)
{
    auto ptr = new std::tuple<std::string, std::string, Token>(
            filename, name, t);

    // Loop waiting for the incoming tree to be claimed
    while (incoming.load() != nullptr);

    incoming.store(ptr);
    glfwPostEmptyEvent();
}

void Window::quit()
{
    halt.store(true);
    glfwPostEmptyEvent();
}

void Window::clearFrames()
{
    clear.store(true);
    glfwPostEmptyEvent();
}

void Window::clearFile(std::string filename)
{
    for (auto f : frames[filename])
    {
        stale.push_back(f.second);
        redraw.store(true);
        glfwPostEmptyEvent();
    }
    frames[filename].clear();
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
        for (auto g : f.second)
        {
            g.second->render(M(), width, height, (width+height) / 2);
        }
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
        for (auto g : f.second)
        {
            g.second->poll();
            g.second->draw(m);
        }
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

bool Window::loadFrame()
{
    // Grab an incoming tree from the atomic pointer
    if (auto in = incoming.exchange(nullptr))
    {
        auto filename = std::get<0>(*in);
        auto partname = std::get<1>(*in);

        // If the name is already claimed, replace it
        if (frames.count(filename) != 0 &&
            frames[filename].count(partname) != 0)
        {
            stale.push_back(frames[filename][partname]);
        }

        // Construct a new frame
        frames[filename][partname] = new Frame(std::get<2>(*in));

        // Delete the key, Tree* pair (which was allocated on the heap)
        delete in;

        // Kick off a render operation for the new tree
        render();

        // Mark that a redraw is needed
        return true;
    }
    return false;
}

bool Window::pollFrames()
{
    bool out = false;

    for (auto f : frames)
    {
        for (auto g : f.second)
        {
            out |= g.second->poll();
        }
    }

    return out;
}

bool Window::checkClear()
{
    if (clear.load())
    {
        clear.store(false);
        for (auto f : frames)
        {
            for (auto g : f.second)
            {
                stale.push_back(g.second);
            }
        }
        frames.clear();
        return true;
    }
    return false;
}

void Window::poll()
{
    // Block until a GLFW event occurs
    glfwWaitEvents();

    if (halt.load())
    {
        glfwSetWindowShouldClose(window, GL_TRUE);
    }

    // Flag used to detect if a redraw is needed
    bool needs_draw = redraw.exchange(false);

    // Load incoming frames
    needs_draw |= loadFrame();

    // Poll for render tasks that have finished
    needs_draw |= pollFrames();

    // If the clear flag is set, mark all frames as stale
    needs_draw |= checkClear();

    if (needs_draw)
    {
        draw();
    }

    // Check all stale frames to see if they can be pruned
    pruneStale();
}

void Window::pruneStale()
{
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

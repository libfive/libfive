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
#pragma once

#include <memory>
#include <map>
#include <cmath>

#include <glm/mat4x4.hpp>
#include <glm/vec3.hpp>
#include <glm/vec2.hpp>

#include "ao/ui/gl/core.hpp"
#include "ao/ui/gl/frame.hpp"
#include "ao/ui/gl/axes.hpp"

class Tree;

class Window
{
public:
    /*
     *  We should only have one window constructed at a time
     */
    static Window* singleton;
    static Window* instance();

    /*
     *  Window destructor deletes all associated Frames
     */
    ~Window();

    /*
     *  Adds a Frame to redraw the given shape asynchronously
     *  (will not take effect until the next poll() call)
     */
    void addTree(std::string filename, std::string name, Tree* t);

    /*
     *  Instructs the window to quit asynchronously
     *  (will not take effect until the next poll() call)
     */
    void quit();

    /*
     *  Clears existing frames asynchronously
     *  (will not take effect until the next poll() call)
     */
    void clearFrames();

    /*
     *  Clears frames associated with the given filename
     *  Executed synchronously (so must be called from main thread)
     */
    void clearFile(std::string filename);

    /*
     *  Runs the window forever
     */
    void run();

    /*
     *  Polls for window events
     */
    void poll();

    /*
     *  Redraw the window with the current state
     */
    void draw() const;

protected:
    /*
     *  Default Window constructor
     *
     *  Triggers an assertion failure on OpenGL errors
     */
    explicit Window();

    /*
     *  Check if the incoming pointer is populated and create a Frame if so
     *
     *  Returns true if the window should be redrawn.
     */
    bool loadFrame();

    /*
     *  Iterate over all frames, checking their render status.
     *
     *  Returns true if the window should be redrawn.
     */
    bool pollFrames();

    /*
     *  Checks if the clear flag is set, clearing frames if it is
     *  (by moving them to the stale list)
     *
     *  Returns true if the window should be redrawn.
     */
    bool checkClear();

    /*
     *  Checks the frames in the stale list, deleting those that are done
     */
    void pruneStale();

    /*
     *  Request that every child Frame render itself
     */
    void render();

    /*
     *  Projection matrix
     *  (compensates for window size)
     */
    glm::mat4 proj() const;

    /*
     *  View matrix
     *  Applies scale, rotation, offset
     */
    glm::mat4 view() const;

    /*
     *  Complete transform matrix
     */
    glm::mat4 M() const { return proj() * view(); }

    /*
     *  Returns a mouse position scaled to the range -1, 1
     */
    glm::vec2 scaledMousePos(double x, double y) const;

    /*
     *  Resized window callback
     */
    static void _resized(GLFWwindow* window, int w, int h);
    void resized(int w, int h);

    /*
     *  Callback for mouse movement
     */
    static void _mouseMove(GLFWwindow* window, double x, double y);
    void mouseMove(double x, double y);

    /*
     *  Callback for mouse scroll
     */
    static void _mouseButton(GLFWwindow* window, int b, int a, int m);
    void mouseScroll(double x, double y);

    /*
     *  Mouse press callback
     */
    static void _mouseScroll(GLFWwindow* window, double sx, double sy);
    void mouseButton(int button, int action, int mods);

    /*  Pointer to raw window  */
    GLFWwindow* const window;

    /*  Atomic pointer to incoming tree  */
    std::atomic<std::tuple<std::string, std::string, Tree*>*> incoming;

    /*  Set to true if frames should be cleared  */
    std::atomic_bool clear;

    /*  Set to true if the window should be redrawn  */
    std::atomic_bool redraw;

    /*  Set to true if the window should be closed  */
    std::atomic_bool halt;

    /*  Width and height are of the framebuffer, rather than the window  *
     *  (to properly cope with high DPI monitors)                        */
    int width;
    int height;

    /*  Store the scene's center, scale, and rotation                    *
     *  (Euler angles aren't the best representation, but they're easy)  */
    glm::vec3 center;
    float scale=1;
    float pitch=0;
    float  roll=0;

    enum { WINDOW_DRAG_NONE,
           WINDOW_DRAG_PAN,
           WINDOW_DRAG_ROTATE} drag_mode=WINDOW_DRAG_NONE;

    /*  This is the current mouse position (in -1, 1 window coordinates)  */
    glm::vec2 mouse_pos;

    /*  Objects to draw in 3D viewport  */
    Axes axes;
    std::map<std::string, std::map<std::string, Frame*>> frames;

    /*  Frames that should be deleted once they are done rendering  */
    std::list<Frame*> stale;
};

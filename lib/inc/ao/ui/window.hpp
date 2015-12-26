#pragma once

#include <memory>

#include <glm/mat4x4.hpp>
#include <glm/vec3.hpp>
#include <glm/vec2.hpp>

#include "ao/gl/core.hpp"
#include "ao/gl/frame.hpp"
#include "ao/gl/axes.hpp"

class Tree;

class Window
{
public:
    /*
     *  Default Window constructor
     *
     *  Triggers an assertion failure on OpenGL errors
     */
    explicit Window();

    /*
     *  Window destructor deletes all associated Frames
     */
    ~Window();

    /*
     *  Adds a Frame to redraw the given shape
     */
    void addShape(Tree* t);

    /*
     *  Resized window callback
     */
    void resized(int w, int h);

    /*
     *  Callback for mouse movement
     */
    void mouseMove(double x, double y);

    /*
     *  Callback for mouse scroll
     */
    void mouseScroll(double x, double y);

    /*
     *  Mouse press callback
     */
    void mouseButton(int button, int action, int mods);

    /*
     *  Blocking loop updating the window until it is closed
     */
    void run();

protected:
    /*
     *  Redraw the window with the current state
     */
    void draw() const;

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

    /*  Pointer to raw window  */
    GLFWwindow* const window;

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
    std::list<Frame*> frames;
};

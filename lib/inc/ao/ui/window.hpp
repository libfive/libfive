#pragma once

#include <memory>
#include <map>

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
    void addTree(std::string name, Tree* t);

    /*
     *  Clears existing frames asynchronously
     *  (will not take effect until the next poll() call)
     */
    void clearFrames();

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
    std::atomic<std::pair<std::string, Tree*>*> incoming;

    /*  Set to true if frames should be cleared  */
    std::atomic<bool> clear;

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
    std::map<std::string, Frame*> frames;

    /*  Frames that should be deleted once they are done rendering  */
    std::list<Frame*> stale;
};

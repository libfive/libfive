#pragma once

#include <memory>

#include "ao/gl/core.hpp"
#include "ao/gl/frame.hpp"
#include "ao/gl/axes.hpp"

class Tree;

class Window
{
public:
    static bool Show(Tree* tree);

    /*
     *  Resized window callback
     */
    void resized(int w, int h);

protected:
    explicit Window(GLFWwindow* window);

    /*
     *  Blocking loop updating the window until it is closed
     */
    void run();

    /*
     *  Redraw the window with the current state
     */
    void draw() const;

    /*
     *  Projection matrix
     *  (compensates for window size)
     */
    glm::mat4 proj() const;

    /*  Pointer to raw window  */
    GLFWwindow* const window;

    /*  Width and height are of the framebuffer, rather than the window  *
     *  (to properly cope with high DPI monitors)                        */
    int width;
    int height;

    Axes axes;
};

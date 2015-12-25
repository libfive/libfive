#pragma once

#include <memory>

#include "ao/gl/core.hpp"
#include "ao/gl/frame.hpp"

class Tree;

class Window
{
public:
    static bool Show(Tree* tree);

protected:
    explicit Window(GLFWwindow* window);

    void run();

    GLFWwindow* const window;
};

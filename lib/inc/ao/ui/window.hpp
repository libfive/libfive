#pragma once

#include <memory>
#include <GLFW/glfw3.h>

#include "ao/gl/shaders.hpp"
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
    Frame frame;
    Shaders shaders;
};

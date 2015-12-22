#pragma once

#include <memory>
#include <GLFW/glfw3.h>

class Tree;
class Frame;
class Shaders;

class Window
{
public:
    static bool Show(Tree* tree);

protected:
    Window(GLFWwindow* window);

    void run();

    GLFWwindow* const window;
    std::unique_ptr<Frame> frame;
    std::unique_ptr<Shaders> shaders;
};

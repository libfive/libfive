#include <iostream>
#include <chrono>

#include <catch/catch.hpp>

#include "ao/tree/tree.hpp"
#include "ao/tree/store.hpp"

#include "ao/render/heightmap.hpp"
#include "ao/gl/accelerator.hpp"

static std::pair<DepthImage, NormalImage> RENDER(Tree* t, const Region& r)
{
    GLFWwindow* window = makeContext();
    Accelerator a(t);
    auto out = a.Render(r);
    glfwDestroyWindow(window);
    return out;
}

#define DESCRIPTION "(GPU)"
#define EPSILON 1e-6
#include "render.ipp"

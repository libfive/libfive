#include <iostream>
#include <chrono>

#include <catch/catch.hpp>

#include "ao/tree/tree.hpp"
#include "ao/tree/store.hpp"

#include "ao/render/heightmap.hpp"
#include "ao/gl/accelerator.hpp"
#include "ao/gl/texture.hpp"

static std::pair<DepthImage, NormalImage> RENDER(Tree* t, const Region& r)
{
    GLFWwindow* window = makeContext();
    Accelerator a(t);
    auto out = a.Render(r);
    glfwDestroyWindow(window);
    return out;
}

TEST_CASE("Partial rendering (GPU)")
{
    Store s;
    Tree t(&s, s.constant(-1));

    GLFWwindow* window = makeContext();
    Accelerator a(&t);

    Region r({-1, 1}, {-1, 1}, {-1, 1}, 5);

    GLuint depth, norm;
    glGenTextures(1, &depth);
    glGenTextures(1, &norm);

    a.init(r, depth, norm);
    {
        auto d = fromDepthTexture(depth, r);
        CAPTURE(d);
        REQUIRE((d == -std::numeric_limits<double>::infinity()).all());
    }

    Region ra({-1, 1}, {-1, 1}, {0, 0}, 5);
    a.RenderSubregion(ra);

    {
        auto d = fromDepthTexture(depth, r);
        CAPTURE(d);
        REQUIRE((d == 0).all());
    }

    glDeleteTextures(1, &depth);
    glDeleteTextures(1, &norm);
    glfwDestroyWindow(window);
}

#define DESCRIPTION "(GPU)"
#define EPSILON 1e-6
#include "render.ipp"

#include <catch/catch.hpp>

#include "ao/render/heightmap.hpp"
#include "ao/eval/evaluator.hpp"
#include "ao/tree/tree.hpp"
#include "ao/tree/store.hpp"

#include "ao/gl/accelerator.hpp"
#include "ao/gl/texture.hpp"

static std::pair<DepthImage, NormalImage> RENDER(Tree* t, const Region& r)
{
    std::atomic<bool> abort(false);
    GLFWwindow* window = makeContext();

    GLuint depth, norm;
    glGenTextures(1, &depth);
    glGenTextures(1, &norm);

    Accelerator a(t);
    Evaluator e(t);

    Heightmap::Render(&e, &a, r, depth, norm, abort);

    auto out = std::make_pair(fromDepthTexture(depth, r),
                              fromNormalTexture(norm, r));

    glDeleteTextures(1, &depth);
    glDeleteTextures(1, &norm);
    glfwDestroyWindow(window);

    return out;
}

#define DESCRIPTION " (CPU + GPU)"
#define EPSILON 1e-6
#include "render.ipp"

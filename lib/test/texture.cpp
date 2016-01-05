#include <catch/catch.hpp>

#include "ao/gl/texture.hpp"

TEST_CASE("Texture value preservation")
{
    auto window = makeContext();
    Region r({-1, 1}, {0, 1}, {-1, 1}, 5);

    GLuint tex;
    glGenTextures(1, &tex);

    SECTION("Depth")
    {
        DepthImage d = DepthImage::Random(r.Y.size, r.X.size);
        toDepthTexture(d, tex, {r.Z.lower(), r.Z.upper()});
        auto d_ = fromDepthTexture(tex, r);

        auto diff = d - d_;
        CAPTURE(d);
        CAPTURE(d_);
        CAPTURE(diff);

        const double EPSILON = 1e-6;
        REQUIRE((diff.abs() < EPSILON).all());
    }

    SECTION("Normal")
    {
        NormalImage n = NormalImage::Random(r.Y.size, r.X.size);
        toNormalTexture(n, tex);
        auto n_ = fromNormalTexture(tex, r);

        CAPTURE(n);
        CAPTURE(n_);

        REQUIRE((n == n_).all());
    }

    glDeleteTextures(1, &tex);
    glfwDestroyWindow(window);
}

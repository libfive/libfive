#include <memory>

#include <catch/catch.hpp>

#include "ao/ui/gl/core.hpp"
#include "ao/ui/gl/shader.hpp"

#include "ao/kernel/tree/store.hpp"
#include "ao/kernel/tree/tree.hpp"

////////////////////////////////////////////////////////////////////////////////

typedef std::unique_ptr<GLFWwindow, std::function<void(GLFWwindow*)>> WindowPtr;

TEST_CASE("Context creation")
{
    WindowPtr window(makeContext(), glfwDestroyWindow);
    REQUIRE(window != nullptr);
}

TEST_CASE("OpenGL version")
{
    WindowPtr window(makeContext(), glfwDestroyWindow);

    auto version = glGetString(GL_VERSION);
    REQUIRE(version);

    int major = version[0] - '0';
    int minor = version[2] - '0';

    CAPTURE(version);
    REQUIRE((major * 10 + minor) >= 33);
}

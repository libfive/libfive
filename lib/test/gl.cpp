#include <memory>

#include <catch/catch.hpp>

#include "ao/gl/core.hpp"
#include "ao/gl/shader.hpp"
#include "ao/gl/accelerator.hpp"

#include "ao/core/store.hpp"
#include "ao/core/tree.hpp"

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

TEST_CASE("Shader creation")
{
    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
               s.constant(1)));

    WindowPtr window(makeContext(), glfwDestroyWindow);

    // Redirect std::cerr
    //std::streambuf* _cerr = std::cerr.rdbuf();
    //std::ostringstream capture;
    //std::cerr.rdbuf(capture.rdbuf());

    // Build a tree accelerator (which throws assertions on failure)
    auto out = Accelerator(&t);

    // Restore old std::cerr
    //std::cerr.rdbuf(_cerr);
}

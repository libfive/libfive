#include <memory>

#include <catch/catch.hpp>

#include "ao/gl/core.hpp"
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

    SECTION("Raw OpenGL")
    {
        GLuint shader = glCreateShader(GL_FRAGMENT_SHADER);
        const char* txt = t.toShader().c_str();
        glShaderSource(shader, 1, &txt, NULL);
        glCompileShader(shader);

        CAPTURE(t.toShader());
        GLint status;
        glGetShaderiv(shader, GL_COMPILE_STATUS, &status);

        std::string error_msg;
        if (status == GL_FALSE)
        {
            GLint log_length;
            glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &log_length);

            GLchar* info_log = new GLchar[log_length + 1];
            glGetShaderInfoLog(shader, log_length, NULL, info_log);

            error_msg = info_log;
            delete [] info_log;
        }
        CAPTURE(error_msg);

        REQUIRE(status != GL_FALSE);
    }

    SECTION("OGLplus")
    {
        oglplus::VertexShader vs;
        vs.Source(t.toShader());
        vs.Compile();
    }
}

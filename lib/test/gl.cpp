#include <catch/catch.hpp>

#include "ao/gl/core.hpp"
#include "ao/core/store.hpp"
#include "ao/core/tree.hpp"

////////////////////////////////////////////////////////////////////////////////

static GLFWwindow* makeContext()
{
    if (!glfwInit())
    {
        return nullptr;
    }

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, GL_VERSION_MAJOR);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, GL_VERSION_MINOR);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

    glfwWindowHint(GLFW_VISIBLE, GL_FALSE);

    auto out = glfwCreateWindow(640, 480, "", nullptr, nullptr);
    glfwMakeContextCurrent(out);

    return out;
}

static GLFWwindow* init()
{
    auto window = makeContext();
    if (window)
    {
        glewInit();
    }
    return window;
}

static void finish(GLFWwindow* window)
{
    if (window)
    {
        glfwDestroyWindow(window);
    }
    glfwTerminate();
}

////////////////////////////////////////////////////////////////////////////////

TEST_CASE("Context creation")
{
    auto window = makeContext();
    REQUIRE(window != nullptr);
    finish(window);
}

TEST_CASE("Loading GLEW")
{
    auto window = makeContext();
    REQUIRE(glewInit() == GLEW_OK);
    finish(window);
}

TEST_CASE("OpenGL version")
{
    auto window = init();

    auto version = glGetString(GL_VERSION);
    REQUIRE(version);

    int major = version[0] - '0';
    int minor = version[2] - '0';

    CAPTURE(version);
    REQUIRE((major * 10 + minor) >= 33);

    finish(window);
}

TEST_CASE("Shader creation")
{
    Store s;
    Tree t(&s, s.operation(OP_SUB,
               s.operation(OP_ADD, s.operation(OP_MUL, s.X(), s.X()),
                                   s.operation(OP_MUL, s.Y(), s.Y())),
               s.constant(1)));

    auto window = init();

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

    finish(window);
}

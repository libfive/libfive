#include <catch/catch.hpp>

#include "ao/core/store.hpp"
#include "ao/core/tree.hpp"

TEST_CASE("Matrix evaluation")
{
    Store s;
    Tree t(&s, s.X());

    SECTION("Identity")
    {
        t.setMatrix(glm::mat4(1.0, 0.0, 0.0, 0.0,
                              0.0, 1.0, 0.0, 0.0,
                              0.0, 0.0, 1.0, 0.0,
                              0.0, 0.0, 0.0, 1.0));
        REQUIRE(t.eval(1.0, 2.0, 3.0) == 1.0);
    }

    SECTION("Scaling")
    {
        t.setMatrix(glm::mat4(0.5, 0.0, 0.0, 0.0,
                              0.0, 1.0, 0.0, 0.0,
                              0.0, 0.0, 1.0, 0.0,
                              0.0, 0.0, 0.0, 1.0));
        REQUIRE(t.eval(1.0, 2.0, 3.0) == 0.5);
    }

    SECTION("Re-evaluation")
    {
        t.setMatrix(glm::mat4(1.0, 0.0, 0.0, 0.0,
                              0.0, 1.0, 0.0, 0.0,
                              0.0, 0.0, 1.0, 0.0,
                              0.0, 0.0, 0.0, 1.0));
        t.eval(1.0, 2.0, 3.0);
        t.setMatrix(glm::mat4(0.5, 0.0, 0.0, 0.0,
                              0.0, 1.0, 0.0, 0.0,
                              0.0, 0.0, 1.0, 0.0,
                              0.0, 0.0, 0.0, 1.0));
        REQUIRE(t.eval(1.0, 2.0, 3.0) == 0.5);
    }

    SECTION("Offset")
    {
        t.setMatrix(glm::mat4(1.0, 0.0, 0.0, 1.5,
                              0.0, 1.0, 0.0, 0.0,
                              0.0, 0.0, 1.0, 0.0,
                              0.0, 0.0, 0.0, 1.0));
        REQUIRE(t.eval(1.0, 2.0, 3.0) == 2.5);
    }
}

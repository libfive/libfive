#include <cmath>

#include <catch/catch.hpp>
#include <glm/gtc/matrix_transform.hpp>

#include "ao/core/store.hpp"
#include "ao/core/tree.hpp"

TEST_CASE("Matrix evaluation")
{
    Store s;
    Tree t(&s, s.X());

    SECTION("Default matrix")
    {
        REQUIRE(t.eval(1.0, 2.0, 3.0) == 1.0);
    }

    SECTION("Scaling")
    {
        t.setMatrix(glm::scale(glm::mat4(), {0.5, 1.0, 1.0}));
        REQUIRE(t.eval(1.0, 2.0, 3.0) == 0.5);
    }

    SECTION("Swapping")
    {
        t.setMatrix(glm::rotate(glm::mat4(), -(float)M_PI * 0.5f,
                               {0.0, 0.0, 1.0}));
        REQUIRE(t.eval(1.0, 2.0, 3.0) == Approx(2.0));
    }

    SECTION("Re-evaluation")
    {
        t.eval(1.0, 2.0, 3.0);
        t.setMatrix(glm::scale(glm::mat4(), {0.5, 1.0, 1.0}));
        REQUIRE(t.eval(1.0, 2.0, 3.0) == 0.5);
    }

    SECTION("Offset")
    {
        t.setMatrix(glm::translate(glm::mat4(), {0.5, 0.0, 0.0}));
        REQUIRE(t.eval(1.0, 2.0, 3.0) == 1.5);
    }
}

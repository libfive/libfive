#include <cmath>

#include <catch/catch.hpp>
#include <glm/gtc/matrix_transform.hpp>

#include "ao/kernel/tree/store.hpp"
#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/eval/evaluator.hpp"

TEST_CASE("Matrix evaluation")
{
    Store s;
    Tree t(&s, s.X());
    Evaluator e(&t);

    SECTION("Default matrix")
    {
        REQUIRE(e.eval<float>(1.0, 2.0, 3.0) == 1.0);
    }

    SECTION("Scaling")
    {
        e.setMatrix(glm::scale(glm::mat4(), {0.5, 1.0, 1.0}));
        REQUIRE(e.eval<float>(1.0, 2.0, 3.0) == 0.5);
    }

    SECTION("Swapping")
    {
        e.setMatrix(glm::rotate(glm::mat4(), -(float)M_PI * 0.5f,
                               {0.0, 0.0, 1.0}));
        REQUIRE(e.eval<float>(1.0, 2.0, 3.0) == Approx(2.0));
    }

    SECTION("Re-evaluation")
    {
        e.eval<float>(1.0, 2.0, 3.0);
        e.setMatrix(glm::scale(glm::mat4(), {0.5, 1.0, 1.0}));
        REQUIRE(e.eval<float>(1.0, 2.0, 3.0) == 0.5);
    }

    SECTION("Offset")
    {
        e.setMatrix(glm::translate(glm::mat4(), {0.5, 0.0, 0.0}));
        REQUIRE(e.eval<float>(1.0, 2.0, 3.0) == 1.5);
    }
}

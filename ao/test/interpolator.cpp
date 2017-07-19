#include "catch.hpp"

#include "ao/render/brep/interpolator.hpp"
#include "ao/eval/evaluator.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("Interpolator::between")
{
    Evaluator e(circle(1));
    Interpolator<2> i(&e);

    {
        auto a = i.between({0,0}, {2,2});
        auto target = Eigen::Array2f(1 / sqrt(2), 1 / sqrt(2));
        CAPTURE(a);
        CAPTURE((a - target).abs());
        REQUIRE(((a - target).abs() < 0.001).all());
    }

    {
        auto a = i.between({0,0}, {0,2});
        auto target = Eigen::Array2f(0, 1);
        CAPTURE(a);
        CAPTURE((a - target).abs());
        REQUIRE(((a - target).abs() < 0.001).all());
    }
}

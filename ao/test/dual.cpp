#include "catch.hpp"

#include "ao/render/brep/xtree.hpp"
#include "ao/render/brep/dual.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

struct Walker2
{
    void operator()(const std::array<const XTree<2>*, 2>& a)
    {
        for (auto t : a)
        {
            auto n = t->vert.norm();
            min_norm = fmin(n, min_norm);
            max_norm = fmax(n, max_norm);
        }
    }
    float min_norm = 2;
    float max_norm = 0;
};

struct Walker3
{
    void operator()(const std::array<const XTree<3>*, 4>& a)
    {
        for (auto t : a)
        {
            auto n = t->vert.norm();
            min_norm = fmin(n, min_norm);
            max_norm = fmax(n, max_norm);
        }
    }
    float min_norm = 2;
    float max_norm = 0;
};

TEST_CASE("Dual<2>::walk")
{
    Evaluator a(circle(0.5));
    auto ta = XTree<2>(&a, Region<2>({-1, -1}, {1, 1}));

    Walker2 c;
    Dual<2>::walk(&ta, c);
    REQUIRE(c.min_norm > 0.49);
    REQUIRE(c.max_norm < 0.51);
}

TEST_CASE("Dual<3>::walk")
{
    Evaluator a(sphere(0.5));
    auto ta = XTree<3>(&a, Region<3>({-1, -1, -1}, {1, 1, 1}));

    Walker3 c;
    Dual<3>::walk(&ta, c);
    REQUIRE(c.min_norm > 0.49);
    REQUIRE(c.max_norm < 0.51);
}

#include "catch.hpp"

#include "ao/render/brep/ms.hpp"
#include "ao/render/brep/dual.hpp"
#include "ao/render/brep/scaffold.hpp"
#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("SquareMarcher: operator() on circle (fixed depth)")
{
    Evaluator e(circle(1));

    // Create a padded scaffolding for the tree
    Region<2> r({-1, -1}, {1, 1});
    const auto scaffold = Scaffold<2>(&e, r, 2, true);

    // Create the quadtree on the scaffold (without subdividing farther)
    auto xtree = XTree<2>(&e, scaffold, 1000);

    SquareMarcher ms(&e);
    Dual<2>::walk(xtree, ms);

    float rmin = 2;
    float rmax = 0;
    for (auto pt : ms.pts)
    {
        rmin = fmin(rmin, pt.norm());
        rmax = fmax(rmax, pt.norm());
    }
    REQUIRE(rmin != 2);
    REQUIRE(rmax != 0);
    REQUIRE(rmin > 0.99);
    REQUIRE(rmax < 1.01);
}

TEST_CASE("SquareMarcher: operator() on circle (recursive)")
{
    Evaluator e(circle(1));

    // Create a padded scaffolding for the tree
    Region<2> r({-1, -1}, {1, 1});
    const auto scaffold = Scaffold<2>(&e, r, 2, true);

    // Create the quadtree on the scaffold (without subdividing farther)
    auto xtree = XTree<2>(&e, scaffold);

    SquareMarcher ms(&e);
    Dual<2>::walk(xtree, ms);

    float rmin = 2;
    float rmax = 0;
    for (auto pt : ms.pts)
    {
        rmin = fmin(rmin, pt.norm());
        rmax = fmax(rmax, pt.norm());
    }
    REQUIRE(rmin != 2);
    REQUIRE(rmax != 0);
    REQUIRE(rmin > 0.99);
    REQUIRE(rmax < 1.01);
}

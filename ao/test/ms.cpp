#include "catch.hpp"

#include "ao/render/brep/ms.hpp"
#include "ao/render/brep/dual.hpp"
#include "ao/render/brep/scaffold.hpp"
#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("SquareMarcher: operator() on circle")
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
    for (auto pt : ms.points)
    {
        rmin = fmin(rmin, pt.second.norm());
        rmax = fmax(rmax, pt.second.norm());
    }
    REQUIRE(rmin > 0.99);
    REQUIRE(rmax < 1.01);
}

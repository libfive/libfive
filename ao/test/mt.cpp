#include "catch.hpp"

#include "ao/render/brep/mt.hpp"
#include "ao/render/brep/dual.hpp"
#include "ao/render/brep/scaffold.hpp"
#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("TetMarcher: operator() on sphere")
{
    Evaluator e(sphere(1));

    // Create a padded scaffolding for the tree
    Region<3> r({-1, -1, -1}, {1, 1, 1});
    const auto scaffold = Scaffold<3>(&e, r, 2, true);

    // Create the quadtree on the scaffold (without subdividing farther)
    auto xtree = XTree<3>(&e, scaffold, 1000);

    TetMarcher mt(&e);
    Dual<3>::walk(xtree, mt);

    float rmin = 2;
    float rmax = 0;
    for (auto pt : mt.verts)
    {
        std::cout << pt.transpose() << '\n';
        rmin = fmin(rmin, pt.norm());
        rmax = fmax(rmax, pt.norm());
    }
    REQUIRE(rmin > 0.99);
    REQUIRE(rmax < 1.01);
}


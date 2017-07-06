#include "catch.hpp"

#include "ao/render/brep/xtree.hpp"
#include "ao/render/brep/dual.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

struct Walker2
{
    void operator()(const std::array<const XTree<2>*, 4>& a)
    {
        CAPTURE(a[0]->vert);
        CAPTURE(a[1]->vert);
        CAPTURE(a[2]->vert);
        CAPTURE(a[3]->vert);

        // Confirm that we're unpacking vertices in the right order
        REQUIRE(a[0]->vert.x() <= a[1]->vert.x());
        REQUIRE(a[0]->vert.y() <= a[2]->vert.y());
        REQUIRE(a[2]->vert.x() <= a[3]->vert.x());
        REQUIRE(a[1]->vert.y() <= a[3]->vert.y());
    }
};

TEST_CASE("Dual<2>::walk")
{
    Evaluator a(circle(0.5));
    auto ta = XTree<2>(&a, Region<2>({0.6, 0.6}, {0.8, 0.8}));

    Walker2 c;
    Dual<2>::walk(ta, c);
}

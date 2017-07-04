#include "catch.hpp"

#include "ao/render/brep/xtree.hpp"
#include "ao/render/brep/dual.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

struct Walker2
{
    void operator()(const XTree<2>& a,
                    const XTree<2>& b,
                    const XTree<2>& c,
                    const XTree<2>& d)
    {
        // Confirm that we're unpacking vertices in the right order
        REQUIRE(a.vert.x() <= b.vert.x());
        REQUIRE(a.vert.y() <= c.vert.y());
        REQUIRE(c.vert.x() <= d.vert.x());
        REQUIRE(b.vert.y() <= d.vert.y());
    }
};

TEST_CASE("Dual<2>::walk")
{
    Evaluator a(circle(0.5));
    auto ta = XTree<2>(&a, Region<2>({0.6, 0.6}, {0.8, 0.8}));

    Walker2 c;
    Dual<2>::walk(ta, c);
}

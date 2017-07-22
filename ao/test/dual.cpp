#include "catch.hpp"

#include "ao/render/brep/xtree.hpp"
#include "ao/render/brep/dual.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

struct Walker2
{
    void push(const Region<2>&) {}
    void pop() {}

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

struct Walker3
{
    void push(const Region<3>&) {}
    void pop() {}

    void operator()(const std::array<const XTree<3>*, 8>& a)
    {
        CAPTURE(a[0]->vert.transpose());
        CAPTURE(a[1]->vert.transpose());
        CAPTURE(a[2]->vert.transpose());
        CAPTURE(a[3]->vert.transpose());
        CAPTURE(a[4]->vert.transpose());
        CAPTURE(a[5]->vert.transpose());
        CAPTURE(a[6]->vert.transpose());
        CAPTURE(a[7]->vert.transpose());
        CAPTURE(a[6]->region.lower.transpose());
        CAPTURE(a[6]->region.upper.transpose());
        CAPTURE(a[7]->region.lower.transpose());
        CAPTURE(a[7]->region.upper.transpose());

        // Confirm that we're unpacking vertices in the right order
        REQUIRE(a[0]->vert.x() <= a[1]->vert.x());
        REQUIRE(a[2]->vert.x() <= a[3]->vert.x());
        REQUIRE(a[4]->vert.x() <= a[5]->vert.x());
        REQUIRE(a[6]->vert.x() <= a[7]->vert.x());

        REQUIRE(a[0]->vert.y() <= a[2]->vert.y());
        REQUIRE(a[1]->vert.y() <= a[3]->vert.y());
        REQUIRE(a[4]->vert.y() <= a[6]->vert.y());
        REQUIRE(a[5]->vert.y() <= a[7]->vert.y());

        REQUIRE(a[0]->vert.z() <= a[4]->vert.z());
        REQUIRE(a[1]->vert.z() <= a[5]->vert.z());
        REQUIRE(a[2]->vert.z() <= a[6]->vert.z());
        REQUIRE(a[3]->vert.z() <= a[7]->vert.z());
    }
};

TEST_CASE("Dual<2>::walk")
{
    Evaluator a(circle(0.5));
    auto ta = XTree<2>(&a, Region<2>({0.6, 0.6}, {0.8, 0.8}));

    Walker2 c;
    Dual<2>::walk(ta, c);
}

TEST_CASE("Dual<3>::walk")
{
    Evaluator a(sphere(0.5));
    auto ta = XTree<3>(&a, Region<3>({0.6, 0.6, 0.6}, {0.8, 0.8, 0.8}));

    Walker3 c;
    Dual<3>::walk(ta, c);
}

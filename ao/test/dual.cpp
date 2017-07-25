#include "catch.hpp"

#include "ao/render/brep/xtree.hpp"
#include "ao/render/brep/dual.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

struct Walker2
{
    void push(const Region<2>&) {}
    void pop() {}

    void operator()(const std::array<const XTree<2>*, 2>& a)
    {
        (void)a;
        // Figure out what to test here
    }
};

struct Walker3
{
    void push(const Region<3>&) {}
    void pop() {}

    void operator()(const std::array<const XTree<3>*, 4>& a)
    {
        (void)a;
        // Figure out what to test here
    }
};

TEST_CASE("Dual<2>::walk")
{
    Evaluator a(circle(0.5));
    auto ta = XTree<2>(&a, Region<2>({-1, -1}, {1, 1}));

    Walker2 c;
    Dual<2>::walk(&ta, c);
}

TEST_CASE("Dual<3>::walk")
{
    Evaluator a(sphere(0.5));
    auto ta = XTree<3>(&a, Region<3>({-1, -1, -1}, {1, 1, 1}));

    Walker3 c;
    Dual<3>::walk(&ta, c);
}

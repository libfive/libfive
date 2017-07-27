#include "catch.hpp"

#include "ao/render/brep/xtree.hpp"
#include "ao/render/brep/dual.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

struct Walker2
{
    // Check winding of contours
    void operator()(const std::array<const XTree<2>*, 2>& a)
    {
        auto norm = Eigen::Vector2d(a[0]->vert.y() - a[1]->vert.y(),
                                    a[1]->vert.x() - a[0]->vert.x()).normalized();
        Eigen::Vector2d center = (a[0]->vert + a[1]->vert).normalized();
        auto dot_ = center.dot(norm);
        std::cout << dot_ << '\n';
        neg += (dot_ < 0);
        pos += (dot_ > 0);
        dot = fmin(dot, dot_);
    }
    int pos = 0;
    int neg = 0;
    float dot = 2;
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
    auto ta = XTree<2>::build(circle(0.5), Region<2>({-1, -1}, {1, 1}));

    Walker2 c;
    Dual<2>::walk(ta.get(), c);
    CAPTURE(c.neg);
    CAPTURE(c.pos);
    REQUIRE(c.dot > 0.9);
}

TEST_CASE("Dual<3>::walk")
{
    auto ta = XTree<3>::build(sphere(0.5), Region<3>({-1, -1, -1}, {1, 1, 1}));

    Walker3 c;
    Dual<3>::walk(ta.get(), c);
    REQUIRE(c.min_norm > 0.49);
    REQUIRE(c.max_norm < 0.51);
}

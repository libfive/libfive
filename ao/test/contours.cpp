#include "catch.hpp"

#include "ao/tree/tree.hpp"

#include "ao/format/contours.hpp"
#include "ao/render/region.hpp"

// Overloaded toString for glm::vec3
#include "util/glm.hpp"
#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("Contours::render (adjacent rectangles)")
{
    auto rects = min(rectangle(-1, 0, -1, 1), rectangle(0, 1, -1, 1));
    Region r({-2, 2}, {-2, 2}, {0, 0}, 2);

    auto cs_pos = Contours::render(rects, r);
    REQUIRE(cs_pos.contours.size() == 1);

    auto cs_neg = Contours::render(-rects, r);
    REQUIRE(cs_neg.contours.size() == 1);
}

TEST_CASE("Simple 2D contouring")
{
    Tree t = circle(0.5);

    Region r({-1, 1}, {-1, 1}, {0, 0}, 1);

    auto m = Contours::render(t, r);
    REQUIRE(m.contours.size() == 1);
}

TEST_CASE("2D contour tracking")
{
    Tree t = circle(0.5);

    Region r({-1, 1}, {-1, 1}, {0, 0}, 10);

    auto m = Contours::render(t, r);
    REQUIRE(m.contours.size() == 1);

    float min = 1;
    float max = 0;
    for (auto c : m.contours[0])
    {
        auto r = sqrt(pow(c.x, 2) + pow(c.y, 2));
        min = fmin(min, r);
        max = fmax(max, r);
    }
    REQUIRE(max < 0.51);
    REQUIRE(min > 0.49);
}

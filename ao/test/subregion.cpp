#include "catch.hpp"

#include "ao/render/region.hpp"
#include "ao/render/subregion.hpp"
#include "ao/render/octree.hpp"

using namespace Kernel;

TEST_CASE("Subregion::Axis split")
{
    auto d = Region::Axis(Interval(0, 1), 10.0f);
    auto s = Subregion::Axis(d.bounds, d.values);
    auto ds = s.split();

    REQUIRE(ds.first.lower() == 0);
    REQUIRE(ds.first.upper() == 0.5);
    REQUIRE(ds.first.min == 0);
    REQUIRE(ds.first.size == 5);

    REQUIRE(ds.second.lower() == 0.5);
    REQUIRE(ds.second.upper() == 1.0);
    REQUIRE(ds.second.min == 5);
    REQUIRE(ds.second.size == 5);
}

TEST_CASE("Subregion zero position")
{
    auto r = Region({0, 10}, {0, 5}, {0, 2.5}, 10);
    auto s = r.view();

    REQUIRE(s.X.min == 0);
    REQUIRE(s.Y.min == 0);
    REQUIRE(s.Z.min == 0);
}

TEST_CASE("Subregion canSplit")
{
    auto ra= Region({0, 1}, {0, 1}, {0, 2}, 1);
    auto sa = ra.view();
    REQUIRE(sa.canSplit());

    auto rb = Region({0, 1}, {0, 1}, {0, 1}, 1);
    auto sb = rb.view();
    REQUIRE(!sb.canSplit());
}

TEST_CASE("Splitting a region")
{
    auto r = Region({0, 1}, {0, 1}, {0, 2}, 1);
    auto s = r.view();
    auto rs = s.split();

    REQUIRE(rs.first.Z.min == 0);
    REQUIRE(rs.first.Z.size == 1);
    REQUIRE(rs.first.Z.lower() == 0);
    REQUIRE(rs.first.Z.upper() == 1);

    REQUIRE(rs.second.Z.min == 1);
    REQUIRE(rs.second.Z.size == 1);
    REQUIRE(rs.second.Z.lower() == 1);
    REQUIRE(rs.second.Z.upper() == 2);
}

TEST_CASE("Splitting a region with odd voxel count")
{
    auto r = Region({0, 1}, {0, 1}, {0, 3}, 1);
    auto s = r.view();
    auto rs = s.split();

    REQUIRE(rs.first.Z.min == 0);
    REQUIRE(rs.first.Z.size == 1);
    REQUIRE(rs.first.Z.lower() == 0);
    REQUIRE(rs.first.Z.upper() == 1);

    REQUIRE(rs.second.Z.min == 1);
    REQUIRE(rs.second.Z.size == 2);
    REQUIRE(rs.second.Z.lower() == 1);
    REQUIRE(rs.second.Z.upper() == 3);
}

TEST_CASE("Splitting a big region")
{
    Region r({-1, 1}, {-1, 1}, {0, 0}, 100);
    auto s = r.view();
    auto rs = s.split();

    REQUIRE(rs.first.X.lower() == -1);
    REQUIRE(rs.first.X.upper() == 0);

    REQUIRE(rs.second.X.lower() == 0);
    REQUIRE(rs.second.X.upper() == 1);
}

TEST_CASE("Splitting a region with equal voxels")
{
    Region r({-1, 1}, {-1, 1}, {-1, 1}, 8);
    auto s = r.view();
    auto rs = s.split();

    REQUIRE(rs.first.X.lower() == -1);
    REQUIRE(rs.first.X.upper() == 0);
}

TEST_CASE("Subregion::canSplitEven")
{
    Region a({-1, 1}, {-1, 1}, {-1, 1}, 8);
    REQUIRE(a.view().canSplitEven<3>());
    REQUIRE(!a.view().canSplitEven<2>());

    Region b({0, 1}, {0, 1}, {0, 1}, 7);
    REQUIRE(!b.view().canSplitEven<3>());
    REQUIRE(!b.view().canSplitEven<2>());

    Region c({0, 1}, {-1, 1}, {-1, 1}, 8);
    REQUIRE(!c.view().canSplitEven<3>());
    REQUIRE(!c.view().canSplitEven<2>());

    Region d({-1, 1}, {-1, 1}, {0, 0}, 8);
    REQUIRE(d.view().canSplitEven<2>());
    REQUIRE(!d.view().canSplitEven<3>());
}

TEST_CASE("Subregion::splitEven<3>")
{
    Region a({-1, 1}, {-2, 2}, {-4, 4}, 4, 2, 1);
    Subregion s = a.view();

    REQUIRE(s.canSplitEven<3>());

    auto out = s.splitEven<3>();
    REQUIRE(out.size() == 8);
    for (int i=0; i < 8; ++i)
    {
        const Subregion& sub = out[i];
        CAPTURE(sub.X.lower());
        CAPTURE(sub.X.upper());
        CAPTURE(sub.Y.lower());
        CAPTURE(sub.Y.upper());
        CAPTURE(sub.Z.lower());
        CAPTURE(sub.Z.upper());
        if (i & AXIS_X)
        {
            REQUIRE(sub.X.lower() ==  0);
            REQUIRE(sub.X.upper() ==  1);
        }
        else
        {
            REQUIRE(sub.X.lower() == -1);
            REQUIRE(sub.X.upper() ==  0);
        }

        if (i & AXIS_Y)
        {
            REQUIRE(sub.Y.lower() ==  0);
            REQUIRE(sub.Y.upper() ==  2);
        }
        else
        {
            REQUIRE(sub.Y.lower() == -2);
            REQUIRE(sub.Y.upper() ==  0);
        }

        if (i & AXIS_Z)
        {
            REQUIRE(sub.Z.lower() ==  0);
            REQUIRE(sub.Z.upper() ==  4);
        }
        else
        {
            REQUIRE(sub.Z.lower() == -4);
            REQUIRE(sub.Z.upper() ==  0);
        }
    }
}

TEST_CASE("Subregion::splitEven<2>()")
{
    Region a({-1, 1}, {-2, 2}, {0, 0}, 4, 2, 1);
    Subregion s = a.view();

    REQUIRE(s.canSplitEven<2>());

    auto out = s.splitEven<2>();
    REQUIRE(out.size() == 4);
    for (int i=0; i < 4; ++i)
    {
        const Subregion& sub = out[i];
        CAPTURE(sub.X.lower());
        CAPTURE(sub.X.upper());
        CAPTURE(sub.Y.lower());
        CAPTURE(sub.Y.upper());
        if (i & AXIS_X)
        {
            REQUIRE(sub.X.lower() ==  0);
            REQUIRE(sub.X.upper() ==  1);
        }
        else
        {
            REQUIRE(sub.X.lower() == -1);
            REQUIRE(sub.X.upper() ==  0);
        }

        if (i & AXIS_Y)
        {
            REQUIRE(sub.Y.lower() ==  0);
            REQUIRE(sub.Y.upper() ==  2);
        }
        else
        {
            REQUIRE(sub.Y.lower() == -2);
            REQUIRE(sub.Y.upper() ==  0);
        }

        REQUIRE(!(i & AXIS_Z));
    }
}

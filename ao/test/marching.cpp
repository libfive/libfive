#include "catch.hpp"

#include "ao/render/brep/marching.hpp"

using namespace Kernel;

TEST_CASE("Marching::buildTable<2>")
{
    auto t = Marching::buildTable<2>();
    REQUIRE(t->v.size() == 16);

    // Make sure that we have built every item in the table
    for (unsigned i=1; i < t->v.size() - 1; ++i)
    {
        CAPTURE(i);
        auto& _t = t->v[i];
        REQUIRE(t->v[i][0][0].first != -1);
    }

    REQUIRE(t->v[0][0][0].first == -1);
    REQUIRE(t->v[15][0][0].first == -1);
}

TEST_CASE("Marching::buildTable<3>")
{
    auto t = Marching::buildTable<3>();
    REQUIRE(t->v.size() == 256);

    // Make sure that we have built every item in the table
    for (unsigned i=1; i < t->v.size() - 1; ++i)
    {
        CAPTURE(i);
        auto& _t = t->v[i];
        REQUIRE(t->v[i][0][0].first != -1);
    }

    REQUIRE(t->v[0][0][0].first == -1);
    REQUIRE(t->v[255][0][0].first == -1);
}

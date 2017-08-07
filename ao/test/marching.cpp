#include "catch.hpp"

#include "ao/render/brep/marching.hpp"

using namespace Kernel;

TEST_CASE("Marching::buildTable<2>")
{
    auto t = Marching::buildTable<2>();
    REQUIRE(t->size() == 16);

    // Make sure that we have built every item in the table
    for (unsigned i=1; i < t->size() - 1; ++i)
    {
        CAPTURE(i);
        auto& _t = (*t)[i];
        REQUIRE((*t)[i][0][0].first != -1);
    }
}

TEST_CASE("Marching::buildTable<3>")
{
    auto t = Marching::buildTable<3>();
    REQUIRE(t->size() == 256);

    // Make sure that we have built every item in the table
    for (unsigned i=1; i < t->size() - 1; ++i)
    {
        CAPTURE(i);
        auto& _t = (*t)[i];
        REQUIRE((*t)[i][0][0].first != -1);
    }
}

#include <catch/catch.hpp>

#include "ao/eval/result.hpp"

TEST_CASE("AVX load / store")
{
    Result r;
    REQUIRE(r.count<double>() == 256);

    for (int i=0; i < 256; ++i)
    {
        r.set<double>(i, i);
    }

    // Confirm that loading worked
    for (int i=0; i < 256; ++i)
    {
        REQUIRE(r.get<double>(i) == i);
    }

    r.packAVX();

    // Wipe the double memory banks
    for (int i=0; i < 256; ++i)
    {
        r.set<double>(0.0f, i);
    }

    // Confirm that wiping worked
    for (int i=0; i < 256; ++i)
    {
        REQUIRE(r.get<double>(i) == 0.0f);
    }

    r.unpackAVX();

    // Confirm that unpacking worked
    for (int i=0; i < 256; ++i)
    {
        REQUIRE(r.get<double>(i) == i);
    }
}

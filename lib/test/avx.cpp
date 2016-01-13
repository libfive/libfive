#include <catch/catch.hpp>

#include "ao/eval/result.hpp"

#ifdef USE_AVX
TEST_CASE("AVX load / store")
{
    Result r;
    REQUIRE(r.count<float>() == 256);

    for (int i=0; i < 256; ++i)
    {
        r.set<float>(i, i);
    }

    // Confirm that loading worked
    for (int i=0; i < 256; ++i)
    {
        REQUIRE(r.get<float>(i) == i);
    }

    r.packAVX();

    // Wipe the float memory banks
    for (int i=0; i < 256; ++i)
    {
        r.set<float>(0.0f, i);
    }

    // Confirm that wiping worked
    for (int i=0; i < 256; ++i)
    {
        REQUIRE(r.get<float>(i) == 0.0f);
    }

    r.unpackAVX();

    // Confirm that unpacking worked
    for (int i=0; i < 256; ++i)
    {
        REQUIRE(r.get<float>(i) == i);
    }
}
#endif

#include <catch/catch.hpp>

#include "ao/kernel/tree/store.hpp"
#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/eval/result.hpp"

#ifdef __AVX__
TEST_CASE("AVX load / store")
{
    Result r;
    REQUIRE(r.count<float>() == 256);

    for (int i=0; i < 256; ++i)
    {
        r.set<float>(i, i);
    }

    // Confirm that loading worked
    bool success = true;
    for (int i=0; i < 256; ++i)
    {
        success &= r.get<float>(i) == i;
    }
    REQUIRE(success);

    r.packAVX();

    // Wipe the float memory banks
    for (int i=0; i < 256; ++i)
    {
        r.set<float>(0.0f, i);
    }

    // Confirm that wiping worked
    success = true;
    for (int i=0; i < 256; ++i)
    {
        success &= r.get<float>(i) == 0.0f;
    }
    REQUIRE(success);

    r.unpackAVX();

    // Confirm that unpacking worked
    success = true;
    for (int i=0; i < 256; ++i)
    {
        success &= r.get<float>(i) == i;
    }
    REQUIRE(success);
}

TEST_CASE("Vectorized performance")
{
    const float N = 1000;

    Store s;
    Tree t(&s, s.operation(OP_ADD, s.X(), s.Y()));
    Evaluator e(&t);

    for (int i=0; i < 256; ++i)
    {
        e.setPoint<float>(i, 2*i, 0, i);
    }
    e.packAVX();

    std::chrono::time_point<std::chrono::system_clock> start, end;
    start = std::chrono::system_clock::now();
    for (int i=0; i < N; ++i)
    {
        e.evalCore<float>(256);
    }
    end = std::chrono::system_clock::now();
    std::chrono::duration<double> ft = end - start;

    start = std::chrono::system_clock::now();
    for (int i=0; i < N; ++i)
    {
        e.evalCore<__m256>(256);
    }
    end = std::chrono::system_clock::now();
    std::chrono::duration<double> mt = end - start;

    REQUIRE(mt.count() < ft.count());
}
#endif

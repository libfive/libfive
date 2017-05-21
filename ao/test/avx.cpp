#include <chrono>
#include <cstring>

#include "catch.hpp"

#include "ao/tree/tree.hpp"
#include "ao/eval/evaluator_base.hpp"
#include "ao/eval/evaluator_avx.hpp"
#include "ao/eval/result.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

#ifdef __AVX__

TEST_CASE("Vectorized performance")
{
    // Oversample to get meaningful result
    const float N = 100;

    Tree t = menger(3);
    EvaluatorBase e(t);
    EvaluatorAVX ea(t);

    for (unsigned i=0; i < Result::N; ++i)
    {
        e.set({i, 2*i, 0}, i);
        ea.set({i, 2*i, 0}, i);
    }

    SECTION("Speed")
    {
        std::chrono::time_point<std::chrono::system_clock> start, end;
        start = std::chrono::system_clock::now();
        for (int i=0; i < N; ++i)
        {
            e.values(Result::N);
        }
        end = std::chrono::system_clock::now();
        std::chrono::duration<double> ft = end - start;

        start = std::chrono::system_clock::now();
        for (int i=0; i < N; ++i)
        {
            ea.values(Result::N);
        }
        end = std::chrono::system_clock::now();
        std::chrono::duration<double> mt = end - start;

        // Theoretically, vectorized performance is up to 8x faster
        // We'll insist on at least a 2x speedup
        REQUIRE(mt.count() < ft.count()/2);
    }

    SECTION("Accuracy")
    {
        float vec[Result::N];
        float nonvec[Result::N];

        memcpy(   vec, e.values(Result::N),  Result::N * sizeof(float));
        memcpy(nonvec, ea.values(Result::N), Result::N * sizeof(float));

        bool matched = true;
        for (unsigned i=0; i < Result::N; ++i)
        {
            if (vec[i] != nonvec[i])
            {
                CAPTURE(i);
                CAPTURE(vec[i]);
                CAPTURE(nonvec[i]);
                REQUIRE(false);
                matched = false;
            }
        }
        REQUIRE(matched);
    }
}

TEST_CASE("Alignment")
{
    // Make sure that struct padding works like I think it works
    struct { char a;
             Result result; } s;
    s.result.resize(2);

    // Check the alignment of the first two members of value array
    REQUIRE(((intptr_t)(&s.result.mf[0]) & 0x1f) == 0);
    REQUIRE(((intptr_t)(&s.result.mf[1]) & 0x1f) == 0);

    // Check the derivative arrays too
    REQUIRE(((intptr_t)(&s.result.mdx[0]) & 0x1f) == 0);
    REQUIRE(((intptr_t)(&s.result.mdx[1]) & 0x1f) == 0);
    REQUIRE(((intptr_t)(&s.result.mdy[0]) & 0x1f) == 0);
    REQUIRE(((intptr_t)(&s.result.mdy[1]) & 0x1f) == 0);
    REQUIRE(((intptr_t)(&s.result.mdz[0]) & 0x1f) == 0);
    REQUIRE(((intptr_t)(&s.result.mdz[1]) & 0x1f) == 0);
}

#endif

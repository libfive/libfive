/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <chrono>
#include <cstring>
#include <catch/catch.hpp>

#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/eval/result.hpp"

#include "util/shapes.hpp"

#ifdef __AVX__

TEST_CASE("Vectorized performance")
{
    // Oversample to get meaningful result
    const float N = 100;

    Tree t = menger(3);
    Evaluator e(t);

    for (unsigned i=0; i < Result::N; ++i)
    {
        e.set(i, 2*i, 0, i);
    }

    SECTION("Speed")
    {
        std::chrono::time_point<std::chrono::system_clock> start, end;
        start = std::chrono::system_clock::now();
        for (int i=0; i < N; ++i)
        {
            e.values(Result::N, false);
        }
        end = std::chrono::system_clock::now();
        std::chrono::duration<double> ft = end - start;

        start = std::chrono::system_clock::now();
        for (int i=0; i < N; ++i)
        {
            e.values(Result::N, true);
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

        memcpy(   vec, e.values(Result::N,  true), Result::N * sizeof(float));
        memcpy(nonvec, e.values(Result::N, false), Result::N * sizeof(float));

        bool matched = true;
        for (unsigned i=0; i < Result::N; ++i)
        {
            if (vec[i] != nonvec[i])
            {
                CAPTURE(i);
                CAPTURE(vec[i]);
                CAPTURE(nonvec[i]);
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
    REQUIRE(((intptr_t)(&s.result[0]) & 0x1f) == 0);

    // Ensure that tightly-packing Clauses keeps everything aligned
    REQUIRE((sizeof(Clause) & 0x1f) == 0);

    // Double-check alignment requirements
    REQUIRE(alignof(Result) == 32);
    REQUIRE(alignof(Clause) == 32);
}

#endif

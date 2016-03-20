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
#include <catch/catch.hpp>

#include "ao/kernel/tree/store.hpp"
#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/eval/result.hpp"

#ifdef __AVX__

TEST_CASE("Vectorized performance")
{
    // Oversample to get meaningful result
    const float N = 1000;

    Store s;
    Tree t(&s, s.operation(OP_ADD, s.X(), s.Y()));
    Evaluator e(&t);

    for (unsigned i=0; i < Result::N; ++i)
    {
        e.set(i, 2*i, 0, i);
    }

    std::chrono::time_point<std::chrono::system_clock> start, end;
    start = std::chrono::system_clock::now();
    const float* slow;
    for (int i=0; i < N; ++i)
    {
        slow = e.values(Result::N, false);
    }
    end = std::chrono::system_clock::now();
    std::chrono::duration<double> ft = end - start;

    start = std::chrono::system_clock::now();
    const float* fast;
    for (int i=0; i < N; ++i)
    {
        fast = e.values(Result::N, true);
    }
    end = std::chrono::system_clock::now();
    std::chrono::duration<double> mt = end - start;

    REQUIRE(mt.count() < ft.count());

    bool matched = true;
    for (unsigned i=0; i < Result::N; ++i)
    {
        if (fast[i] != slow[i])
        {
            CAPTURE(i);
            CAPTURE(fast[i]);
            CAPTURE(slow[i]);
            matched = false;
        }
    }
    REQUIRE(matched);
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

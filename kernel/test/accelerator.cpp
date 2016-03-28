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
#include <cstdlib>

#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/cuda/multikernel.hpp"
#include "ao/kernel/cuda/tape.hpp"

#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/tree/store.hpp"

#include "shapes.hpp"

#ifdef USE_CUDA

TEST_CASE("Vectorized evaluation")
{
    Store s;
    Tree t(&s, s.operation(OP_ADD, s.X(), s.Y()));
    Evaluator e(&t);
    TapeAccelerator a(&e);

    for (unsigned i=0; i < TapeAccelerator::N; ++i)
    {
        a.set(i, 2*i, 0, i);
    }
    a.toDevice();

    auto out_d = a.values(TapeAccelerator::N);
    auto out = a.fromDevice(out_d);

    bool matched = true;
    unsigned i;
    for (i=0; i < TapeAccelerator::N; ++i)
    {
        if (out[i] != 3*i)
        {
            matched = false;
            break;
        }
    }

    CAPTURE(i);
    CAPTURE(out[i]);
    CAPTURE(3*i);
    REQUIRE(matched);
}

template <class T> void testSponge()
{
    std::unique_ptr<Tree> sponge(menger(2));

    Evaluator e(sponge.get());
    T a(&e);

    for (unsigned i=0; i < Result::N; ++i)
    {
        float x = rand() / float(RAND_MAX);
        float y = rand() / float(RAND_MAX);
        float z = rand() / float(RAND_MAX);

        e.set(x, y, z, i);
        a.set(x, y, z, i);
    }

    a.toDevice();
    auto out_d = a.values(Result::N);
    auto out_accel = a.fromDevice(out_d);

    auto out_normal = e.values(Result::N);

    bool matched = true;
    unsigned i;
    for (i=0; i < Result::N; ++i)
    {
        if (out_normal[i] != out_accel[i])
        {
            matched = false;
            break;
        }
    }

    CAPTURE(i);
    CAPTURE(out_normal[i]);
    CAPTURE(out_accel[i]);
    REQUIRE(matched);
}

template <class T> void testSpongeSpeed(int oversample)
{
    std::chrono::time_point<std::chrono::system_clock> start, end;
    std::chrono::duration<double> elapsed;

    std::unique_ptr<Tree> sponge(menger(2));

    Evaluator e(sponge.get());
    T a(&e);

    for (unsigned i=0; i < T::N; ++i)
    {
        float x = rand() / float(RAND_MAX);
        float y = rand() / float(RAND_MAX);
        float z = rand() / float(RAND_MAX);

        e.set(x, y, z, i);
        a.set(x, y, z, i);
    }

    start = std::chrono::system_clock::now();
    a.toDevice();
    float* out_d;
    for (int i=0; i < oversample; ++i)
    {
        out_d = a.values(T::N);
    }
    auto out_accel = a.fromDevice(out_d);
    end = std::chrono::system_clock::now();

    elapsed = end - start;
    std::string log("Rendered sponge in " + std::to_string(elapsed.count() / oversample) + " sec");

    WARN(log);
}

TEST_CASE("Vectorized sponge")
{
    testSponge<TapeAccelerator>();
    testSpongeSpeed<TapeAccelerator>(500);
}

#endif

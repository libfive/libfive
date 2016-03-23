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

#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/eval/accelerator.hpp"

#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/tree/store.hpp"

TEST_CASE("Vectorized evaluation")
{
    Store s;
    Tree t(&s, s.operation(OP_ADD, s.X(), s.Y()));
    Evaluator e(&t);
    Accelerator a(&e);

    for (unsigned i=0; i < Accelerator::N; ++i)
    {
        a.set(i, 2*i, 0, i);
    }
    a.toDevice();

    auto out_d = a.values(Accelerator::N);
    auto out = a.fromDevice(out_d);

    bool matched = true;
    unsigned i;
    for (i=0; i < Accelerator::N; ++i)
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

/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "catch.hpp"

#include "libfive/eval/interval.hpp"

using namespace Kernel;

TEST_CASE("Interval::state")
{
    REQUIRE(Interval::state<double>(-1.0) == Interval::FILLED);
    REQUIRE(Interval::state<double>( 1.0) == Interval::EMPTY);
    REQUIRE(Interval::state<double>( 0.0) == Interval::AMBIGUOUS);
}

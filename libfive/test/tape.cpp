/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

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
#include <Eigen/Geometry>

#include "catch.hpp"

#include "libfive/tree/tree.hpp"
#include "libfive/eval/tape.hpp"

using namespace Kernel;

TEST_CASE("Tape::num_clauses")
{
    Tape t(Tree::X() + 1);
    REQUIRE(t.num_clauses == 5); // X, Y, Z, 1, +
}

TEST_CASE("Tape::constants")
{
    Tape t(Tree::X() + 5);
    REQUIRE(t.constants.size() == 1);

    CAPTURE(*t.constants.begin());
    REQUIRE(t.constants.at(0) == 5.0f);
}

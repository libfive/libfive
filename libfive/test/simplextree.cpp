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

#include "libfive/render/simplex/simplextree.hpp"
#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("SimplexTree<3>::SimplexTree")
{
    auto s = sphere(1);
    auto eval = DerivArrayEvaluator(std::shared_ptr<Tape>(new Tape(s)));

    auto t = SimplexTree<3>(&eval, Simplex<3, 7>({-2, -2, -2}, {2, 2, 2}),
                            0.5, 0.1);
    REQUIRE(true);
}

TEST_CASE("SimplexTree<2>::SimplexTree")
{
    auto s = circle(1);
    auto eval = DerivArrayEvaluator(std::shared_ptr<Tape>(new Tape(s)));

    auto t = SimplexTree<2>(&eval, Simplex<2, 3>({-2, -2}, {2, 2}),
                            0.5, 0.1);
    REQUIRE(true);
}

TEST_CASE("SimplexTree::isInSimplex")
{
    std::array<int, 3> simplex = {{-1, 1, -1}};
    REQUIRE(SimplexTree<3>::isInSimplex(2, simplex));
}

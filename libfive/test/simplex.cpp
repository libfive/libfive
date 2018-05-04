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

#include "libfive/render/simplex/simplex.hpp"
#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("Simplex::containsCorner")
{
    Simplex<3> s = Simplex<3>::fromIndex(2);
    REQUIRE(s.containsCorner(0));
    REQUIRE(s.containsCorner(1));
}

TEST_CASE("Simplex<2>::to/fromSimplex")
{
    for (unsigned i=0; i < 8; ++i)
    {
        REQUIRE(Simplex<2>::fromIndex(i).toIndex() == i);
    }
}

TEST_CASE("Simplex<3>::to/fromSimplex")
{
    for (unsigned i=0; i < 26; ++i)
    {
        REQUIRE(Simplex<3>::fromIndex(i).toIndex() == i);
    }
}

TEST_CASE("Simplex<3>::containsSimplex")
{
    Simplex<3> s = Simplex<3>::fromIndex(2);
    REQUIRE(s.containsSimplex(Simplex<3>::fromIndex(0)));
    REQUIRE(s.containsSimplex(Simplex<3>::fromIndex(1)));
    REQUIRE(s.containsSimplex(Simplex<3>::fromIndex(2)));
}

TEST_CASE("Simplex<2>::fromCorner")
{
    REQUIRE(Simplex<2>::fromCorner(0).toIndex() == 0);
    REQUIRE(Simplex<2>::fromCorner(1).toIndex() == 1);
    REQUIRE(Simplex<2>::fromCorner(2).toIndex() == 3);
    REQUIRE(Simplex<2>::fromCorner(3).toIndex() == 4);
}

TEST_CASE("Simplex<3>::fromCorner")
{
    REQUIRE(Simplex<3>::fromCorner(0).toIndex() == 0);
    REQUIRE(Simplex<3>::fromCorner(1).toIndex() == 1);
    REQUIRE(Simplex<3>::fromCorner(2).toIndex() == 3);
    REQUIRE(Simplex<3>::fromCorner(3).toIndex() == 4);
    REQUIRE(Simplex<3>::fromCorner(4).toIndex() == 9);
    REQUIRE(Simplex<3>::fromCorner(5).toIndex() == 10);
    REQUIRE(Simplex<3>::fromCorner(6).toIndex() == 12);
    REQUIRE(Simplex<3>::fromCorner(7).toIndex() == 13);
}

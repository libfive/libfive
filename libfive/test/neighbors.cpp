/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "catch.hpp"

#include "libfive/render/brep/dc/neighbors.hpp"
#include "libfive/render/brep/neighbor_tables.hpp"

using namespace Kernel;


/*
These test cases are hand-constructed by drawing them out.
Neighbor numbers are as follows (it makes sense in ternary)
   -------------
   | 3 | 5 | 4 |
   -------------
   | 6 | X | 7 |
   -------------
   | 0 | 2 | 1 |
   -------------

XTree children are as follows (based on counting in binary):
  ---------
  | 2 | 3 |
  ---------
  | 0 | 1 |
  ---------

XTree cell corners are as follows (based on counting in binary):
  2-------3
  |       |
  |       |
  |       |
  0-------1
*/

TEST_CASE("NeighborTables<2>::getCorner")
{
    REQUIRE(NeighborTables<2>::getCorner(0, 0).i == 3);
    REQUIRE(NeighborTables<2>::getCorner(0, 1).i == -1);
    REQUIRE(NeighborTables<2>::getCorner(0, 2).i == 2);
    REQUIRE(NeighborTables<2>::getCorner(0, 3).i == -1);
    REQUIRE(NeighborTables<2>::getCorner(0, 4).i == -1);
    REQUIRE(NeighborTables<2>::getCorner(0, 5).i == -1);
    REQUIRE(NeighborTables<2>::getCorner(0, 6).i == 1);
    REQUIRE(NeighborTables<2>::getCorner(0, 7).i == -1);

    REQUIRE(NeighborTables<2>::getCorner(1, 0).i == -1);
    REQUIRE(NeighborTables<2>::getCorner(1, 1).i == 2);
    REQUIRE(NeighborTables<2>::getCorner(1, 2).i == 3);
    REQUIRE(NeighborTables<2>::getCorner(1, 3).i == -1);
    REQUIRE(NeighborTables<2>::getCorner(1, 4).i == -1);
    REQUIRE(NeighborTables<2>::getCorner(1, 5).i == -1);
    REQUIRE(NeighborTables<2>::getCorner(1, 6).i == -1);
    REQUIRE(NeighborTables<2>::getCorner(1, 7).i == 0);

    REQUIRE(NeighborTables<2>::getCorner(2, 0).i == -1);
    REQUIRE(NeighborTables<2>::getCorner(2, 1).i == -1);
    REQUIRE(NeighborTables<2>::getCorner(2, 2).i == -1);
    REQUIRE(NeighborTables<2>::getCorner(2, 3).i == 1);
    REQUIRE(NeighborTables<2>::getCorner(2, 4).i == -1);
    REQUIRE(NeighborTables<2>::getCorner(2, 5).i == 0);
    REQUIRE(NeighborTables<2>::getCorner(2, 6).i == 3);
    REQUIRE(NeighborTables<2>::getCorner(2, 7).i == -1);

    REQUIRE(NeighborTables<2>::getCorner(3, 0).i == -1);
    REQUIRE(NeighborTables<2>::getCorner(3, 1).i == -1);
    REQUIRE(NeighborTables<2>::getCorner(3, 2).i == -1);
    REQUIRE(NeighborTables<2>::getCorner(3, 3).i == -1);
    REQUIRE(NeighborTables<2>::getCorner(3, 4).i == 0);
    REQUIRE(NeighborTables<2>::getCorner(3, 5).i == 1);
    REQUIRE(NeighborTables<2>::getCorner(3, 6).i == -1);
    REQUIRE(NeighborTables<2>::getCorner(3, 7).i == 2);
}

TEST_CASE("Neighbors<2>::edgeCheckIndex")
{
    // Just a few spot checks, since we exhaustively test the building blocks
    // in the test above for cornerCheckIndex.
    REQUIRE(Neighbors<2>::edgeCheckIndex({0, 2}, 6) == std::make_pair(1, 3));
    REQUIRE(Neighbors<2>::edgeCheckIndex({2, 0}, 6) == std::make_pair(3, 1));
    REQUIRE(Neighbors<2>::edgeCheckIndex({0, 3}, 6) == std::make_pair(-1, -1));
    REQUIRE(Neighbors<2>::edgeCheckIndex({0, 2}, 1) == std::make_pair(-1, -1));
}

TEST_CASE("NeighborTables<2>::withinTreeIndex")
{
    // These test cases are hand-constructed by drawing them out on paper
    REQUIRE(NeighborTables<2>::withinTreeIndex(0, 0).i == -1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(0, 1).i == -1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(0, 2).i == -1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(0, 3).i == -1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(0, 4).i == 3);
    REQUIRE(NeighborTables<2>::withinTreeIndex(0, 5).i == 2);
    REQUIRE(NeighborTables<2>::withinTreeIndex(0, 6).i == -1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(0, 7).i == 1);

    REQUIRE(NeighborTables<2>::withinTreeIndex(1, 0).i == -1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(1, 1).i == -1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(1, 2).i == -1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(1, 3).i == 2);
    REQUIRE(NeighborTables<2>::withinTreeIndex(1, 4).i == -1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(1, 5).i == 3);
    REQUIRE(NeighborTables<2>::withinTreeIndex(1, 6).i == 0);
    REQUIRE(NeighborTables<2>::withinTreeIndex(1, 7).i == -1);

    REQUIRE(NeighborTables<2>::withinTreeIndex(2, 0).i == -1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(2, 1).i == 1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(2, 2).i == 0);
    REQUIRE(NeighborTables<2>::withinTreeIndex(2, 3).i == -1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(2, 4).i == -1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(2, 5).i == -1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(2, 6).i == -1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(2, 7).i == 3);

    REQUIRE(NeighborTables<2>::withinTreeIndex(3, 0).i == 0);
    REQUIRE(NeighborTables<2>::withinTreeIndex(3, 1).i == -1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(3, 2).i == 1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(3, 3).i == -1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(3, 4).i == -1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(3, 5).i == -1);
    REQUIRE(NeighborTables<2>::withinTreeIndex(3, 6).i == 2);
    REQUIRE(NeighborTables<2>::withinTreeIndex(3, 7).i == -1);
}

TEST_CASE("NeighborTables<2>::neighborTargetIndex")
{
    REQUIRE(NeighborTables<2>::neighborTargetIndex(0, 0).second.i == 3);
    REQUIRE(NeighborTables<2>::neighborTargetIndex(0, 1).second.i == 3);
    REQUIRE(NeighborTables<2>::neighborTargetIndex(0, 2).second.i == 2);
    REQUIRE(NeighborTables<2>::neighborTargetIndex(0, 3).second.i == 3);
    REQUIRE(NeighborTables<2>::neighborTargetIndex(0, 6).second.i == 1);

    REQUIRE(NeighborTables<2>::neighborTargetIndex(1, 0).second.i == 2);
    REQUIRE(NeighborTables<2>::neighborTargetIndex(1, 1).second.i == 2);
    REQUIRE(NeighborTables<2>::neighborTargetIndex(1, 2).second.i == 3);
    REQUIRE(NeighborTables<2>::neighborTargetIndex(1, 4).second.i == 2);
    REQUIRE(NeighborTables<2>::neighborTargetIndex(1, 7).second.i == 0);

    REQUIRE(NeighborTables<2>::neighborTargetIndex(2, 0).second.i == 1);
    REQUIRE(NeighborTables<2>::neighborTargetIndex(2, 3).second.i == 1);
    REQUIRE(NeighborTables<2>::neighborTargetIndex(2, 4).second.i == 1);
    REQUIRE(NeighborTables<2>::neighborTargetIndex(2, 5).second.i == 0);
    REQUIRE(NeighborTables<2>::neighborTargetIndex(2, 6).second.i == 3);

    REQUIRE(NeighborTables<2>::neighborTargetIndex(3, 1).second.i == 0);
    REQUIRE(NeighborTables<2>::neighborTargetIndex(3, 3).second.i == 0);
    REQUIRE(NeighborTables<2>::neighborTargetIndex(3, 4).second.i == 0);
    REQUIRE(NeighborTables<2>::neighborTargetIndex(3, 5).second.i == 1);
    REQUIRE(NeighborTables<2>::neighborTargetIndex(3, 7).second.i == 2);
}

TEST_CASE("NeighborTables<2>::whichNeighbor")
{
    REQUIRE(NeighborTables<2>::whichNeighbor(0, 0).i == 0);
    REQUIRE(NeighborTables<2>::whichNeighbor(0, 1).i == 2);
    REQUIRE(NeighborTables<2>::whichNeighbor(0, 2).i == 2);
    REQUIRE(NeighborTables<2>::whichNeighbor(0, 3).i == 6);
    REQUIRE(NeighborTables<2>::whichNeighbor(0, 6).i == 6);

    REQUIRE(NeighborTables<2>::whichNeighbor(1, 0).i == 2);
    REQUIRE(NeighborTables<2>::whichNeighbor(1, 1).i == 1);
    REQUIRE(NeighborTables<2>::whichNeighbor(1, 2).i == 2);
    REQUIRE(NeighborTables<2>::whichNeighbor(1, 4).i == 7);
    REQUIRE(NeighborTables<2>::whichNeighbor(1, 7).i == 7);

    REQUIRE(NeighborTables<2>::whichNeighbor(2, 0).i == 6);
    REQUIRE(NeighborTables<2>::whichNeighbor(2, 3).i == 3);
    REQUIRE(NeighborTables<2>::whichNeighbor(2, 4).i ==5);
    REQUIRE(NeighborTables<2>::whichNeighbor(2, 5).i == 5);
    REQUIRE(NeighborTables<2>::whichNeighbor(2, 6).i == 6);

    REQUIRE(NeighborTables<2>::whichNeighbor(3, 1).i == 7);
    REQUIRE(NeighborTables<2>::whichNeighbor(3, 3).i == 5);
    REQUIRE(NeighborTables<2>::whichNeighbor(3, 4).i == 4);
    REQUIRE(NeighborTables<2>::whichNeighbor(3, 5).i == 5);
    REQUIRE(NeighborTables<2>::whichNeighbor(3, 7).i == 7);
}

/*
TEST_CASE("Neighbors<2>::neighborTargetIndex")
{
    REQUIRE(Neighbors<2>::neighborTargetIndex(0, 0) == std::make_pair(0,3));
    REQUIRE(Neighbors<2>::neighborTargetIndex(0, 1) == std::make_pair(2,3));
    REQUIRE(Neighbors<2>::neighborTargetIndex(0, 2) == std::make_pair(2,2));
    REQUIRE(Neighbors<2>::neighborTargetIndex(0, 3) == std::make_pair(6,3));
    REQUIRE(Neighbors<2>::neighborTargetIndex(0, 6) == std::make_pair(6,1));

    REQUIRE(Neighbors<2>::neighborTargetIndex(1, 0) ==2);
    REQUIRE(Neighbors<2>::neighborTargetIndex(1, 1) ==1);
    REQUIRE(Neighbors<2>::neighborTargetIndex(1, 2) ==2);
    REQUIRE(Neighbors<2>::neighborTargetIndex(1, 4) ==7);
    REQUIRE(Neighbors<2>::neighborTargetIndex(1, 7) == std::make_pair(7,0));

    REQUIRE(Neighbors<2>::neighborTargetIndex(2, 0) == std::make_pair(6,1));
    REQUIRE(Neighbors<2>::neighborTargetIndex(2, 3) == std::make_pair(3,1));
    REQUIRE(Neighbors<2>::neighborTargetIndex(2, 4) == std::make_pair(5,1));
    REQUIRE(Neighbors<2>::neighborTargetIndex(2, 5) == std::make_pair(5,0));
    REQUIRE(Neighbors<2>::neighborTargetIndex(2, 6) == std::make_pair(6,3));

    REQUIRE(Neighbors<2>::neighborTargetIndex(3, 1) == std::make_pair(7,0));
    REQUIRE(Neighbors<2>::neighborTargetIndex(3, 3) == std::make_pair(5,0));
    REQUIRE(Neighbors<2>::neighborTargetIndex(3, 4) == std::make_pair(4,0));
    REQUIRE(Neighbors<2>::neighborTargetIndex(3, 5) == std::make_pair(5,1));
    REQUIRE(Neighbors<2>::neighborTargetIndex(3, 7) == std::make_pair(7,2));
}
*/

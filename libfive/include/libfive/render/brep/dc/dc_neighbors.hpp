/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <array>
#include <atomic>
#include <memory>

#include "libfive/render/brep/dc/intersection.hpp"
#include "libfive/render/brep/neighbors.hpp"
#include "libfive/render/brep/indexes.hpp"
#include "libfive/eval/interval.hpp"

namespace libfive {

// Forward declaration
template <unsigned N> class DCTree;
class PseudoDCTree2;

template <unsigned N>
class DCNeighbors : public Neighbors<N, DCTree<N>, DCNeighbors<N>>
{
public:
    /*
     *  Constructor, returning an empty neighbor array
     */
    DCNeighbors() : Neighbors<N, DCTree<N>, DCNeighbors<N>>() {}

    /*
     *  Looks up the given corner to see if it has already been calculated
     *  by any of the neighbors, returning FILLED / EMPTY if that is the case
     *  and UNKNOWN otherwise.
     */
    Interval::State check(CornerIndex corner) const;

    /*
     *  Checks the given corner against all neighbors that contain it.
     *  If any of them don't match, then return that tree + corner index.
     */
    std::pair<const DCTree<N>*, unsigned> checkConsistency(
            CornerIndex corner, Interval::State i) const;

    /*
     *  Looks up the given edge to see if it has been calculated by any
     *  of the neighbors, assigning the pointer if that is the case
     *  and setting it to nullptr otherwise.
     */
    Intersection<N>* check(uint8_t a, uint8_t b) const;
};

template <unsigned N>
struct NeighborHelper<DCTree<N>> {
    using NeighborType = DCNeighbors<N>;
};

class PseudoDCNeighbors2 : public Neighbors<2, PseudoDCTree2, PseudoDCNeighbors2>
{
public:
    /*
     *  Constructor, returning an empty neighbor array
     */
    PseudoDCNeighbors2() : Neighbors<2, PseudoDCTree2, PseudoDCNeighbors2>() {}

    /*
     *  Looks up the given corner to see if it has already been calculated
     *  by any of the neighbors, returning FILLED / EMPTY if that is the case
     *  and UNKNOWN otherwise.
     */
    Interval::State check(CornerIndex corner) const;

    /*
     *  Checks the given corner against all neighbors that contain it.
     *  If any of them don't match, then return that tree + corner index.
     */
    std::pair<const PseudoDCTree2*, unsigned> checkConsistency(
            CornerIndex corner, Interval::State i) const;

    /*
     *  Looks up the given edge to see if it has been calculated by any
     *  of the neighbors, assigning the pointer if that is the case
     *  and setting it to nullptr otherwise.
     */
    Intersection<2>* check(uint8_t a, uint8_t b) const;
};

template<>
struct NeighborHelper<PseudoDCTree2> {
    using NeighborType = PseudoDCNeighbors2;
};

}   // namespace libfive

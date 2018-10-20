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

#include "libfive/render/brep/dc/intersection.hpp"
#include "libfive/render/brep/neighbors.hpp"
#include "libfive/eval/interval.hpp"

namespace Kernel {

// Forward declaration
template <unsigned N> class XTree;

template <unsigned N>
class DCNeighbors : public Neighbors<N, XTree<N>, DCNeighbors<N>>
{
public:
    /*
     *  Constructor, returning an empty neighbor array
     */
    DCNeighbors() : Neighbors<N, XTree<N>, DCNeighbors<N>>() {}

    /*
     *  Looks up the given corner to see if it has already been calculated
     *  by any of the neighbors, returning FILLED / EMPTY if that is the case
     *  and UNKNOWN otherwise.
     */
    Interval::State check(uint8_t corner) const;

    /*
     *  Looks up the given edge to see if it has been calculated by any
     *  of the neighbors, assigning the pointer if that is the case
     *  and setting it to nullptr otherwise.
     */
    std::shared_ptr<IntersectionVec<N>> check(uint8_t a, uint8_t b) const;
};

//  We explicitly instantiate the Neighbors classes in neighbors.cpp
extern template class DCNeighbors<2>;
extern template class DCNeighbors<3>;

}   // namespace Kernel

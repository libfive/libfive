/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <atomic>
#include <array>
#include "libfive/render/brep/util.hpp"

namespace libfive {

// N = number of dimensions
// T = leaf node type
// C = CRTP template parameter (so push can return the derived type)
template <unsigned N, typename T, typename C>
class Neighbors
{
public:
    /*  Constructor, returning an empty neighbor array
     */
    Neighbors();

    /*
     *  Returns the neighbors of a particular quad/octree child,
     *  given the child's index and the array of other children.
     */
    C push(uint8_t child,
           const std::array<std::atomic<T*>, 1 << N>& children) const;

protected:
    std::array<const T*, ipow(3, N) - 1> neighbors;
};

}   // namespace libfive

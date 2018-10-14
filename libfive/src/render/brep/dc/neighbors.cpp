/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "libfive/render/brep/dc/neighbors.hpp"
#include "libfive/render/brep/dc/xtree.hpp"

namespace Kernel {

// Here are all of our static variables
template <unsigned N> std::array<uint8_t, ipow(3, N)-1> Neighbors<N>::fixed;
template <unsigned N> std::array<uint8_t, ipow(3, N)-1> Neighbors<N>::floating;
template <unsigned N> std::array<uint8_t, 1 << (2 * N)> Neighbors<N>::remap;
template <unsigned N> bool Neighbors<N>::loaded = Neighbors<N>::populatePositions();

template <unsigned N>
Neighbors<N>::Neighbors() {
    std::fill(neighbors.begin(), neighbors.end(), nullptr);
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
int Neighbors<N>::cornerCheckIndex(uint8_t corner, uint8_t neighbor)
{
    assert(corner < (1 << N));
    assert(neighbor < ipow(3, N) - 1);

    return ((fixed[neighbor] & invert(floating[neighbor])) ==
            (corner & invert(floating[neighbor])))
        ? ((invert(corner) & (~floating[neighbor])) |
           (corner         &   floating[neighbor]))
        : -1;
}


template <unsigned N>
std::pair<int, int> Neighbors<N>::edgeCheckIndex(
    std::pair<int, int> edge, uint8_t neighbor)
{
    const auto a = cornerCheckIndex(edge.first, neighbor);
    const auto b = cornerCheckIndex(edge.second, neighbor);

    if (a == -1 || b == -1)
    {
        return {-1, -1};
    }
    else
    {
        return {a, b};
    }
}

template <unsigned N>
int Neighbors<N>::withinTreeIndex(uint8_t child, uint8_t neighbor)
{
    assert(child < (1 << N));
    assert(neighbor < ipow(3, N) - 1);
    return (((fixed[neighbor] ^ child) & invert(floating[neighbor])) == invert(floating[neighbor]))
        ? (fixed[neighbor] | (floating[neighbor] & child))
        : -1;
}

template <unsigned N>
std::pair<int, int> Neighbors<N>::neighborTargetIndex(uint8_t child,
                                                      uint8_t neighbor)
{
    // Figure out which higher-level neighbor we should index into
    auto target_floating = floating[neighbor] | (child ^ fixed[neighbor]);
    auto target_fixed = child & invert(target_floating);

    // Look up the index of this higher-level neighbor
    auto index = (target_fixed << N) | target_floating;
    int j = remap[index];
    assert(j != 0xFF);

    return {j, (invert(child) & invert(floating[neighbor])) |
               (child         &        floating[neighbor])};
}

template <unsigned N>
bool Neighbors<N>::populatePositions() {
    // Early exit if already populated
    std::fill(remap.begin(), remap.end(), 0xFF);

    for (unsigned i=0; i < ipow(3, N) - 1; ++i)
    {
        // Ternary decoding loop
        unsigned j=i;
        for (unsigned axis=0; axis < N; ++axis)
        {
            switch (j % 3) {
                case 1: fixed[i]    |= (1 << axis); break;
                case 2: floating[i] |= (1 << axis); break;
                default: break;
            }
            j /= 3;
        }

        // Store the remapping lookup here as well
        remap[(fixed[i] << N) | floating[i]] = i;
    }
    return true;
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
Neighbors<N> Neighbors<N>::push(uint8_t child,
        const std::array<std::atomic<XTree<N>*>, 1 << N>&
            children)
{
    Neighbors out;
    for (unsigned i=0; i < ipow(3, N) - 1; ++i)
    {
        // If the neighbor is destined to come from within the array
        // of children, then pick it out in this conditional.
        auto within = withinTreeIndex(child, i);
        if (within != -1) {
            out.neighbors[i] = children[within].load();
        }
        else
        {
            auto target = neighborTargetIndex(child, i);
            if (neighbors[target.first])
            {
                out.neighbors[i] = neighbors[target.first]->
                       children[target.second].load();
            }
        }
    }
    return out;
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
Interval::State Neighbors<N>::check(uint8_t corner) const
{
    for (unsigned i=0; i < ipow(N, 3) - 1; ++i)
    {
        if (neighbors[i] != nullptr)
        {
            auto index = cornerCheckIndex(corner, i);
            if (index != -1)
            {
                return neighbors[i]->cornerState(index);
            }
        }
    }
    return Interval::UNKNOWN;
}

template <unsigned N>
std::shared_ptr<IntersectionVec<N>> Neighbors<N>::check(
        uint8_t a, uint8_t b) const
{
    for (unsigned i=0; i < ipow(N, 3) - 1; ++i)
    {
        if (neighbors[i] != nullptr)
        {
            auto index = edgeCheckIndex({a, b}, i);
            if (index.first != -1)
            {
                return neighbors[i]->intersection(index.first, index.second);
            }
        }
    }
    return nullptr;
}


// Explicit initialization of template
template class Neighbors<2>;
template class Neighbors<3>;

}   // namespace Kernel

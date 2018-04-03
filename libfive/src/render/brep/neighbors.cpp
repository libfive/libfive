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
#include "libfive/render/brep/neighbors.hpp"
#include "libfive/render/brep/xtree.hpp"

namespace Kernel {

// Here are all of our static variables
template <unsigned N> std::array<uint8_t, _pow(3, N)-1> Neighbors<N>::fixed;
template <unsigned N> std::array<uint8_t, _pow(3, N)-1> Neighbors<N>::floating;
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
    assert(neighbor < _pow(3, N) - 1);

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
    assert(neighbor < _pow(3, N) - 1);
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

    for (unsigned i=0; i < _pow(3, N) - 1; ++i)
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
        const std::array<std::unique_ptr<const XTree<N>>, 1 << N>&
            children)
{
    Neighbors out;
    for (unsigned i=0; i < _pow(3, N) - 1; ++i)
    {
        // If the neighbor is destined to come from within the array
        // of children, then pick it out in this conditional.
        auto within = withinTreeIndex(child, i);
        if (within != -1) {
            out.neighbors[i] = children[within].get();
        }
        else
        {
            auto target = neighborTargetIndex(child, i);
            if (neighbors[target.first])
            {
                out.neighbors[i] = neighbors[target.first]->
                       children[target.second].get();
            }
        }
    }
    return out;
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
Interval::State Neighbors<N>::check(uint8_t corner)
{
    for (unsigned i=0; i < _pow(N, 3) - 1; ++i)
    {
        if (neighbors[i] != nullptr)
        {
            auto index = cornerCheckIndex(corner, i);
            if (index != -1)
            {
                return neighbors[i]->corners[index];
            }
        }
    }
    return Interval::UNKNOWN;
}

template <unsigned N>
const IntersectionVec<N>* Neighbors<N>::check(uint8_t a, uint8_t b)
{
    for (unsigned i=0; i < _pow(N, 3) - 1; ++i)
    {
        if (neighbors[i] != nullptr)
        {
            auto index = edgeCheckIndex({a, b}, i);
            if (index.first != -1)
            {
                return &neighbors[i]->intersection(a, b);
            }
        }
    }
    return nullptr;
}


// Explicit initialization of template
template class Neighbors<2>;
template class Neighbors<3>;

}   // namespace Kernel

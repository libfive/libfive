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
#pragma once

#include <array>

#include "libfive/render/brep/marching.hpp"
#include "libfive/eval/interval.hpp"

namespace Kernel {

// Forward declaration
template <unsigned N> class XTree;

/*
 *  Hopefully, the bitmask operations are correct and no one ever needs
 *  to debug this file ever.  To generate the bitmasks, I drew out a lot
 *  of pictures of the 2D case, then thought really hard about whether
 *  it generalized to 3D.
 *
 *  There are more details in the unit tests, including numbering out
 *  all of the 2D cases.
 */
template <unsigned N>
class Neighbors
{
public:
    /*
     *  Constructor, returning an empty neighbor array
     */
    Neighbors() {
        populatePositions();
        std::fill(neighbors.begin(), neighbors.end(), nullptr);
    }

    /*
     *  Returns the XTree corner index that matches the given corner index
     *  in the given neighbor, or -1 if no such match is possible.
     */
    static int cornerCheckIndex(uint8_t corner, uint8_t neighbor)
    {
        populatePositions();

        assert(corner < (1 << N));
        assert(neighbor < _pow(3, N) - 1);

        return ((fixed[neighbor] & invert(floating[neighbor])) ==
                (corner & invert(floating[neighbor])))
            ? ((invert(corner) & (~floating[neighbor])) |
               (corner         &   floating[neighbor]))
            : -1;
    }

    /*
     *  Looks up the given corner to see if it has already been calculated
     *  by any of the neighbors, returning FILLED / EMPTY if that is the case
     *  and UNKNOWN otherwise.
     */
    Interval::State check(uint8_t corner)
    {
        for (unsigned i=0; i < _pow(i, 3) - 1; ++i)
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

    /*
     *  Given an XTree child index, returns the XTree child index of the
     *  given neighbor (if it is within the same XTree), or -1 otherwise.
     *
     *  For example, in 2D:
     *
     *  -------------
     *  |     |     |
     *  -------------
     *  |  0  |     |
     *  -------------
     *
     *  Calling withinTreeIndex(0b00, 0t11) should return 0b11,
     *  since that's the XTree child index that contains the neighbor
     *  0t11 (i.e. 11 in ternary).
     */
    static int withinTreeIndex(uint8_t child, uint8_t neighbor)
    {
        populatePositions();

        assert(child < (1 << N));
        assert(neighbor < _pow(3, N) - 1);
        return (((fixed[neighbor] ^ child) & invert(floating[neighbor])) == invert(floating[neighbor]))
            ? (fixed[neighbor] | (floating[neighbor] & child))
            : -1;
    }

    /*
     *  Given an XTree child index, returns a pair of
     *      [neighbor index, XTree child index]
     *  for the given neighbor (if it is within the same XTree)
     *
     *  For example, in 2D:
     *
     *  ------------------------
     *  |     :    |     |     |
     *  | - - - -  -------------
     *  |     : !  |  0  |     |
     *  ------------------------
     *
     *  Calling withinTreeIndex(0b00, 0t20) should return {0t20, 0b01}
     *  since that's the XTree child index that contains the neighbor
     *  0t11 (i.e. 11 in ternary).
     */
    static std::pair<int, int> neighborTargetIndex(uint8_t child,
                                                   uint8_t neighbor)
    {
        populatePositions();

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


    Neighbors<N> push(uint8_t child,
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

protected:
    static void populatePositions() {
        // Early exit if already populated
        if (loaded)
        {
            return;
        }
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
        loaded = true;
    }

    static constexpr uint8_t invert(uint8_t in) {
        return (~in) & mask();
    }
    static constexpr uint8_t mask() {
        return (1 << N) - 1;
    }

    /*  bitfield representing direction for non-floating axes
     *  floating axes have their relevant bit set to 0, but you
     *  need to use the floating field to decode */
    static std::array<uint8_t, _pow(3, N) - 1> fixed;

    /*  bitfield representing which axes are floating  */
    static std::array<uint8_t, _pow(3, N) - 1> floating;

    static bool loaded;

    /*  remap[(fixed << N) | floating] returns the index into
     *  the fixed/floating arrays with the given bitfields.  */
    static std::array<uint8_t, 1 << (2 * N)> remap;

    std::array<const XTree<N>*, _pow(3, N) - 1> neighbors;
};

//  We explicitly instantiate the Neighbors classes in neighbors.cpp
extern template class Neighbors<2>;
extern template class Neighbors<3>;

}   // namespace Kernel

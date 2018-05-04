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
#include "libfive/render/ipow.hpp"

namespace Kernel {

enum SimplexCorner {
    SIMPLEX_CORNER_LOWER = -1,
    SIMPLEX_CORNER_SPANS =  0,
    SIMPLEX_CORNER_UPPER =  1,
};

template <unsigned N>
struct Simplex : std::array<SimplexCorner, N>
{
    static Simplex fromIndex(unsigned index)
    {
        Simplex<N> out;
        assert(index < ipow(3, N));
        for (unsigned j=0; j < N; ++j)
        {
            switch (index % 3)
            {
                case 0: out[j] = SIMPLEX_CORNER_LOWER; break;
                case 1: out[j] = SIMPLEX_CORNER_UPPER; break;
                case 2: out[j] = SIMPLEX_CORNER_SPANS; break;
            }
            index /= 3;
        }
        return out;
    }

    unsigned toIndex() const
    {
        unsigned out = 0;
        for (unsigned j=0; j < N; ++j)
        {
            out *= 3;
            switch ((*this)[N - j - 1])
            {
                case SIMPLEX_CORNER_LOWER: out += 0; break;
                case SIMPLEX_CORNER_UPPER: out += 1; break;
                case SIMPLEX_CORNER_SPANS: out += 2; break;
            }
        }
        return out;
    }

    static Simplex fromCorner(unsigned corner)
    {
        Simplex<N> out;
        assert(corner < ipow(2, N));
        for (unsigned j=0; j < N; ++j)
        {
            switch (corner % 2)
            {
                case 0: out[j] = SIMPLEX_CORNER_LOWER; break;
                case 1: out[j] = SIMPLEX_CORNER_UPPER; break;
            }
            corner /= 2;
        }
        return out;
    }

    bool containsCorner(unsigned index) const
    {
        assert(index < ipow(2, N));
        bool included = true;
        for (unsigned a=0; a < N; ++a)
        {
            switch ((*this)[a])
            {
                case SIMPLEX_CORNER_SPANS:  break;
                case SIMPLEX_CORNER_LOWER:  included &= (index & (1 << a)) == 0;
                                            break;
                case SIMPLEX_CORNER_UPPER:  included &= (index & (1 << a)) != 0;
                                            break;
            }
        }
        return included;
    }

    /*
     *  Checks whether the given index is a sub-simplex
     *
     *  For example, if this simplex is a face on a cube,
     *  that face's edges and corners are sub-simplexes
     *  (but a cube corner that isn't on the face is not).
     */
    bool containsSimplex(const Simplex<N>& other) const
    {
        bool included = true;
        for (unsigned a=0; a < N; ++a)
        {
            switch ((*this)[a])
            {
                case SIMPLEX_CORNER_SPANS:  break;
                case SIMPLEX_CORNER_LOWER:  // fallthrough
                case SIMPLEX_CORNER_UPPER: included &= (other[a] == (*this)[a]);
                                           break;
            }
        }
        return included;
    }

    /*
     *  Returns the number of free axes
     *  (i.e. the number of axes marked as SPANS)
     */
    unsigned freeAxes(void) const
    {
        unsigned out = 0;
        for (auto& i : *this)
        {
            out += (i == SIMPLEX_CORNER_SPANS);
        }
        return out;
    }

    /*
     *  Returns the union of two simplexes
     */
    static Simplex<N> merge(const Simplex<N>& a, const Simplex<N>& b)
    {
        Simplex<N> out;
        for (unsigned i=0; i < N; ++i)
        {
            out[i] = (a[i] == b[i]) ? a[i] : SIMPLEX_CORNER_SPANS;
        }
        return out;
    }

};

}   // namespace Kernel

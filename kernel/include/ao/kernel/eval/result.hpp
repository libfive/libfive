/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#pragma once

#include <array>
#include <vector>

#include "ao/kernel/eval/interval.hpp"
#include "ao/kernel/eval/clause.hpp"

#ifdef __AVX__
#include <immintrin.h>

// AVX data needs to be aligned on 32-byte boundaries.
// This isn't the default on certain OSs, so we make a custom allocator
// here that uses _mm_malloc and _mm_free.
template <class T>
struct _AlignedAllocator {
    typedef T value_type;
    _AlignedAllocator() noexcept {}

    template <class U>
    _AlignedAllocator(const _AlignedAllocator<U>& /*other*/) throw() {}

    T* allocate(std::size_t n)
        { return static_cast<T*>(_mm_malloc(n * sizeof(T), 32)); }
    void deallocate(T* p, std::size_t /*n*/)
        { _mm_free(p); }
};

template <typename T, typename U>
inline bool operator==(const _AlignedAllocator<T>& /*a*/,
                       const _AlignedAllocator<U>& /*b*/)
    { return true; }

template <typename T, typename U>
inline bool operator!=(const _AlignedAllocator<T>& a,
                       const _AlignedAllocator<U>& b)
    { return !(a == b); }
#endif


struct Result {
    typedef Clause::Id Index;

    /*
     *  Prepares the given number of clauses
     */
    void resize(Index clauses, Index vars=0);

    /*
     *  Sets all of the values to the given constant float
     *  (across the Interval, and float / __m256 arrays)
     *
     *  Gradients are set to {0, 0, 0}
     *  Gradient is set to 0
     */
    void fill(float v, Index clause);

    /*
     *  Sets all of the values to the given constant float
     *  (across the Interval, and float / __m256 arrays)
     */
    void setValue(float v, Index clause);

    /*
     *  Marks that j[clause][var] = 1
     */
    void setGradient(Index clause, Index var);

    /*
     *  Fills the derivative arrays with the given values
     */
    void setDeriv(float x, float y, float z, Index clause);

    // This is the number of samples that we can process in one pass
    static constexpr Index N = 256;

    // If we're using AVX for evaluation, then our floats are simply
    // pointers to the first member of the __m256 array
#ifdef __AVX__

    typedef std::array<__m256, N/8> AVXArray;
    typedef std::vector<AVXArray, _AlignedAllocator<AVXArray>> AVXVector;

public:
    AVXVector mf;
    AVXVector mdx;
    AVXVector mdy;
    AVXVector mdz;

    float (*f)[N];
    float (*dx)[N];
    float (*dy)[N];
    float (*dz)[N];
#else
    std::vector<std::array<float, N>> f;
    std::vector<std::array<float, N>> dx;
    std::vector<std::array<float, N>> dy;
    std::vector<std::array<float, N>> dz;
#endif
    /*  j[clause][var] = dclause / dvar */
    std::vector<std::vector<float>> j;

    std::vector<Interval> i;
};

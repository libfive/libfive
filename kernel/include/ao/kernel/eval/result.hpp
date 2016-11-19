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

#include <immintrin.h>

#include "ao/kernel/eval/interval.hpp"
#include "ao/kernel/eval/clause.hpp"

struct Result {
    typedef uint32_t Index;

    /*
     *  Prepares the given number of clauses
     */
    void resize(Clause::Id clauses);

    /*
     *  Sets all of the values to the given constant float
     *  (across the Interval, float, Gradient, and __m256 arrays)
     *
     *  Gradients are set to {0, 0, 0}
     */
    void fill(float v, Clause::Id clause);

    /*
     *  Fills the derivative arrays with the given values
     */
    void deriv(float x, float y, float z, Clause::Id clause);

    // This is the number of samples that we can process in one pass
    static constexpr Index N = 256;

    // If we're using AVX for evaluation, then our floats are simply
    // pointers to the first member of the __m256 array
#ifdef __AVX__
    std::vector<std::array<__m256, N/8>> mf;
    std::vector<std::array<__m256, N/8>> mdx;
    std::vector<std::array<__m256, N/8>> mdy;
    std::vector<std::array<__m256, N/8>> mdz;

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
    std::vector<Interval> i;
};

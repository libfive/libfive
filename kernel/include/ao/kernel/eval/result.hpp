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

struct Result {
    /*
     *  Initialize array pointers
     */
    Result();

    /*
     *  Look up a particular value by index
     */
    float& operator[](size_t index) { return f[index]; }

    /*
     *  Sets a particular value in the array
     *  (inlined for efficiency)
     */
    void set(float v, size_t index)
    {
        f[index] = v;
    }

    /*
     *  Sets the interval value in the array
     */
    void set(Interval V);

    /*
     *  Returns the float at the given index
     */
    float get(size_t index) const { return f[index]; }

    /*
     *  Sets all of the values to the given constant float
     *  (across the Interval, float, Gradient, and __m256 arrays)
     *
     *  Gradients are set to {0, 0, 0}
     */
    void fill(float v);

    /*
     *  Fills the derivative arrays with the given values
     */
    void deriv(float x, float y, float z);

    // This is the number of samples that we can process in one pass
    static constexpr size_t N = 256;

protected:

    // If we're using AVX for evaluation, then our floats are simply
    // pointers to the first member of the __m256 array
    float  f[N];
    float dx[N];
    float dy[N];
    float dz[N];

    Interval i;

    friend class Evaluator;
    friend class Clause;
};

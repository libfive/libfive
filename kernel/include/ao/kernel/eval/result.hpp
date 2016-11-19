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
     *  Look up a particular value by index
     */
    float& f(Clause::Id clause, Index index)
    { return _f[clause][index]; }

    float& dx(Clause::Id clause, Index index)
    { return _dx[clause][index]; }
    float& dy(Clause::Id clause, Index index)
    { return _dy[clause][index]; }
    float& dz(Clause::Id clause, Index index)
    { return _dz[clause][index]; }

#ifdef __AVX__
    __m256& mf(Clause::Id clause, Index index)
    { return _mf[clause][index]; }

    __m256& mdx(Clause::Id clause, Index index)
    { return _mdx[clause][index]; }
    __m256& mdy(Clause::Id clause, Index index)
    { return _mdy[clause][index]; }
    __m256& mdz(Clause::Id clause, Index index)
    { return _mdz[clause][index]; }
#endif

    /*
     *  Sets a particular value in the array
     */
    void set(float v, Clause::Id clause, Index index)
    { _f[clause][index] = v; }

    /*
     *  Sets the interval value in the array
     */
    void set(const Interval& V, Clause::Id clause)
    { _i[clause] = V; }

    /*
     *  Returns the float at the given index
     */
    float get(Clause::Id clause, Index index) const
    { return _f[clause][index]; }

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

protected:

    // If we're using AVX for evaluation, then our floats are simply
    // pointers to the first member of the __m256 array
#ifdef __AVX__
    std::vector<std::array<__m256, N/8>> _mf;
    std::vector<std::array<__m256, N/8>> _mdx;
    std::vector<std::array<__m256, N/8>> _mdy;
    std::vector<std::array<__m256, N/8>> _mdz;

    float (*_f)[N];
    float (*_dx)[N];
    float (*_dy)[N];
    float (*_dz)[N];
#else
    std::vector<std::array<float, N>> _f;
    std::vector<std::array<float, N>> _dx;
    std::vector<std::array<float, N>> _dy;
    std::vector<std::array<float, N>> _dz;
#endif
    std::vector<Interval> _i;

    friend class Evaluator;
};

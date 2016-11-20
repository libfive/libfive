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
#include <unordered_map>
#include <vector>

#include <glm/mat4x4.hpp>

#include "ao/kernel/eval/result.hpp"
#include "ao/kernel/eval/interval.hpp"
#include "ao/kernel/eval/clause.hpp"
#include "ao/kernel/tree/tree.hpp"

////////////////////////////////////////////////////////////////////////////////

class Evaluator
{
public:
    /*
     *  Construct an evaluator for the given tree
     */
    Evaluator(const Tree root, const glm::mat4& M=glm::mat4());

    /*
     *  Single-argument evaluation
     */
    float eval(float x, float y, float z);
    Interval eval(Interval x, Interval y, Interval z);

    /*
     *  Evaluates a set of floating-point results
     *  (which have been loaded with set)
     */
#ifdef __AVX__
    const float* values(Result::Index count, bool vectorize=true);
#else
    const float* values(Result::Index count);
#endif

    /*
     *  Evaluate a set of gradients, returning a tuple
     *      value, dx, dy, dz
     *
     *  Values must have been previously loaded by set
     */
#ifdef __AVX__
    std::tuple<const float*, const float*,
               const float*, const float*> derivs(Result::Index count,
                                                  bool vectorize=true);
#else
    std::tuple<const float*, const float*,
               const float*, const float*> derivs(Result::Index count);
#endif

    /*
     *  Evaluates a single interval (stored with set)
     */
    Interval interval();

    /*
     *  Stores the given value in the result arrays
     *  (inlined for efficiency)
     */
    void set(float x, float y, float z, Result::Index index)
    {
        result.f[X][index] = M[0][0] * x + M[1][0] * y + M[2][0] * z + M[3][0];
        result.f[Y][index] = M[0][1] * x + M[1][1] * y + M[2][1] * z + M[3][1];
        result.f[Z][index] = M[0][2] * x + M[1][2] * y + M[2][2] * z + M[3][2];
    }

    /*
     *  Unsafe setter (which requires a call to applyTransform afterwards)
     */
    void setRaw(float x, float y, float z, Result::Index index)
    {
        result.f[X][index] = x;
        result.f[Y][index] = y;
        result.f[Z][index] = z;
    }

    /*
     *  Applies M to values stored by set
     */
    void applyTransform(Result::Index count);

    /*
     *  Stores the given interval in the result objects
     */
    void set(Interval X, Interval Y, Interval Z);

    /*
     *  Pushes into a subinterval, disabling inactive nodes
     */
    void push();

    /*
     *  Pops out of interval evaluation, re-enabling disabled nodes
     */
    void pop();

    /*
     *  Returns the fraction active / total nodes
     *  (to check how well disabling is working)
     */
    double utilization() const;

    /*
     *  Sets the global matrix transform
     *  Invalidates all positions and results
     */
    void setMatrix(const glm::mat4& m);

protected:
    /*  Global matrix transform (and inverse) applied to all coordinates  */
    glm::mat4 M;
    glm::mat4 Mi;

    /*  Indices of X, Y, Z coordinates */
    uint32_t X, Y, Z;

    /*  Tape containing our opcodes in reverse order */
    typedef std::vector<Clause> Tape;
    std::list<Tape> tapes;
    std::list<Tape>::iterator tape;

    std::vector<uint8_t> disabled;

    Result result;
};

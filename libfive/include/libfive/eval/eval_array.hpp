/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

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

#include <Eigen/Eigen>

#include "libfive/eval/base.hpp"

namespace Kernel {

class ArrayEvaluator : public BaseEvaluator
{
public:
    ArrayEvaluator(std::shared_ptr<Tape> t);
    ArrayEvaluator(std::shared_ptr<Tape> t,
                   const std::map<Tree::Id, float>& vars);

    /*
     *  Single-point evaluation
     */
    float eval(const Eigen::Vector3f& pt);
    float evalAndPush(const Eigen::Vector3f& pt);

    /*
     *  Stores the given value in the result arrays
     *  (inlined for efficiency)
     */
    void set(const Eigen::Vector3f& p, size_t index)
    {
        f(tape->X, index) = p.x();
        f(tape->Y, index) = p.y();
        f(tape->Z, index) = p.z();
        points(index) = p;
        for (auto oracle : tape->oracles)
        {
            f(oracle.first, index) = oracle.second.first->getValue(p);
        }
    }

    /*  This is the number of samples that we can process in one pass */
    static constexpr size_t N=256;

protected:
    /*  Stored in values() and used in operator() to decide how much of the
     *  array we're addressing at once  */
    size_t count;

    /*  f(clause, index) is a specific data point */
    Eigen::Array<float, Eigen::Dynamic, N, Eigen::RowMajor> f;

    /*  Stores the points evaluated at each index. */
    Eigen::Array<Eigen::Vector3f, 1, N> points;

    /*  ambig(index) returns whether a particular slot is ambiguous */
    Eigen::Array<bool, 1, N> ambig;

    /*
     *  Per-clause evaluation, used in tape walking
     */
    void operator()(Opcode::Opcode op, Clause::Id id,
                    Clause::Id a, Clause::Id b);
public:
    /*
     *  Multi-point evaluation (values must be stored with set)
     */
    Eigen::Block<decltype(f), 1, Eigen::Dynamic> values(size_t count);

    /*
     *  Changes a variable's value
     *
     *  If the variable isn't present in the tree, does nothing
     *  Returns true if the variable's value changes
     */
    bool setVar(Tree::Id var, float value);

    /*
     *  Returns a list of ambiguous items from indices 0 to i
     *
     *  This call performs O(i) work to set up the ambig array
     */
    Eigen::Block<decltype(ambig), 1, Eigen::Dynamic> getAmbiguous(size_t i);

    /*  Make an aligned new operator, as this class has Eigen structs
     *  inside of it (which are aligned for SSE) */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    friend class Tape; // for rwalk<ArrayEvaluator>
};

}   // namespace Kernel


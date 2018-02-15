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
#include "libfive/eval/interval.hpp"

// Make intervals interoperate with Eigen arrays
namespace Eigen {
using namespace Kernel;

template<> struct NumTraits<Interval::I>
    : GenericNumTraits<Interval::I>
{
    typedef Interval::I Real;
    typedef Interval::I NonInteger;
    typedef Interval::I Nested;
    enum {
        IsComplex = 0,
        IsInteger = 0,
        IsSigned = 1,
        RequireInitialization = 1,
        ReadCost = 1,
        AddCost = 3,
        MulCost = 6
    };
};
}   // namespace Eigen

namespace Kernel {

class IntervalArrayEvaluator : public BaseEvaluator
{
public:
    IntervalArrayEvaluator(std::shared_ptr<Tape> t);
    IntervalArrayEvaluator(std::shared_ptr<Tape> t,
                           const std::map<Tree::Id, float>& vars);

    /*
     *  Interval evaluation
     */
    Interval::I eval(const Eigen::Vector3f& lower,
                     const Eigen::Vector3f& upper);
    Interval::I evalAndPush(const Eigen::Vector3f& lower,
                            const Eigen::Vector3f& upper);

protected:
    /*  This is the number of samples that we can process in one pass */
    static constexpr size_t N=8;

    /*  i(clause, index) is a specific data point */
    Eigen::Array<Interval::I, Eigen::Dynamic, N, Eigen::RowMajor> i;

    /*
     *  Per-clause evaluation, used in tape walking
     */
    void operator()(Opcode::Opcode op, Clause::Id id,
                    Clause::Id a, Clause::Id b);

public:
    /*
     *  Get a particular evaluated value
     */
    Interval::I get(size_t j) { return i(j); }

    /*
     *  Changes a variable's value
     *
     *  If the variable isn't present in the tree, does nothing
     *  Returns true if the variable's value changes
     */
    bool setVar(Tree::Id var, float value);

    /*  Make an aligned new operator, as this class has Eigen structs
     *  inside of it (which are aligned for SSE) */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    friend class Tape; // for rwalk<IntervalEvaluator>
};

}   // namespace Kernel


/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <Eigen/Eigen>

#include "libfive/eval/base.hpp"
#include "libfive/eval/deck.hpp"
#include "libfive/eval/eval_array_size.hpp"

namespace Kernel {

class ArrayEvaluator : public BaseEvaluator
{
public:
    ArrayEvaluator(std::shared_ptr<Deck> t);
    ArrayEvaluator(std::shared_ptr<Deck> t,
                   const std::map<Tree::Id, double>& vars);

    /*
     *  Stores the given value in the result arrays
     *  (inlined for efficiency)
     */
    void set(const Eigen::Vector3d& p, size_t index)
    {
        f(deck->X, index) = p.x();
        f(deck->Y, index) = p.y();
        f(deck->Z, index) = p.z();

        for (auto& o : deck->oracles)
        {
            o->set(p, index);
        }
    }

    /*  This is the number of samples that we can process in one pass */
    static constexpr size_t N=LIBFIVE_EVAL_ARRAY_SIZE;

protected:
    /*  Stored in values() and used in operator() to decide how much of the
     *  array we're addressing at once  */
    size_t count;

    /*  f(clause, index) is a specific data point */
    Eigen::Array<double, Eigen::Dynamic, N, Eigen::RowMajor> f;

    /*  ambig(index) returns whether a particular slot is ambiguous */
    Eigen::Array<bool, 1, N> ambig;

    /*
     *  Per-clause evaluation, used in tape walking
     */
    void operator()(Opcode::Opcode op, Clause::Id id,
                    Clause::Id a, Clause::Id b);

    /*
     *  Sets this->count to count, rounding up to the appropriate SIMD
     *  block size (because Eigen sometimes returns different results
     *  depending on whether it took the SIMD or non-SIMD path).
     */
    void setCount(size_t count);

public:
    /*
     *  Multi-point evaluation (values must be stored with set)
     */
    Eigen::Block<decltype(f), 1, Eigen::Dynamic> values(size_t count);
    Eigen::Block<decltype(f), 1, Eigen::Dynamic> values(
            size_t count, std::shared_ptr<Tape> tape);

    /*
     *  Changes a variable's value
     *
     *  If the variable isn't present in the tree, does nothing
     *  Returns true if the variable's value changes
     */
    bool setVar(Tree::Id var, double value);

    /*
     *  Returns a list of ambiguous items from indices 0 to i
     *
     *  This call performs O(i) work to set up the ambig array
     */
    Eigen::Block<decltype(ambig), 1, Eigen::Dynamic> getAmbiguous(size_t i);
    Eigen::Block<decltype(ambig), 1, Eigen::Dynamic> getAmbiguous(
            size_t i, std::shared_ptr<Tape> tape);

    /*  Make an aligned new operator, as this class has Eigen structs
     *  inside of it (which are aligned for SSE) */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    friend class Tape; // for rwalk<ArrayEvaluator>
};

}   // namespace Kernel


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

namespace libfive {

class ArrayEvaluator : public virtual BaseEvaluator
{
public:
    ArrayEvaluator(const Tree& root);
    ArrayEvaluator(const Tree& root,
                   const std::map<Tree::Id, float>& vars);
    ArrayEvaluator(std::shared_ptr<Deck> t);
    ArrayEvaluator(std::shared_ptr<Deck> t,
                   const std::map<Tree::Id, float>& vars);

    /*
     *  Stores the given value in the result arrays
     *  (inlined for efficiency)
     */
    void set(const Eigen::Vector3f& p, size_t index)
    {
        v(deck->X, index) = p.x();
        v(deck->Y, index) = p.y();
        v(deck->Z, index) = p.z();

        for (auto& o : deck->oracles)
        {
            o->set(p, index);
        }
    }

    /*
     *  Helper function to reduce boilerplate in functions which work
     *  in arbitrary dimensions and with double-precision values.
     */
    template <unsigned N>
    void set(const Eigen::Matrix<double, N, 1>& p, const Region<N>& region,
             size_t index)
    {
        Eigen::Vector3f v;
        v << p.template cast<float>(), region.perp.template cast<float>();
        set(v, index);
    }

    /*  This is the number of samples that we can process in one pass */
    static constexpr size_t N=LIBFIVE_EVAL_ARRAY_SIZE;

protected:
    /*  Stored in values() and used in operator() to decide how much of the
     *  array we're addressing at once.  count_simd is rounded up to the
     *  nearest SIMD block size; count_actual is the actual count. */
    size_t count_simd;
    size_t count_actual;

    /*  Sets count_simd and count_actual based on count */
    void setCount(size_t count);

    /*  v(clause, index) is a specific data point */
    Eigen::Array<float, Eigen::Dynamic, N, Eigen::RowMajor> v;

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
    Eigen::Block<decltype(v), 1, Eigen::Dynamic> values(size_t count);
    Eigen::Block<decltype(v), 1, Eigen::Dynamic> values(
            size_t count, const Tape& tape);

    /*
     *  Single-point evaluation
     *  Invalidates slot 0 in the data and results array
     */
    float value(const Eigen::Vector3f& pt);
    float value(const Eigen::Vector3f& pt, const Tape& tape);

    /*
     *  Evaluates a single point and returns a tape that doesn't
     *  contain branches that weren't taken by that point evaluation.
     */
    std::pair<float, std::shared_ptr<Tape>> valueAndPush(
            const Eigen::Vector3f& pt);
    std::pair<float, std::shared_ptr<Tape>> valueAndPush(
            const Eigen::Vector3f& pt, const std::shared_ptr<Tape>& tape);

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
    Eigen::Block<decltype(ambig), 1, Eigen::Dynamic> getAmbiguous(
            size_t i, const Tape& tape);

    /*  Make an aligned new operator, as this class has Eigen structs
     *  inside of it (which are aligned for SSE) */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

}   // namespace libfive


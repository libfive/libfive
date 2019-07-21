/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <Eigen/Eigen>

#include "libfive/eval/eval_array.hpp"

namespace libfive {

class DerivArrayEvaluator : public ArrayEvaluator
{
public:
    DerivArrayEvaluator(const Tree& root);
    DerivArrayEvaluator(const Tree& root,
                        const std::map<Tree::Id, float>& vars);
    DerivArrayEvaluator(std::shared_ptr<Deck> t);
    DerivArrayEvaluator(std::shared_ptr<Deck> t,
                        const std::map<Tree::Id, float>& vars);

protected:
    /*  d(clause).col(index) is a set of partial derivatives [dx, dy, dz] */
    Eigen::Array<Eigen::Array<float, 3, N>, Eigen::Dynamic, 1> d;

    /*  out(col) is a result [dx, dy, dz, w] */
    Eigen::Array<float, 4, N> out;

    /* When evaluating from a parent JacobianEvaluator, we want the
     * CONST_VARS opcode to clear the derivatives, which is special-cased
     * in this flag.  */
    bool clear_vars = false;

public:
    /*
     *  Multi-point evaluation (values must be stored with set)
     */
    Eigen::Block<decltype(out), 4, Eigen::Dynamic> derivs(size_t count);
    Eigen::Block<decltype(out), 4, Eigen::Dynamic> derivs(
            size_t count, const Tape& tape);

    /*
     *  Single-point evaluation (return dx, dy, dz, distance)
     *  Invalidates slot 0 in the data array.
     */
    Eigen::Vector4f deriv(const Eigen::Vector3f& pt);
    Eigen::Vector4f deriv(const Eigen::Vector3f& pt,
                          const Tape& tape);

    /*
     *  Per-clause evaluation, used in tape walking
     */
    void operator()(Opcode::Opcode op, Clause::Id id,
                    Clause::Id a, Clause::Id b);

    /*
     *  Returns a list of ambiguous items from indices 0 to i that
     *  have derivatives that vary.  This is similar to getAmbiguous
     *  in the ArrayEvaluator parent class, but helps to more precisely
     *  determine if the ambiguous points matter, or whether it's just
     *  a case where the tree has ended up calculating min(X, X)
     *  (for example).
     *
     *  This call performs O(i) work to set up the ambig array
     */
    Eigen::Block<decltype(ambig), 1, Eigen::Dynamic> getAmbiguousDerivs(
            size_t count, const Tape& tape);
    Eigen::Block<decltype(ambig), 1, Eigen::Dynamic> getAmbiguousDerivs(
            size_t count);

    /*  Make an aligned new operator, as this class has Eigen structs
     *  inside of it (which are aligned for SSE) */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

}   // namespace libfive



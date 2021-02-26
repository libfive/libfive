/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/eval/eval_jacobian.hpp"
#include "libfive/eval/eval_interval.hpp"

namespace libfive {

class Evaluator : public JacobianEvaluator, public IntervalEvaluator
{
public:
    Evaluator(const Tree& root) :
        Evaluator(std::make_shared<Deck>(root)) {}
    Evaluator(const Tree& root,
              const std::map<Tree::Id, float>& vars) :
        Evaluator(std::make_shared<Deck>(root), vars) {}

    Evaluator(std::shared_ptr<Deck> t) :
        Evaluator(t, std::map<Tree::Id, float>()) {}

    Evaluator(std::shared_ptr<Deck> t,
              const std::map<Tree::Id, float>& vars) :
        BaseEvaluator(t, vars),
        JacobianEvaluator(t, vars),
        IntervalEvaluator(t, vars) {}

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    std::shared_ptr<Deck> getDeck() { return BaseEvaluator::deck; }

    /*
     *  Updates variable values, return true if changed
     */
    bool updateVars(const std::map<libfive::Tree::Id, float>& vars)
    {
        bool changed = false;
        for (auto& v : vars) {
            changed |= JacobianEvaluator::setVar(v.first, v.second);
            changed |= IntervalEvaluator::setVar(v.first, v.second);
        }
        return changed;
}
    /// This computes intervals and checks against the result of the array
    /// evaluator.  It returns true if the result is valid and false if not;
    /// it is intended for debugging (as, in the absence of bugs, it will
    /// always return true if the passed point is inside the passed region;
    /// if not, it will return false with no evaluation necessary).
    bool intervalAndCheck(const Eigen::Vector3f& lower,
                          const Eigen::Vector3f& upper,
                          size_t count,
                          const Tape& tape);
protected:
    /// Used in intervalAndCheck.
    void operator()(Opcode::Opcode op, Clause::Id id,
                    Clause::Id a, Clause::Id b, bool& failed);
};

}   // namespace libfive


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
};

}   // namespace libfive


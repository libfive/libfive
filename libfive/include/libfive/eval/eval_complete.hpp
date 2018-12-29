/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/eval/eval_deriv_array.hpp"
#include "libfive/eval/eval_interval.hpp"
#include "libfive/eval/eval_feature.hpp"
#include "libfive/eval/eval_deriv.hpp"

namespace Kernel {

/*  This evaluator contains all specific types once inheritance is considered,
 *  and so is suitable for uses such as oracles that are based on evaluators
 *  and need to use all types.
 */

class CompleteEvaluator
{
public:
    CompleteEvaluator(const Tree t)
        : CompleteEvaluator(t, std::map<Tree::Id, double>())
    { /* Nothing to do here */ }

    CompleteEvaluator(const Tree t, const std::map<Tree::Id, double>& vars)
        : deck(new Deck(t)), array(deck, vars),
          interval(deck, vars), feature(deck, vars), deriv(deck, vars)
    { /* Nothing to do here */ }

    /*
    *  Updates variable values, return true if changed
    */
    bool updateVars(const std::map<Kernel::Tree::Id, double>& vars)
    {
        bool changed = false;
        for (auto& v : vars)
        {
            changed |= array.setVar(v.first, v.second);
            changed |= interval.setVar(v.first, v.second);
            changed |= feature.setVar(v.first, v.second);
            changed |= deriv.setVar(v.first, v.second);
        }
        return changed;
    }

protected:
    std::shared_ptr<Deck> deck;

public:
    DerivArrayEvaluator array;
    IntervalEvaluator interval;
    FeatureEvaluator feature;
    DerivEvaluator deriv;

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

}   // namespace Kernel

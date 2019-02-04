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

namespace Kernel {

class XTreeEvaluator
{
public:
    XTreeEvaluator(const Tree t)
        : XTreeEvaluator(t, std::map<Tree::Id, float>())
    { /* Nothing to do here */ }

    XTreeEvaluator(const Tree t, const std::map<Tree::Id, float>& vars)
        : deck(new Deck(t)), array(deck, vars), interval(deck, vars),
          feature(deck, vars), deriv(deck, vars)
    { /* Nothing to do here */ }

    /*
     *  Updates variable values, return true if changed
     */
    bool updateVars(const std::map<Kernel::Tree::Id, float>& vars)
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

public:
    std::shared_ptr<Deck> deck;

    DerivArrayEvaluator array;
    IntervalEvaluator interval;
    FeatureEvaluator feature;
    DerivEvaluator deriv;

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

}   // namespace Kernel

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

namespace Kernel {

class HeightmapEvaluator
{
public:
    HeightmapEvaluator(const Tree t)
        : deck(new Deck(t)), array(deck), interval(deck)
    { /* Nothing to do here */ }

public:
    std::shared_ptr<Deck> deck;

    DerivArrayEvaluator array;
    IntervalEvaluator interval;

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

}   // namespace Kernel

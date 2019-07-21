/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <iostream>

#include "libfive/eval/base.hpp"
#include "libfive/eval/deck.hpp"

namespace libfive {

BaseEvaluator::BaseEvaluator(std::shared_ptr<Deck> deck,
                             const std::map<Tree::Id, float>& vars)
    : deck(deck)
{
    for (auto& v : deck->vars.right)
    {
        if (vars.find(v.first) == vars.end())
        {
            std::cerr << "BaseEvaluator::BaseEvaluator: "
                      << "uninitialized variable." << std::endl;
        }
    }
}

}   // namespace libfive

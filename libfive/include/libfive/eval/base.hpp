/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <map>
#include <memory>

#include "libfive/tree/tree.hpp"

namespace libfive {
class Deck; /*  Forward declaration */

class BaseEvaluator
{
public:
    BaseEvaluator(std::shared_ptr<Deck> deck,
                  const std::map<Tree::Id, float>& vars);

protected:
    std::shared_ptr<Deck> deck;
};

}   // namespace libfive

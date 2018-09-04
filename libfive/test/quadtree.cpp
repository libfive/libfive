/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "catch.hpp"

#include "libfive/tree/tree.hpp"

#include "libfive/render/quadtree.hpp"
#include "libfive/render/region.hpp"

using namespace Kernel;

TEST_CASE("Vertex positioning on sliced rectangle")
{
    auto t = max(max(max(-Tree::X(), Tree::X() - 0.5),
                     max(-Tree::Y(), Tree::Y() - 0.5)),
                 0.5 - Tree::X() - Tree::Y());

    Region r({-1, 1}, {-1, 1}, {0, 0}, 4);
    std::unique_ptr<Quadtree> out(Quadtree::render(t, r));

    auto eval = Evaluator(t);
    float worst = 0;
    std::list<const Quadtree*> targets = {out.get()};
    while (targets.size())
    {
        const Quadtree* o = targets.front();
        targets.pop_front();

        if (o->getType() == Quadtree::BRANCH)
        {
            for (unsigned i=0; i < 4; ++i)
            {
                targets.push_back(o->child(i));
            }
        }
        else if (o->getType() == Quadtree::LEAF)
        {
            worst = fmax(worst, fabs(eval.eval(o->getVertex().cast<float>())));
        }
    }
    REQUIRE(worst < 0.001);
}

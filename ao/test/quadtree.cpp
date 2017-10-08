/*
Ao: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "catch.hpp"

#include "ao/tree/tree.hpp"

#include "ao/render/quadtree.hpp"
#include "ao/render/region.hpp"

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

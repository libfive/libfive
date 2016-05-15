/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#pragma once

#include "ao/kernel/render/xtree.hpp"

class Quadtree : public XTree<Quadtree, 2>
{
    Quadtree(const Subregion& r);
    Quadtree(Evaluator* e, const Subregion& r, uint32_t flags);
    Quadtree(Evaluator* e, const std::array<Quadtree*, 4>& cs,
           const Subregion& r, uint32_t flags);

    bool cornerTopology() const override;
    bool leafTopology() const override;
    const std::vector<std::pair<unsigned, unsigned>>& cellEdges() override
    { return _cellEdges; }

    const static std::vector<std::pair<unsigned, unsigned>> _cellEdges;

    friend class XTree;
};

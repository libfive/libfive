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

class Octree : public XTree<Octree, 3>
{
protected:
    Octree(const Subregion& r);
    Octree(Evaluator* e, const Subregion& r);
    Octree(Evaluator* e, const std::array<Octree*, 8>& cs, const Subregion& r);

    static const std::vector<bool>& cornerTable()
        { return _cornerTable; }
    static const std::vector<std::pair<unsigned, unsigned>>& cellEdges()
        { return _cellEdges; }
    bool leafTopology() const;

    const static std::vector<std::pair<unsigned, unsigned>> _cellEdges;
    const static std::vector<bool> _cornerTable;

    friend class XTree;
};

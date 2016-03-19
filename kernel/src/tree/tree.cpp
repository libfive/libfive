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
#include <cassert>
#include <algorithm>

#include "ao/kernel/tree/tree.hpp"
#include "ao/kernel/tree/store.hpp"
#include "ao/kernel/tree/atom.hpp"
#include "ao/kernel/tree/token.hpp"

////////////////////////////////////////////////////////////////////////////////

Tree::Tree(Store* s, Token* root_token)
    : root(nullptr)
{
    std::unordered_map<const Token*, Atom*> atoms;

    // Set flags to mark which tokens are used in the tree
    s->markFound(root_token);

    // Ensure that the base variables are all present in the tree and marked
    // as found (even if there wasn't a path to them from the root)
    for (auto c : {s->X(), s->Y(), s->Z()})
    {
        c->found = true;
    }

    // Flatten constants into the data array and constants vector
    constants.reserve(constants.size() +
                      s->constants.size() +
                      s->values.size());
    for (auto c : s->constants)
    {
        if (c.second->found)
        {
            constants.push_back(new Atom(c.second, atoms));
        }
    }
    for (auto v : s->values)
    {
        if (v->found)
        {
            constants.push_back(new Atom(v, atoms));
        }
    }

    {   // Flatten operations into the vector of rows vectors
        rows.resize(s->ops.size());
        auto row = rows.begin();

        for (auto itr = s->ops.begin(); itr != s->ops.end(); ++itr, ++row)
        {
            for (auto b : *itr)
            {
                for (auto c : b)
                {
                    if (c.second->found)
                    {
                        row->push_back(new Atom(c.second, atoms));
                    }
                }
            }
        }
        assert(row == rows.end());
    }

    // Get the root atom from the root token
    assert(atoms.count(root_token) == 1);
    root = atoms[root_token];
}

Tree::~Tree()
{
    for (auto c : constants)
    {
        delete c;
    }

    for (auto row : rows)
    {
        for (auto m : row)
        {
            delete m;
        }
    }
}

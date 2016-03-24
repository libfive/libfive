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
    // Optimize the store by collapsing affine nodes, now that calculations
    // are done and we're packing it into a tree.
    root_token = s->collapseAffine(root_token);

    // Get a set of Tokens that are connected to the root
    auto found = s->findConnected(root_token);

    // This is a mapping from Tokens in the Store to Atoms in the Tree
    std::unordered_map<const Token*, Atom*> atoms;

    // Ensure that the base variables are all present in the tree and marked
    // as found (even if there wasn't a path to them from the root)
    X = new Atom(s->X(), atoms);
    Y = new Atom(s->Y(), atoms);
    Z = new Atom(s->Z(), atoms);
    for (auto c : {s->X(), s->Y(), s->Z()})
    {
        found.insert(c);
    }

    // Flatten constants into the data array and constants vector
    constants.reserve(constants.size() + s->constants.size());
    for (auto c : s->constants)
    {
        if (found.find(c.second) != found.end())
        {
            constants.push_back(new Atom(c.second, atoms));
        }
    }

    {   // Flatten operations into the vector of rows vectors
        rows.resize(s->ops.size() - 1);
        auto row = rows.begin();

        for (auto itr = s->ops.begin() + 1; itr != s->ops.end(); ++itr, ++row)
        {
            assert(row != rows.end());
            for (auto b : *itr)
            {
                for (auto c : b)
                {
                    if (found.find(c.second) != found.end())
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
    delete(X);
    delete(Y);
    delete(Z);

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

////////////////////////////////////////////////////////////////////////////////


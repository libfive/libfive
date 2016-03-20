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
    : X(new Atom(OP_X)), Y(new Atom(OP_Y)), Z(new Atom(OP_Z)), root(nullptr)
{
    // Optimize the store by collapsing affine nodes, now that calculations
    // are done and we're packing it into a tree.
    root_token = s->collapseAffine(root_token);

    // Get a set of Tokens that are connected to the root
    auto found = s->findConnected(root_token);

    // This is a mapping from Tokens in the Store to Atoms in the Tree
    std::unordered_map<const Token*, Atom*> atoms;

    // We'll be adding a 4x3 transform matrix below X, Y, Z
    const size_t MATRIX_ROWS = 3;
    rows.resize(MATRIX_ROWS);

    // Ensure that the base variables are all present in the tree and marked
    // as found (even if there wasn't a path to them from the root)
    for (auto c : {s->X(), s->Y(), s->Z()})
    {
        found.insert(c);
        atoms[c] = buildMatrixRow(c->op);
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
        assert(rows.size() == MATRIX_ROWS);
        rows.resize(MATRIX_ROWS + s->ops.size() - 1);
        auto row = rows.begin() + MATRIX_ROWS;

        // We skip the weight-0 operations, since we've already captured them
        // in the matrix transform and the X, Y, Z members
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

    for (auto m : matrix)
    {
        delete m;
    }
}

////////////////////////////////////////////////////////////////////////////////

Atom* Tree::buildMatrixRow(Opcode op)
{
    assert(op == OP_X || op == OP_Y || op == OP_Z);
    assert(X != nullptr && Y != nullptr && Z != nullptr);
    assert(rows.size() == 3);

    // The matrix transform is of the form
    //     q' = a*x + b*y + c*z + d
    // (where q' is x', y', or z')
    //
    // It is implemented with three OP_MULs (a*x, b*y, c*z)
    // and three OP_ADDs (a*x + b*y, c*z + d, a*x + b*y + c*z + d)

    // The default matrix preserves X, Y, Z values
    Atom* a  = new Atom(op == OP_X ? 1.0 : 0.0);
    Atom* b  = new Atom(op == OP_Y ? 1.0 : 0.0);
    Atom* c  = new Atom(op == OP_Z ? 1.0 : 0.0);
    Atom* d  = new Atom(0.0);

    Atom* ax = new Atom(OP_MUL, X, a);
    Atom* by = new Atom(OP_MUL, Y, b);
    Atom* cz = new Atom(OP_MUL, Z, c);

    Atom* ax_by = new Atom(OP_ADD, ax, by);
    Atom* cz_d  = new Atom(OP_ADD, cz, d);

    Atom* ax_by_cz_d = new Atom(OP_ADD, ax_by, cz_d);

    // Store relevant constant Atoms in the matrix array
    matrix[4*(op - OP_X)]     = a;
    matrix[4*(op - OP_X) + 1] = b;
    matrix[4*(op - OP_X) + 2] = c;
    matrix[4*(op - OP_X) + 3] = d;

    // Load matrix transform into the operator array
    rows[0].push_back(ax);
    rows[0].push_back(by);
    rows[0].push_back(cz);

    rows[1].push_back(ax_by);
    rows[1].push_back(cz_d);

    rows[2].push_back(ax_by_cz_d);

    return ax_by_cz_d;
}

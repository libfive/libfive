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

#include <array>
#include <vector>
#include <list>
#include <cstdlib>

class Atom;
class Store;
class Token;

/*
 *  A tree represents a math expression that can be evaluated in various ways
 */
class Tree
{
public:
    /*
     *  Construct a tree from the given Store
     */
    explicit Tree(Store* s, Token* root);

    /*
     *  In destructor, delete all of the data that this Tree owns
     */
    ~Tree();

    /*
     *  Looks up root atom in the tree
     */
    Atom* getRoot() const { return root; }

    /*  Pointer to a parent (used to decide who destroys the tree)  */
    void* parent=nullptr;

protected:
    /*
     *  Creates a row of the transform matrix
     *
     *  Requires X, Y, Z to be populated
     *  Fills 12 spots in the data array
     */
    Atom* buildMatrixRow(size_t i);

    /*  All operations live in a set of rows sorted by weight */
    std::vector<std::vector<Atom*>> rows;

    /*  Our position variables are stored as separate pointers     *
     *  (so that they can be easily accessed to set their values)  */
    Atom *X, *Y, *Z;

    /*  matrix is a pointer to a 4x3 transform matrix  */
    std::array<Atom*, 12> matrix;

    /*  Pointers to constants live in this vector  */
    std::vector<Atom*> constants;

    /*  This is the top atom of the tree  */
    Atom* root;

    friend class Evaluator;
};

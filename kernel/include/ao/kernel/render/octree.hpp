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
#include <memory>
#include <vector>

#include <glm/vec3.hpp>
#include <glm/vec3.hpp>

#include "ao/kernel/eval/interval.hpp"

class Region;
class Subregion;
class Tree;
class Evaluator;

class Octree
{
public:
    static Octree* Render(Tree* t, const Region& r,
                          uint32_t flags=COLLAPSE,
                          bool multithread=true);

    /*  Enumerator for optional flags  */
    enum Flags { COLLAPSE = 1 };

    /*  Enumerator that distinguishes between cell types  */
    enum Type { LEAF, BRANCH, EMPTY, FULL };

    /*  Enumerator to refer to octree axes  */
    enum Axis { AXIS_X = 1, AXIS_Y = 2, AXIS_Z = 4 };

    /*
     *  Returns the position of the given corner
     *
     *  Must be in agreement with the Subregion splitting order in octsect
     */
    glm::vec3 pos(uint8_t i) const;

    /*
     *  Look up a corner's value
     */
    bool corner(uint8_t i) const { return corners[i]; }

    /*
     *  Look up a child octree
     */
    const Octree* child(uint8_t i) const
    { return type == BRANCH ? children[i].get() : this; }

    /*
     *  Returns this cell's type
     */
    Type getType() const { return type; }

    /*  Struct to store Hermite intersection data  */
    struct Intersection {
        glm::vec3 pos;
        glm::vec3 norm;
    };

    /*
     *  Returns the vector of intersections
     */
    const std::vector<Intersection>& getIntersections() const
    { return intersections; }

    /*
     *  Looks up the vertex position
     */
    glm::vec3 getVertex() const { return vert; }

    /*
     *  Return the tree's level (leafs are 0 and it goes up from there)
     */
    unsigned getLevel() const { return level; }

protected:
    /*
     *  Constructs an octree recursively from the given subregion
     */
    Octree(Evaluator* e, const Subregion& r, uint32_t flags);
    /*  Delegating constructor to initialize X, Y, Z  */
    Octree(const Subregion& r);
    /*  Helpful constructor to assemble from multiple threads  */
    Octree(Evaluator* e, const std::array<Octree*, 8>& children,
           const Subregion& r, uint32_t flags);

    /*
     *  Splits a subregion and fills out child pointers and cell type
     */
    void populateChildren(Evaluator* e, const Subregion& r, uint32_t flags);

    /*
     *  Finishes initialization once the type and child pointers are in place
     *  (split into a separate function because we can get child pointers
     *   either through recursion or with threaded construction)
     */
    void finalize(Evaluator* e, uint32_t flags);

    /*
     *  Stores edge-wise intersections for the cell,
     *  storing them in the intersections vector.
     *
     *  For leaf cells, intersections are found with binary search along every
     *  edge that exhibits a sign change.  For branch cells, intersections are
     *  found by accumulating from all child leaf cells with max rank.
     */
    void findIntersections(Evaluator* e);

    /*
     *  If all children are filled or empty, collapse the branch
     *
     *  Otherwise, collapse the branch if it is topologically safe to do
     *  so and the residual QEF error isn't too large.
     */
    void collapseBranch(Evaluator* e);

    /*
     *  If all corners are of the same sign, convert to FULL or EMPTY
     */
    void collapseLeaf();

    /*
     *  Finds a feature vertex by finding intersections then solving a
     *  least-squares fit to minimize a quadratic error function.
     *
     *  The resulting vertex is stored in vert; the residual is returned
     */
    float findVertex(Evaluator* e);

    /*
     *  Checks to see if the eight corners describe an ambiguous
     *  (non-manifold) topology.
     *
     *  Returns true if the cell is safe to collapse (i.e. has manifold corner
     *  topology), false otherwise.
     */
    bool cornerTopology() const;

    /*
     *  Checks to make sure that the fine contour is topologically equivalent
     *  to the coarser contour by comparing signs in edges and faces
     *
     *  Returns true if the cell can be collapsed without changing topology
     *  (with respect to the leaves)
     */
    bool leafTopology() const;

    /*
     *  Performs binary search along a cube's edge
     *
     *  The resulting Intersection's normal is of unit length
     *
     *  eval(a) should be < 0 (inside the shape) and eval(b) should be outside
     */
    static Intersection searchEdge(glm::vec3 a, glm::vec3 b, Evaluator* e);

    /*  Bounds for this octree  */
    const Interval X, Y, Z;

    /*  Cell type  */
    Type type;

    /*  Intersections where the shape crosses the cell  */
    std::vector<Intersection> intersections;

    /*  Pointers to children octrees (either all populated or all null)  */
    std::array<std::unique_ptr<Octree>, 8> children;

    /*  level = max(map(level, children)) + 1  */
    unsigned level;

    /*  Array of filled states for the cell's corners  */
    std::array<bool, 8> corners;

    /*  Feature vertex located in the cell  */
    glm::vec3 vert=glm::vec3(std::numeric_limits<float>::quiet_NaN());

    /*  Feature rank for the cell's vertex, where                    *
     *      1 is face, 2 is edge, 3 is corner                        *
     *                                                               *
     *  This value is populated in findVertex and used when merging  *
     *  intersections from lower-ranked children                     */
    unsigned rank=0;

    /*  This is a hard-coded list of axis pairs that represent cell edges  */
    const static std::pair<unsigned, unsigned> cellEdges[12];
    const static int SEARCH_COUNT = 8;
};

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
#include <limits>

#include <glm/vec3.hpp>
#include <Eigen/Eigen>

#include "ao/kernel/eval/interval.hpp"
#include "ao/kernel/eval/evaluator.hpp"

#include "ao/kernel/render/region.hpp"

#include "ao/kernel/tree/tree.hpp"

/*  Helper struct to store Hermite intersection data  */
struct Intersection {
    glm::vec3 pos;
    glm::vec3 norm;
};

template <class T, int dims>
class XTree
{
public:
    static T* Render(Tree* t, const Region& r, uint32_t flags=COLLAPSE,
                     bool multithread=true);

    /*  Enumerator for optional flags  */
    enum Flags { COLLAPSE   = (1 << 0),
    };

    /*  Enumerator that distinguishes between cell types  */
    enum Type { LEAF, BRANCH, EMPTY, FULL };

    /*  Enumerator to refer to octree axes  */
    enum Axis { AXIS_X = 1, AXIS_Y = 2, AXIS_Z = 4 };

    /*
     *  Returns the position of the given corner
     *
     *  Must be in agreement with the Subregion splitting order in octsect
     */
    glm::vec3 pos(uint8_t i) const
    {
        return {i & AXIS_X ? X.upper() : X.lower(),
                i & AXIS_Y ? Y.upper() : Y.lower(),
                i & AXIS_Z ? Z.upper() : Z.lower()};
    }

    /*
     *  Look up a child octree
     */
    const T* child(uint8_t i) const
    { return type == BRANCH ? children[i].get() : static_cast<const T*>(this); }

    /*
     *  Look up a corner's value
     */
    bool corner(uint8_t i) const { return corners[i]; }

    /*
     *  Returns this cell's type
     */
    Type getType() const { return type; }

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
     *  Recursive constructor that splits r
     *  Requires a call to finalize in the parent constructor
     */
    XTree(Evaluator* e, const Subregion& r, uint32_t flags);

    /*
     *  Collecting constructor that assembles multiple subtrees
     *  Requires a call to finalize in the parent constructor
     */
    XTree(const std::array<T*, 1 << dims>& cs, const Subregion& r);

    /*
     *  Delegating constructor to initialize X, Y, Z
     */
    XTree(const Subregion& r);

    /*
     *  Splits a subregion and fills out child pointers and cell type
     */
    void populateChildren(Evaluator* e, const Subregion& r,
                          uint32_t flags);

    /*
     *  Finishes initialization once the type and child pointers are in place
     *  (split into a separate function because we can get child pointers
     *   either through recursion or with threaded construction)
     */
    void finalize(Evaluator* e, uint32_t flags);

    /*
     *  Finds and returns edge-wise intersections for a LEAF cell
     *
     *  Intersections are found with binary search along every
     *  edge that exhibits a sign change.
     *
     *  This function also populated mass_point as the mean of intersections.
     */
    std::vector<Intersection> findIntersections(Evaluator* e) const;

    /*
     *  Populates AtA, AtB, BtB, mass_point, and rank
     *  by calling findIntersections then building matrices
     */
    Eigen::EigenSolver<Eigen::Matrix3d> findLeafMatrices(Evaluator* e);

    /*
     *  Populates AtA, AtB, BtB, mass_point, and rank
     *  by pulling matrices from children
     */
    void findBranchMatrices();

    /*
     *  If all children are filled or empty, collapse the branch
     *
     *  Otherwise, collapse the branch if it is topologically safe to do
     *  so and the residual QEF error isn't too large.
     */
    void collapseBranch();

    /*
     *  Finds a feature vertex by solving a least-squares fit to minimize
     *  a quadratic error function.
     *
     *  Requires AtA, AtB, BtB, and mass_point to be populated
     *
     *  Returns the vertex and populates err if provided
     */
    glm::vec3 findVertex(float* err=NULL) const;
    glm::vec3 findVertex(Eigen::EigenSolver<Eigen::Matrix3d>& es,
                         float* err=NULL) const;

    /*
     *  Performs binary search along a cube's edge, returning the intersection
     *
     *  The resulting Intersections' normals are of unit length
     *
     *  eval(a) should be < 0 (inside the shape) and eval(b) should be outside
     */
    Intersection searchEdge(glm::vec3 a, glm::vec3 b, Evaluator* eval) const;

    /*
     *  Checks to see if the cell's corners describe an ambiguous
     *  (non-manifold) topology.
     *
     *  Returns true if the cell is safe to collapse (i.e. has manifold
     *  corner topology), false otherwise.
     */
    bool cornerTopology() const;

    /**************************************************************************
     *
     * Every child class needs to implement the following three functions
     * They're not virtual because we use the CRTP and might as well
     * do the binding at compile-time.
     *
     *  static const std::vector<bool>& cornerTable();
     *      Returns a table such that looking up a particular corner
     *      configuration returns whether that configuration is safe to
     *      collapse.
     *
     *      This implements the test from [Gerstner et al, 2000], as
     *      described in [Ju et al, 2002].
     *
     *
     *  bool leafTopology() const;
     *      Checks to make sure that the fine contour is topologically equivalent
     *      to the coarser contour by comparing signs in edges and faces
     *
     *      Returns true if the cell can be collapsed without changing topology
     *      (with respect to the leaves)
     *
     *
     *  const std::vector<std::pair<unsigned, unsigned>>& cellEdges();
     *      Returns a hard-coded list of axis pairs representing cell edges
     *
     **************************************************************************/

    /*  Bounds for this octree  */
    const Interval X, Y, Z;

    /*  Cell type  */
    Type type;

    /*  Pointers to children octrees (either all populated or all null)  */
    std::array<std::unique_ptr<T>, 1 << dims> children;

    /*  level = max(map(level, children)) + 1  */
    unsigned level;

    /*  Array of filled states for the cell's corners  */
    std::array<bool, 1 << dims> corners;

    /*  Feature vertex located in the cell  */
    glm::vec3 vert=glm::vec3(std::numeric_limits<float>::quiet_NaN());

    /*  Marks whether a node is manifold or not  */
    bool manifold;

    /*  Mass point is the average intersection location *
     *  (the w coordinate is number of points averaged) */
    glm::vec4 mass_point=glm::vec4(0.0f, 0.0f, 0.0f, 0.0f);

    /*  QEF terms */
    Eigen::Matrix3d AtA=Eigen::Matrix3d::Zero();
    Eigen::Vector3d AtB=Eigen::Vector3d::Zero();
    double BtB=0;

    /*  Feature rank for the cell's vertex, where                    *
     *      1 is face, 2 is edge, 3 is corner                        *
     *                                                               *
     *  This value is populated in find{Leaf|Branch}Matrices and     *
     *  used when merging intersections from lower-ranked children   */
    unsigned rank=0;

    /*  Number of iterations to run when doing binary search for verts  */
    const static int SEARCH_COUNT = 8;
};

#include "ao/kernel/render/xtree.ipp"

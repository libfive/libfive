/*
libfive: a CAD kernel for modeling with implicit functions
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
#pragma once

#include <array>
#include <atomic>
#include <iostream>

#include <cstdint>

#include <Eigen/Eigen>
#include <Eigen/StdVector>

#include "libfive/export.hpp"
#include "libfive/render/brep/region.hpp"
#include "libfive/render/brep/intersection.hpp"
#include "libfive/render/brep/marching.hpp"
#include "libfive/render/brep/eval_xtree.hpp"
#include "libfive/render/brep/neighbors.hpp"
#include "libfive/eval/interval.hpp"

namespace Kernel {

template <unsigned N>
class XTree
{
public:
    /*
     *  Simple constructor
     */
    explicit XTree(XTree<N>* parent, unsigned index, Region<N> region);

    /*
     *  Populates type, setting corners, manifold, and done if this region is
     *  fully inside or outside the mode.
     *
     *  Returns a shorter version of the tape that ignores unambiguous clauses.
     */
    std::shared_ptr<Tape> evalInterval(IntervalEvaluator& eval,
                                       std::shared_ptr<Tape> tape);

    /*
     *  Evaluates and stores a result at every corner of the cell.
     *  Sets type to FILLED / EMPTY / AMBIGUOUS based on the corner values.
     *  Then, solves for vertex position, populating AtA / AtB / BtB.
     */
    void evalLeaf(XTreeEvaluator* eval, std::shared_ptr<Tape> tape);

    /*
     *  If all children are present, then collapse based on the error
     *  metrics from the combined QEF (or interval filled / empty state).
     *
     *  Returns false if any children are yet to come, true otherwise.
     */
    bool collectChildren(XTreeEvaluator* eval, std::shared_ptr<Tape> tape,
                         double max_err);

    /*
     *  Deletes all of the children
     */
    ~XTree();

    /*
     *  Checks whether this tree splits
     */
    bool isBranch() const { return children[0] != nullptr; }

    /*
     *  Looks up a child, returning *this if this isn't a branch
     */
    const XTree<N>* child(unsigned i) const
    { return isBranch() ? children[i].load() : this; }

    /*
     *  Returns the filled / empty state for the ith corner
     */
    Interval::State cornerState(uint8_t i) const { return corners[i]; }

    /*
     *  Returns the corner position for the ith corner
     */
    Eigen::Array<double, N, 1> cornerPos(uint8_t i) const
    {
        return corner_positions.row(i);
    }

    /*
     *  Returns the averaged mass point
     */
    Eigen::Matrix<double, N, 1> massPoint() const;

    /*  Boilerplate for an object that contains an Eigen struct  */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    /*
     *  Unpack the vertex into a 3-element array
     *  (using the region's perpendicular coordinates)
     */
    FIVE_EXPORT Eigen::Vector3d vert3(unsigned index=0) const;

    /*  Helper typedef for N-dimensional column vector */
    typedef Eigen::Matrix<double, N, 1> Vec;

    /*  Parent tree, or nullptr if this is the root */
    XTree<N>* parent;

    /*  Index into the parent tree's children array.  We only store the tree
     *  in the children array when it is complete, so it needs to know its
     *  index for when that time comes.  */
    unsigned parent_index;

    /*  The region filled by this XTree */
    const Region<N> region;

    /*  Children pointers, if this is a branch  */
    std::array<std::atomic<XTree<N>*>, 1 << N> children;

    /*  level = max(map(level, children)) + 1  */
    unsigned level=0;

    /*  Vertex locations, if this is a leaf
     *
     *  To make cells manifold, we may store multiple vertices in a single
     *  leaf; see writeup in marching.cpp for details  */
    Eigen::Matrix<double, N, _pow(2, N - 1)> verts;

    /*
     *  Look up a particular vertex by index
     */
    Eigen::Matrix<double, N, 1> vert(unsigned i=0) const
    { assert(i < vertex_count); return verts.col(i); }

    /*
     *  Looks up a particular intersection array by corner indices
     */
    const IntersectionVec<N>& intersection(unsigned a, unsigned b) const
    {
        assert(mt->e[a][b] != -1);
        return intersections[mt->e[a][b]];
    }

    /*  Array of filled states for the cell's corners
     *  (must only be FILLED / EMPTY, not UNKNOWN or AMBIGUOUS ) */
    std::array<Interval::State, 1 << N> corners;

    /*  Array of precomputed corner positions, stored once at the
     *  beginning of the constructor and looked up with cornerPos() */
    Eigen::Matrix<double, 1 << N, N> corner_positions;

    /* Here, we'll prepare to store position, {normal, value} pairs
     * for every crossing and feature.  RAM is cheap, so we allocated
     * enough space for at least two inside-outside intersection pairs
     * on each edge; more pairs resize the small_vector */
    std::array<IntersectionVec<N>, _edges(N) * 2> intersections;

    /*  Leaf cell state, when known  */
    Interval::State type=Interval::UNKNOWN;

    /*  Feature rank for the cell's vertex, where                    *
     *      1 is face, 2 is edge, 3 is corner                        *
     *                                                               *
     *  This value is populated in find{Leaf|Branch}Matrices and     *
     *  used when merging intersections from lower-ranked children   */
    unsigned rank=0;

    /* Used as a unique per-vertex index when unpacking into a b-rep;   *
     * this is cheaper than storing a map of XTree* -> uint32_t         */
    mutable std::array<uint32_t, _pow(2, N - 1)> index;

    /*  Bitfield marking which corners are set */
    uint8_t corner_mask=0;

    /*  Stores the number of patches / vertices in this cell
     *  (which could be more than one to keep the surface manifold */
    unsigned vertex_count=0;

    /*  Marks whether this cell is manifold or not  */
    bool manifold=false;

    /*  Single copy of the marching squares / cubes table, lazily
     *  initialized when needed */
    static std::unique_ptr<const Marching::MarchingTable<N>> mt;

protected:
    /*
     *  Searches for a vertex within the XTree cell, using the QEF matrices
     *  that are pre-populated in AtA, AtB, etc.
     *
     *  Minimizes the QEF towards mass_point
     *
     *  Stores the vertex in vert and returns the QEF error
     */
    double findVertex(unsigned i=0);

    /*
     *  Returns edges (as indices into corners)
     *  (must be specialized for a specific dimensionality)
     */
    const std::vector<std::pair<uint8_t, uint8_t>>& edges() const;

    /*
     *  Returns a table such that looking up a particular corner
     *  configuration returns whether that configuration is safe to
     *  collapse.
     *  (must be specialized for a specific dimensionality)
     *
     *  This implements the test from [Gerstner et al, 2000], as
     *  described in [Ju et al, 2002].
     */
    bool cornersAreManifold() const;

    /*
     *  Checks to make sure that the fine contour is topologically equivalent
     *  to the coarser contour by comparing signs in edges and faces
     *  (must be specialized for a specific dimensionality)
     *
     *  Returns true if the cell can be collapsed without changing topology
     *  (with respect to the leaves)
     */
    bool leafsAreManifold() const;

    /*
     *  Sets corner_mask based on corner[] values
     */
    void buildCornerMask();

    /*
     *  Deletes all children branches, setting the children array to nulls
     */
    void deleteBranches();

    /*
     *  Call this when construction is complete; it will atomically install
     *  this tree into the parent's array of children pointers.
     */
    void done();

    /*  Mass point is the average intersection location *
     *  (the last coordinate is number of points summed) */
    Eigen::Matrix<double, N + 1, 1> _mass_point;

    /*  QEF matrices */
    Eigen::Matrix<double, N, N> AtA;
    Eigen::Matrix<double, N, 1> AtB;
    double BtB=0;

    /*  Marks whether this tree is fully constructed */
    std::atomic_int pending;

    /*  Eigenvalue threshold for determining feature rank  */
    constexpr static double EIGENVALUE_CUTOFF=0.1f;
};

// Explicit template instantiation declarations
template <> bool XTree<2>::cornersAreManifold() const;
template <> bool XTree<3>::cornersAreManifold() const;

template <> bool XTree<2>::leafsAreManifold() const;
template <> bool XTree<3>::leafsAreManifold() const;

template <> const std::vector<std::pair<uint8_t, uint8_t>>& XTree<2>::edges() const;
template <> const std::vector<std::pair<uint8_t, uint8_t>>& XTree<3>::edges() const;

extern template class XTree<2>;
extern template class XTree<3>;

}   // namespace Kernel

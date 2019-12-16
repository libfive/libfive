/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <array>
#include <atomic>
#include <iostream>
#include <stack>

#include <cstdint>

#include <Eigen/Eigen>
#include <Eigen/StdVector>

#include "libfive/eval/interval.hpp"
#include "libfive/render/brep/xtree.hpp"
#include "libfive/render/brep/object_pool.hpp"
#include "libfive/render/brep/dc/intersection.hpp"
#include "libfive/render/brep/dc/marching.hpp"

namespace libfive {

/* Forward declaration */
class Evaluator;
class Tape;
template <unsigned N> class Region;
template <unsigned N> class DCNeighbors;

/*  AMBIGUOUS leaf cells have more data, which we heap-allocate in
 *  this struct to keep the overall tree smaller. */
template <unsigned N>
struct DCLeaf
{
    DCLeaf();
    void reset();

    /*  level = max(map(level, children)) + 1  */
    unsigned level;

    /*  Vertex locations, if this is a leaf
     *
     *  To make cells manifold, we may store multiple vertices in a single
     *  leaf; see writeup in marching.cpp for details  */
    Eigen::Matrix<double, N, ipow(2, N - 1)> verts;

    /* This array allows us to store QEFs for cases where the surface
     * crosses a particular cell edge.  The objects are allocated from
     * the shared pool, though they're not released back to the pool
     * (because they could be in more than one DCLeaf) */
    std::array<Intersection<N>*, _edges(N) * 2> intersections;

    /*  Feature rank for the cell's vertex, where                    *
     *      1 is face, 2 is edge, 3 is corner                        *
     *                                                               *
     *  This value is populated in evalLeaf and used when merging    *
     *  from lower-ranked children                                   */
    unsigned rank;

    /* Used as a unique per-vertex index when unpacking into a b-rep;   *
     * this is cheaper than storing a map of DCTree* -> uint32_t         */
    mutable std::array<uint32_t, ipow(2, N - 1)> index;

    /*  Bitfield marking which corners are set */
    uint8_t corner_mask;

    /*  Stores the number of patches / vertices in this cell
     *  (which could be more than one to keep the surface manifold */
    unsigned vertex_count;

    /*  Marks whether this cell is manifold or not  */
    bool manifold;

    /*  Mass point is the average intersection location *
     *  (the last coordinate is number of points summed) */
    Eigen::Matrix<double, N + 1, 1> mass_point;

    /*  QEF matrices */
    Eigen::Matrix<double, N, N> AtA;
    Eigen::Matrix<double, N, 1> AtB;
    double BtB;

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

template <unsigned N>
class DCTree : public XTree<N, DCTree<N>, DCLeaf<N>>
{
public:
    using Pool = ObjectPool<DCTree<N>, DCLeaf<N>, Intersection<N>>;

    /*
     *  Simple constructor
     *
     *  Pointers are initialized to nullptr, but other members
     *  are invalid until reset() is called.
     */
    explicit DCTree();
    explicit DCTree(DCTree<N>* parent, unsigned index, const Region<N>& region);
    static std::unique_ptr<DCTree<N>> empty();

    /*
     *  Populates type, setting corners, manifold, and done if this region is
     *  fully inside or outside the mode.
     *
     *  Returns a shorter version of the tape that ignores unambiguous clauses.
     */
    std::shared_ptr<Tape> evalInterval(Evaluator* eval,
                                       const std::shared_ptr<Tape>& tape,
                                       Pool& object_pool);

    /*
     *  Evaluates and stores a result at every corner of the cell.
     *  Sets type to FILLED / EMPTY / AMBIGUOUS based on the corner values.
     *  Then, solves for vertex position, populating AtA / AtB / BtB.
     */
    void evalLeaf(Evaluator* eval,
                  const std::shared_ptr<Tape>& tape,
                  Pool& spare_leafs,
                  const DCNeighbors<N>& neighbors);

    /*
     *  If all children are present, then collapse based on the error
     *  metrics from the combined QEF (or interval filled / empty state).
     *
     *  Returns false if any children are yet to come, true otherwise.
     */
    bool collectChildren(Evaluator* eval,
                         const std::shared_ptr<Tape>& tape,
                         Pool& object_pool,
                         double max_err);

    /*
     *  Returns the filled / empty state for the ith corner
     */
    Interval::State cornerState(uint8_t i) const;

    /*
     *  Checks whether this cell is manifold.
     *  This must only be called on non-branching cells.
     */
    bool isManifold() const;

    /*
     *  Looks up this cell's corner mask (used in various tables)
     *  This must only be called on non-branching cells.
     */
    uint8_t cornerMask() const;

    /*  Looks up the cell's level.
     *
     *  This must only be called on non-branching cells.
     *
     *  level is defined as 0 for EMPTY or FILLED terminal cells;
     *  for ambiguous leaf cells, it is the depth of the largest 
     *  chain of leafs that were merged into this cell.
     */
    unsigned level() const;

    /*
     *  Looks up this cell's feature rank.
     *
     *  This must only be called on non-branching cells.
     *
     *  rank is defined as 0 for EMPTY and FILLED cells;
     *  otherwise, it is 1 for a plane, 2 for an edge,
     *  3 for a vertex (in the 3D case).
     */
    unsigned rank() const;

    /*
     *  Sanity-check a DCTree by ensuring that all corners are consistent
     *  between shared subtrees.  This is useful for debugging segfaults
     *  in meshing, which are usually caused by inconsistent trees.
     */
    bool checkConsistency() const;
    bool checkConsistency(const DCNeighbors<N>& neighbors) const;

    /*  Boilerplate for an object that contains an Eigen struct  */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    /*  Helper typedef for N-dimensional column vector */
    typedef Eigen::Matrix<double, N, 1> Vec;

    /*
     *  Look up a particular vertex by index
     */
    Vec vert(unsigned i=0) const;

    /*
     *  Looks up a particular intersection array by corner indices
     */
    Intersection<N>* intersection(unsigned a, unsigned b) const;

    /*
     *  Looks up a particular intersection array by (directed) edge index
     */
    Intersection<N>* intersection(unsigned edge) const;

    /*
     *  Releases this tree and any leaf objects to the given object pool
     */
    void releaseTo(Pool& object_pool);

    static constexpr bool hasSingletons() { return true; }
    static DCTree<N>* singletonEmpty() {
        static DCTree<N> empty(Interval::EMPTY);
        return &empty;
    }
    static DCTree<N>* singletonFilled() {
        static DCTree<N> filled(Interval::FILLED);
        return &filled;
    }
    static bool isSingleton(const DCTree<N>* t) {
        return t == singletonEmpty() || t == singletonFilled();
    }

protected:
    /*  Private constructor for a dummy tree of a particular type */
    DCTree(Interval::State type);

    /*
     *  Searches for a vertex within the DCTree cell, using the QEF matrices
     *  that are pre-populated in AtA, AtB, etc.
     *
     *  Minimizes the QEF towards mass_point
     *
     *  Stores the vertex in vert and returns the QEF error
     */
    double findVertex(unsigned i=0);

    /*
     *  Writes the given intersection into the intersections list
     *  for the specified edge.  Allocates an interesections list
     *  if none already exists.  The given set of derivatives is normalized
     *  (to become a surface normal).  If the normal is invalid, then
     *  we store an intersection with an all-zero normal.  This means we
     *  can still use the intersection for mass-point calculation, but
     *  can detect that the normal is invalid (and so will not use it for
     *  building the A and b matrices).
     */
    void saveIntersection(const Vec& pos, const Vec& derivs,
                          const double value, const size_t edge,
                          Pool& object_pool);

    /*
     *  Returns a table such that looking up a particular corner
     *  configuration returns whether that configuration is safe to
     *  collapse.
     *  (must be specialized for a specific dimensionality)
     *
     *  This implements the test from [Gerstner et al, 2000], as
     *  described in [Ju et al, 2002].
     */
    static bool cornersAreManifold(const uint8_t corner_mask);

    /*
     *  Checks to make sure that the fine contour is topologically equivalent
     *  to the coarser contour by comparing signs in edges and faces
     *  (must be specialized for a specific dimensionality)
     *
     *  Returns true if the cell can be collapsed without changing topology
     *  (with respect to the leaves)
     */
    static bool leafsAreManifold(
            const std::array<DCTree<N>*, 1 << N>& children,
            const std::array<Interval::State, 1 << N>& corners);

    /*
     *  When collecting children and collapsing, each child can contribute the
     *  intersections on the N edges (2*N directed edges) adjacent to the
     *  corner that it contributes.  This method uses mt, which therefore
     *  must have been built first.
     */
    static std::array<unsigned, 2*N> edgesFromChild(unsigned childIndex);

    /*
     *  Returns a corner mask bitfield from the given array
     */
    static uint8_t buildCornerMask(
            const std::array<Interval::State, 1 << N>& corners);
};

}   // namespace libfive

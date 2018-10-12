/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

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

#include "libfive/export.hpp"
#include "libfive/render/brep/region.hpp"
#include "libfive/render/brep/progress.hpp"
#include "libfive/render/brep/intersection.hpp"
#include "libfive/render/brep/marching.hpp"
#include "libfive/render/brep/eval_xtree.hpp"
#include "libfive/render/brep/neighbors.hpp"
#include "libfive/eval/interval.hpp"

namespace Kernel {

template <typename T> class Pool; /* Forward declaration */
template <unsigned N> class Simplexes; /* Forward declaration */

template <unsigned N>
class SimplexTree
{
public:

    /*
     *  This is a handle for both the XTree and the object pool data
     *  that were used to allocate all of its memory.
     */
    class Root
    {
    public:
        Root();
        Root(SimplexTree<N>* ptr);
        Root(Root&& other);

        Root& operator=(Root&& other);

        ~Root() { reset(); }

        void reset(ProgressCallback progress_callback=EMPTY_PROGRESS_CALLBACK);

        const SimplexTree<N>* operator->() { return ptr; }
        const SimplexTree<N>* get() const { return ptr; }

        void claim(Pool<SimplexTree<N>>& pool);
        void claim(Pool<Simplexes<N>>& pool);

        int64_t size() const { return tree_count; }

    protected:
        SimplexTree<N>* ptr;
        std::list<SimplexTree<N>*> trees;
        std::list<Simplexes<N>> leafs;

        // Used for progress tracking.  We use a signed value here because,
        // as we claim Pools of SimplexTree, it's possible for the intermediate
        // result to go negative (if one pool has claimed many trees from
        // another Pool, so it owns more trees than it has allocated)..
        int64_t tree_count=0;
    };

    /*
     *  Simple constructor
     *
     *  Pointers are initialized to nullptr, but other members
     *  are invalid until reset() is called.
     */
    explicit SimplexTree();
    explicit SimplexTree(SimplexTree<N>* parent, unsigned index);

    /*
     *  Resets this tree to a freshly-constructed state
     */
    void reset(SimplexTree<N>* p, unsigned i);

    /*
     *  Populates type, setting corners, manifold, and done if this region is
     *  fully inside or outside the mode.
     *
     *  Returns a shorter version of the tape that ignores unambiguous clauses.
     */
    std::shared_ptr<Tape> evalInterval(
            IntervalEvaluator& eval, const Region<N>& region,
            std::shared_ptr<Tape> tape);

    /*
     *  Evaluates and stores a result at every corner of the cell.
     *  Sets type to FILLED / EMPTY / AMBIGUOUS based on the corner values.
     *  Then, solves for vertex position, populating AtA / AtB / BtB.
     */
    void evalLeaf(XTreeEvaluator* eval, const Neighbors<N>& neighbors,
                  const Region<N>& region, std::shared_ptr<Tape> tape,
                  Pool<Simplexes<N>>& spare_simplexes);

    /*
     *  If all children are present, then collapse based on the error
     *  metrics from the combined QEF (or interval filled / empty state).
     *
     *  Returns false if any children are yet to come, true otherwise.
     */
    bool collectChildren(
            XTreeEvaluator* eval, std::shared_ptr<Tape> tape,
            double max_err, const Region<N>& region,
            Pool<SimplexTree<N>>& spare_trees,
            Pool<Simplexes<N>>& spare_simplexes);

    /*
     *  Checks whether this tree splits
     */
    bool isBranch() const { return children[0] != nullptr; }

    /*
     *  Looks up a child, returning *this if this isn't a branch
     */
    const SimplexTree<N>* child(unsigned i) const
    { return isBranch() ? children[i].load(std::memory_order_relaxed) : this; }

    /*
     *  Returns the filled / empty state for the ith corner
     */
    Interval::State cornerState(uint8_t i) const;

    /*
     *  Checks whether this cell is manifold.
     *  This must only be called on non-branching cells.
     */
    bool isManifold() const;

    /*  Looks up the cell's level.
     *
     *  This must only be called on non-branching cells.
     *
     *  level is defined as 0 for EMPTY or FILLED terminal cells;
     *  for ambiguous leaf cells, it is the number of leafs that
     *  were merged into this cell.
     */
    unsigned level() const;

    /*  Boilerplate for an object that contains an Eigen struct  */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    /*  Helper typedef for N-dimensional column vector */
    typedef Eigen::Matrix<double, N, 1> Vec;

    /*  Parent tree, or nullptr if this is the root */
    SimplexTree<N>* parent;

    /*  Index into the parent tree's children array.  We only store the tree
     *  in the children array when it is complete, so it needs to know its
     *  index for when that time comes.  */
    unsigned parent_index;

    /*  Children pointers, if this is a branch  */
    std::array<std::atomic<SimplexTree<N>*>, 1 << N> children;

    /*  Leaf cell state, when known  */
    Interval::State type;

    /*  Optional leaf data, owned by a parent Pool<Leaf> */
    Simplexes<N>* simplexes;

protected:
    /*
     *  Searches for a vertex within the SimplexTree cell, using the QEF matrices
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
     *  Releases the children (and their Leaf pointers, if present)
     *  into the given object pools.
     */
    void releaseChildren(Pool<XTree<N>>& spare_trees,
                         Pool<Simplexes<N>>& spare_leafs);

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
                          const double value, const size_t edge);

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
     *  Returns a corner mask bitfield from the given array
     */
    static uint8_t buildCornerMask(
            const std::array<Interval::State, 1 << N>& corners);

    /*
     *  Call this when construction is complete; it will atomically install
     *  this tree into the parent's array of children pointers.
     */
    void done();

    /*  Marks whether this tree is fully constructed */
    std::atomic_int pending;

    /*  Eigenvalue threshold for determining feature rank  */
    constexpr static double EIGENVALUE_CUTOFF=0.1f;
};

extern template class SimplexTree<2>;
extern template class SimplexTree<3>;

}   // namespace Kernel

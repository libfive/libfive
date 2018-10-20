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
#include "libfive/eval/eval_xtree.hpp"
#include "libfive/eval/interval.hpp"

#include "libfive/render/brep/region.hpp"
#include "libfive/render/brep/progress.hpp"
#include "libfive/render/brep/ipow.hpp"

namespace Kernel {

/* Forward declarations */
template <typename T> class ObjectPool;
template <unsigned N> class SimplexNeighbors;

template <unsigned N>
class SimplexTree
{
public:

    struct Leaf
    {
        Leaf();
        void reset();

        Eigen::Matrix<double, ipow(3, N), N> vertices;
        std::array<bool, ipow(3, N)> inside;

        /*   Global indices for vertices  */
        std::array<uint64_t, ipow(3, N)> index;

        /*  Tape used for evaluation within this leaf */
        std::shared_ptr<Tape> tape;

        /*  Represents how far from minimum-size leafs we are */
        unsigned level;
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
    void evalLeaf(XTreeEvaluator* eval,
                  const Region<N>& region, std::shared_ptr<Tape> tape,
                  ObjectPool<Leaf>& spare_leafs);

    /*
     *  If all children are present, then collapse based on the error
     *  metrics from the combined QEF (or interval filled / empty state).
     *
     *  Returns false if any children are yet to come, true otherwise.
     */
    bool collectChildren(
            XTreeEvaluator* eval, std::shared_ptr<Tape> tape,
            double max_err, const Region<N>& region,
            ObjectPool<SimplexTree<N>>& spare_trees,
            ObjectPool<Leaf>& spare_leafs);

    /*
     *  Checks whether this tree splits
     */
    bool isBranch() const { return children[0] != nullptr; }

    /*
     *  Looks up a child, returning *this if this isn't a branch
     */
    const SimplexTree<N>* child(unsigned i) const
    { return isBranch() ? children[i].load(std::memory_order_relaxed) : this; }

    /*  Looks up the cell's level.
     *
     *  This must only be called on non-branching cells.
     *
     *  level is defined as 0 for EMPTY or FILLED terminal cells;
     *  for ambiguous leaf cells, it is the number of leafs that
     *  were merged into this cell.
     */
    unsigned level() const;

    /*  Looks up the cell's level for purposes of vertex placement,
     *  returning 0 or more for LEAF cells (depending on how many
     *  other leafs were merged into them), and UINT32_MAX max for
     *  EMPTY or FILLED cells */
    uint32_t leafLevel() const;

    /*
     *  Assigns leaf->index to a array of unique integers for every leaf
     *  in the tree, starting at 1.  This provides a globally unique
     *  identifier for every subspace vertex.
     */
    void assignIndices() const;

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

    /*  Optional leaf data, owned by a parent ObjectPool<Leaf> */
    Leaf* leaf;

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
     *  Releases the children (and their Leaf pointers, if present)
     *  into the given object pools.
     */
    void releaseChildren(ObjectPool<SimplexTree<N>>& spare_trees,
                         ObjectPool<Leaf>& spare_leafs);

    /*
     *  Call this when construction is complete; it will atomically install
     *  this tree into the parent's array of children pointers.
     */
    void done();

    /*
     *  Helper function to assign leaf->index for all leafs in a tree
     */
    void assignIndices(uint64_t& i, const SimplexNeighbors<N>& neighbors) const;

    /*  Marks whether this tree is fully constructed */
    std::atomic_int pending;

    /*  Eigenvalue threshold for determining feature rank  */
    constexpr static double EIGENVALUE_CUTOFF=0.1f;
};

extern template class SimplexTree<2>;
extern template class SimplexTree<3>;

}   // namespace Kernel

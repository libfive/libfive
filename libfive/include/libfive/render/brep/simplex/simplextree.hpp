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
#include "libfive/render/brep/util.hpp"
#include "libfive/render/brep/xtree.hpp"
#include "libfive/render/brep/simplex/qef.hpp"

namespace Kernel {

/* Forward declarations */
template <unsigned N> class SimplexNeighbors;

template <unsigned N>
struct SimplexLeaf
{
    SimplexLeaf();
    void reset();

    /*  Subspace vertex positions */
    Eigen::Matrix<double, ipow(3, N), N> vertices;

    /*  Subspace vertex state */
    std::array<bool, ipow(3, N)> inside;

    /*   Global indices for subspace vertices  */
    std::array<uint64_t, ipow(3, N)> index;

    /*  Per-subspace QEFs */
    std::array<QEF<N>, ipow(3, N)> qefs;

    /*  Tape used for evaluation within this leaf */
    std::shared_ptr<Tape> tape;

    /*  Indices of surface vertices, populated when meshing.
     *
     *  The index is a pair of subspace vertex indices.
     *
     *  We can't simply store a fixed number of edges because of
     *  how neighboring cells of varying sizes are meshed. */
    std::map<std::pair<uint64_t, uint64_t>, uint64_t> surface;

    /*  Represents how far from minimum-size leafs we are */
    unsigned level;
};

template <unsigned N>
class SimplexTree : public XTree<N, SimplexTree<N>, SimplexLeaf<N>>
{
public:
    using Leaf = SimplexLeaf<N>;

    /*
     *  Simple constructor
     *
     *  Pointers are initialized to nullptr, but other members
     *  are invalid until reset() is called.
     */
    explicit SimplexTree();
    explicit SimplexTree(SimplexTree<N>* parent, unsigned index,
                         const Region<N>&);
    static std::unique_ptr<SimplexTree> empty();

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
    void evalLeaf(XTreeEvaluator* eval, const SimplexNeighbors<N>& neighbors,
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
    constexpr static uint32_t LEAF_LEVEL_INVALID = UINT32_MAX;
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

protected:
    /*
     *  Helper function to assign leaf->index for all leafs in a tree
     */
    void assignIndices(uint64_t& i, const SimplexNeighbors<N>& neighbors) const;

    /*  Eigenvalue threshold for determining feature rank  */
    constexpr static double EIGENVALUE_CUTOFF=0.1f;
};

extern template class SimplexTree<2>;
extern template class SimplexTree<3>;

}   // namespace Kernel

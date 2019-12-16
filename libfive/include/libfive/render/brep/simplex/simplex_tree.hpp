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

#include "libfive/eval/evaluator.hpp"

#include "libfive/render/brep/util.hpp"
#include "libfive/render/brep/xtree.hpp"
#include "libfive/render/brep/object_pool.hpp"
#include "libfive/render/brep/default_new_delete.hpp"
#include "libfive/render/brep/simplex/qef.hpp"
#include "libfive/render/brep/simplex/surface_edge_map.hpp"

namespace libfive {

/* Forward declarations */
template <unsigned N> class SimplexNeighbors;
template <unsigned N> class Region;
struct BRepSettings;

template <unsigned N>
struct SimplexLeafSubspace {
    SimplexLeafSubspace();
    void reset();

    /*  Subspace vertex position */
    Eigen::Matrix<double, 1, N> vert;

    /*  Subspace vertex state */
    bool inside;

    /*   Global indices for subspace vertices  */
    std::atomic<uint64_t> index;

    /*  Per-subspace QEF */
    QEF<N> qef;

    /*  SimplexLeafSubspace objects are allocated and released to an
     *  object pool, but can be stored by more than one SubspaceLeaf
     *  at a time (since they represent shared spaces).  We use a
     *  homebrew reference counting system to avoid releasing them to
     *  the pool while they're still in use.  */
    std::atomic<uint32_t> refcount;

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

template <unsigned N>
struct SimplexLeaf
{
    SimplexLeaf();
    void reset();

    using Pool = ObjectPool<SimplexLeaf, SimplexLeafSubspace<N>>;
    void releaseTo(Pool& object_pool);

    /*  One QEF structure per subspace in the leaf, shared between neighbors.
     *  These pointers are owned by an object pool, for fast allocation
     *  and re-use. */
    std::array<std::atomic<SimplexLeafSubspace<N>*>, ipow(3, N)> sub;

    /*  Tape used for evaluation within this leaf */
    std::shared_ptr<Tape> tape;

    /*  Indices of surface vertices, populated when meshing.
     *
     *  The index is a pair of subspace vertex indices.
     *
     *  We can't simply store a fixed number of edges because of
     *  how neighboring cells of varying sizes are meshed.  Instead,
     *  we use a pair of small_vectors to act as a stack-allocated
     *  ordered map. */
    SurfaceEdgeMap<32> surface;

    DEFAULT_OPERATORS_NEW_AND_DELETE

    /*  Represents how far from minimum-size leafs we are */
    unsigned level;
};

template <unsigned N>
class SimplexTree : public XTree<N, SimplexTree<N>, SimplexLeaf<N>>
{
public:
    using Pool = ObjectPool<SimplexTree<N>,
                            SimplexLeaf<N>,
                            SimplexLeafSubspace<N>>;

    /*
     *  Simple constructor
     *
     *  Pointers are initialized to nullptr, but other members
     *  are invalid until reset() is called.
     */
    explicit SimplexTree();

    /*
     *  Complete constructor
     */
    explicit SimplexTree(SimplexTree<N>* parent, unsigned index,
                         const Region<N>&);

    /*
     *  Constructs an empty SimplexTree
     *
     *  This only exists for API completeness, and should never
     *  be called.  The returned tree has an invalid parent pointer
     *  and Interval::UNKNOWN as its type.
     */
    static std::unique_ptr<SimplexTree> empty();

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
                  Pool& object_pool,
                  const SimplexNeighbors<N>& neighbors);

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

    /*  Looks up the cell's level for purposes of vertex placement,
     *  returning 0 or more for LEAF / EMPTY / FILLED cells (depending
     *  on how many other leafs were merged into them; 0 is the smallest
     *  leaf).
     *
     *  Returns UINT32_MAX for UNKNOWN cells, which should only be created
     *  with SimplexTree::empty() and are used around the borders of the
     *  model to include those edges.
     *
     *  Triggers an assertion failure if called on a BRANCH cell.
     */
    uint32_t leafLevel() const;

    /*
     *  Assigns leaf->sub[*]->index to a array of unique integers for every leaf
     *  in the tree, starting at 1.  This provides a globally unique
     *  identifier for every subspace vertex, which is used when making edges.
     *
     *  Settings are used for cancellation and worker count.
     */
    void assignIndices(const BRepSettings& settings) const;

    /*
     *  Releases this tree and any leaf objects to the given object pool
     */
    void releaseTo(Pool& object_pool);

    /*  Boilerplate for an object that contains an Eigen struct  */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    /*  Helper typedef for N-dimensional column vector */
    typedef Eigen::Matrix<double, N, 1> Vec;

    static bool hasSingletons() { return false; }
    static SimplexTree<N>* singletonEmpty() { return nullptr; }
    static SimplexTree<N>* singletonFilled() { return nullptr; }
    static bool isSingleton(const SimplexTree<N>*) { return false; }

protected:
    /*
     *  Calculate and store whether each vertex is inside or outside
     *  This populates leaf->sub[i]->inside, for i in 0..ipow(3, N)
     */
    void saveVertexSigns(Evaluator* eval,
                         const Tape::Handle& tape,
                         const std::array<bool, ipow(3, N)>& already_solved);

    /*
     *  Sets this->type to EMPTY / FILLED / AMBIGUOUS depending on
     *  the vertex signs, which must be populated.
     */
    void checkVertexSigns();

    /*
     *  Populates this->leaf->sub[i]->qef for every corner subspace,
     *  then solves for vertex position and signs.
     *
     *  Only corners are evaluated + populated; faces / edges / volumes
     *  are initialized to zero, because they'll be constructed by accumulation
     *  as we walk up the tree.
     */
    void findLeafVertices(Evaluator* eval,
                          const Tape::Handle& tape,
                          Pool& object_pool,
                          const SimplexNeighbors<N>& neighbors);

    /*
     *  Unwraps the atomic pointers, returning an array of plain pointers
     *  Only use this if multiple threads won't be messing with the pointers
     *  simultaneously!
     */
    std::array<SimplexLeafSubspace<N>*, ipow(3, N)> getLeafSubs() const;
};

}   // namespace libfive

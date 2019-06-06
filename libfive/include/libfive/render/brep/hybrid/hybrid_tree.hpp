/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <array>

#include "libfive/eval/eval_xtree.hpp"

#include "libfive/render/brep/util.hpp"
#include "libfive/render/brep/xtree.hpp"
#include "libfive/render/brep/object_pool.hpp"
#include "libfive/render/brep/simplex/surface_edge_map.hpp"
#include "libfive/render/brep/simplex/qef.hpp"

namespace Kernel {

/* Forward declaration */
template <unsigned N> class Region;
template <unsigned N> class HybridNeighbors;
struct BRepSettings;

template <unsigned N>
struct HybridLeaf
{
    HybridLeaf();
    void reset();

    Eigen::Matrix<double, N, ipow(3, N)> pos;
    Eigen::Matrix<double, N + 1, ipow(3, N)> mass_point;
    std::array<bool, ipow(3, N)> inside;
    std::array<bool, ipow(3, N)> on_surface;
    std::array<QEF<N>, ipow(3, N)> qef;

    /* Check whether this point is an intersection by seeing
     * whether we've accumulated any mass points. */
    bool intersection(unsigned i) { return mass_point(N, i) != 0; }

    /* Unique indexes for every subspace, shared when subspaces are
     * shared by more than one neighbor. */
    std::array<uint32_t, ipow(3, N)> index;

    /*  Indices of surface vertices, populated when meshing.
     *  This maps from a pair of subspace vertex indexes to a mesh index */
    SurfaceEdgeMap<32> surface;

    /*  Tape used for evaluation within this leaf */
    Tape::Handle tape;

    EIGEN_MAKE_ALIGNED_OPERATOR_NEW
};

template <unsigned N>
class HybridTree : public XTree<N, HybridTree<N>, HybridLeaf<N>>
{
public:
    using Leaf = HybridLeaf<N>;
    using Pool = ObjectPool<HybridTree<N>, HybridLeaf<N>>;

    /*
     *  Simple constructor
     *
     *  Pointers are initialized to nullptr, but other members
     *  are invalid until reset() is called.
     */
    explicit HybridTree();
    explicit HybridTree(HybridTree<N>* parent, unsigned index,
                        const Region<N>& region);
    static std::unique_ptr<HybridTree<N>> empty();

    /*
     *  Populates type, setting corners, manifold, and done if this region is
     *  fully inside or outside the mode.
     *
     *  Returns a shorter version of the tape that ignores unambiguous clauses.
     */
    std::shared_ptr<Tape> evalInterval(XTreeEvaluator* eval,
                                       std::shared_ptr<Tape> tape,
                                       const Region<N>& region,
                                       Pool& object_pool);

    /*
     *  Evaluates a minimum-size octree node.
     *  Sets type to FILLED / EMPTY / AMBIGUOUS based on the corner values.
     */
    void evalLeaf(XTreeEvaluator* eval,
                  std::shared_ptr<Tape> tape,
                  const Region<N>& region,
                  Pool& spare_leafs,
                  const HybridNeighbors<N>& neighbors);

    /*
     *  If all children are present, then collapse cells based on error
     *  metrics (TODO; cell collapsing is not implemented).
     *
     *  Returns false if any children are yet to come, true otherwise.
     */
    bool collectChildren(XTreeEvaluator* eval,
                         std::shared_ptr<Tape> tape,
                         const Region<N>& region,
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
     *  Assigns leaf->index[*] to a unique integer for every leaf subspace
     *  in the tree, starting at 1.  This provides a globally unique
     *  identifier for every subspace vertex, which is used when making edges.
     *
     *  settings are used for cancellation and worker count.
     */
    void assignIndices(const BRepSettings& settings) const;

    /*
     *  Maps a function across every terminal cell in the tree (which may
     *  be a leaf or an empty / filled region), using naive recursion.
     */
    template <typename F>
    void map(F& f) const {
        if (this->isBranch()) {
            for (auto& c : this->children) {
                c.load()->map(f);
            }
        }
        else {
            f(this);
        }
    }

    /*  Helper typedef for N-dimensional column vector */
    typedef Eigen::Matrix<double, N, 1> Vec;

    /*  Boilerplate for an object that contains an Eigen struct  */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    /*
     *  Releases this tree and any leaf objects to the given object pool
     */
    void releaseTo(Pool& object_pool);

    /*
     *  We've found a point of interest on some subspace.  Now, we need
     *  to solve for insideness, and decide what to accumulate for the QEF:
     *   - This point's position / normal / distance, or
     *   - Intersections on edges between this point and its neighbors
     *
     *   We'd prefer to store intersections, because they make it easier to
     *   cleave to the surface, but they may not exist on this particular
     *   subspace.
     *
     *   This function is public because the template-madness Unroller struct
     *   needs to be able to call it, but should not be considered part
     *   of the public API for the HybridTree.
     */
    void placeSubspaceVertex(
        XTreeEvaluator* eval, Tape::Handle tape,
        const Region<N>& region,
        NeighborIndex n, const Vec& pos);

protected:

    /*
     *  Process all subspaces of dimension D.  This means:
     *      Positioning a vertex on each subspace of dimension D
     *      Recording that vertex's inside/outside status
     *      Building a QEF for that subspace, either from the vertex
     *          position and normal or searching for intersections and
     *          using them instead.
     *
     *  processSubspaces<0> is special-cased to only process corners,
     *  which cannot have intersections and so get a simple QEF
     *
     *  processSubspaces<1> is special-cased to place vertices using
     *  binary search for edges with a sign change, rather than
     *  using Dual Contouring's algorithm.  This lets us more precisely
     *  position the vertex on the surface of the model.
     */
    void processCorners(XTreeEvaluator* eval,
                        Tape::Handle tape,
                        const Region<N>& region);

    /*  We use the same logic for faces and cubes, so it's templated here */
    template <unsigned D>
    void processSubspaces(XTreeEvaluator* eval,
                          Tape::Handle tape,
                          const Region<N>& region);

    /*
     *  After loading the first count slots of eval with data, this function
     *  evaluates them and accumulates to the subspace QEFs specified by
     *  targets[0..count]
     */
    void accumulate(XTreeEvaluator* eval,
                    Tape::Handle tape,
                    unsigned count,
                    NeighborIndex* target);

    /*
     *  Asserts that the leaf is null, pulls a fresh leaf from the object
     *  pool, and assigns it the given tape.
     *
     *  Then, calls processCorners, processEdges, and processSubspaces
     *  to fill out all of the leaf data.
     */
    void buildLeaf(XTreeEvaluator* eval,
                   std::shared_ptr<Tape> tape,
                   const Region<N>& region,
                   Pool& object_pool);
};

extern template class HybridTree<3>;

}   // namespace Kernel

/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <array>

#include "libfive/eval/evaluator.hpp"

#include "libfive/render/brep/util.hpp"
#include "libfive/render/brep/xtree.hpp"
#include "libfive/render/brep/object_pool.hpp"
#include "libfive/render/brep/simplex/surface_edge_map.hpp"
#include "libfive/render/brep/simplex/qef.hpp"

namespace libfive {

/* Forward declaration */
template <unsigned N> class Region;
template <unsigned N> class HybridNeighbors;
struct BRepSettings;

template <unsigned N>
struct HybridLeaf
{
    HybridLeaf();
    void reset();

    /*  This represents the position of the subspace vertices.
     *  Each subspace vertex is positioned either on the model's surface
     *  (in which case vertex_on_surface[i] is true) or on a sharp feature
     *  of the distance field itself.  */
    Eigen::Matrix<double, N, ipow(3, N)> vertex_pos;

    /*  Marks whether the subspace vertex is inside or outside the model. */
    std::array<bool, ipow(3, N)> inside;

    /*  If this is true, then qef[i] represents a normalized QEF based on
     *  exact surface samples.  It is never true for corners, which always
     *  contain (non-normalized) distance-field QEFs. */
    std::array<bool, ipow(3, N)> has_surface_qef;

    /*  Represents a distance-field QEF for corners, a normalized surface QEF
     *  when has_surface_qef[i] is true, and empty otherwise. */
    std::array<QEF<N>, ipow(3, N)> qef;

    /*  True when the vertex position is on the model's surface.  If this is
     *  true, then meshing should snap to the vertex position (instead of
     *  doing an edge search). */
    std::array<bool, ipow(3, N)> vertex_on_surface;

    /*  Represents the mass-point center of surface intersections.  This is
     *  only relevant when has_surface_qef is true.  If has_surface_qef is
     *  false, then we don't care about the mass point for this subspace.
     *
     *  If vertex_on_surface is true, then surface_mass_point is the
     *  same as vertex_pos. */
    Eigen::Matrix<double, N + 1, ipow(3, N)> surface_mass_point;

    /*  This represents the rank of the mass point, which is used when
     *  accumulating: we only accumulate mass points from max-rank
     *  subspaces, to give us a better target for minimization.
     *
     *  This is only relevant when has_surface_qef is true. */
    std::array<unsigned, ipow(3, N)> surface_rank;

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
    std::shared_ptr<Tape> evalInterval(Evaluator* eval,
                                       const std::shared_ptr<Tape>& tape,
                                       Pool& object_pool);

    /*
     *  Evaluates a minimum-size octree node.
     *  Sets type to FILLED / EMPTY / AMBIGUOUS based on the corner values.
     */
    void evalLeaf(Evaluator* eval,
                  const std::shared_ptr<Tape>& tape,
                  Pool& spare_leafs,
                  const HybridNeighbors<N>& neighbors);

    /*
     *  If all children are present, then collapse cells based on error
     *  metrics (TODO; cell collapsing is not implemented).
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
     *  to record the point, and decide whether to accumulate a distance-field
     *  or surface QEF, depending on whether there are surface intersections
     *  on the edges between this vertex and its subspace neighbors.
     *
     *  This function is public because the template-madness Unroller struct
     *  needs to be able to call it, but should not be considered part
     *  of the public API for the HybridTree.
     */
    void placeDistanceVertex(
        Evaluator* eval, const Tape::Handle& tape,
        NeighborIndex n, const Vec& pos);

    /*
     *  After loading the first count columns of positions with data, this
     *  function evaluates them and accumulates to the subspace QEFs specified
     *  by targets[0..count]
     *
     *  If normalize is true, then the normals are normalized for better
     *  Dual Contouring stability.
     *
     *  This function is public because the template-madness Unroller struct
     *  needs to be able to call it, but should not be considered part
     *  of the public API for the HybridTree.
     */
    void accumulate(Evaluator* eval,
                    const Eigen::Array<double, N, ArrayEvaluator::N>& positions,
                    const Tape::Handle& tape,
                    unsigned count,
                    NeighborIndex* target,
                    bool normalize);

    static bool hasSingletons() { return false; }
    static HybridTree<N>* singletonEmpty() { return nullptr; }
    static HybridTree<N>* singletonFilled() { return nullptr; }
    static bool isSingleton(const HybridTree<N>*) { return false; }

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
    void processCorners(Evaluator* eval,
                        const Tape::Handle& tape);

    /*  We use the same logic for faces and cubes, so it's templated here */
    template <unsigned D>
    void processSubspaces(Evaluator* eval,
                          const Tape::Handle& tape);

    /*
     *  Asserts that the leaf is null, pulls a fresh leaf from the object
     *  pool, and assigns it the given tape.
     *
     *  Then, calls processCorners, processEdges, and processSubspaces
     *  to fill out all of the leaf data.
     */
    void buildLeaf(Evaluator* eval,
                   const std::shared_ptr<Tape>& tape,
                   Pool& object_pool);
};

}   // namespace libfive

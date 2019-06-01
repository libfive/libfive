/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "libfive/eval/eval_xtree.hpp"
#include "libfive/eval/tape.hpp"

#include "libfive/render/axes.hpp"
#include "libfive/render/brep/hybrid/hybrid_tree.hpp"
#include "libfive/render/brep/hybrid/hybrid_neighbors.hpp"
#include "libfive/render/brep/region.hpp"
#include "libfive/render/brep/settings.hpp"
#include "libfive/render/brep/neighbor_tables.hpp"
#include "libfive/render/brep/edge_tables.hpp"
#include "libfive/render/brep/manifold_tables.hpp"

#include "../xtree.cpp"

//#define DEBUG(s) std::cout << s << "\n"
#define DEBUG(s) {}

namespace Kernel {

/*
 *  Helper function to pack from a higher dimension to a lower one
 */
template <unsigned BaseDimension, unsigned Target>
Eigen::Matrix<double, NeighborIndex(Target).dimension(), 1> pack(
        const Eigen::Matrix<double, BaseDimension, 1>& in)
{
    constexpr NeighborIndex n(Target);
    constexpr auto TargetFloating = n.floating();

    Eigen::Matrix<double, NeighborIndex(Target).dimension(), 1> out;
    unsigned j=0;
    for (unsigned i=0; i < BaseDimension; ++i) {
        if (TargetFloating & (1 << i)) {
            out[j++] = in[i];
        }
    }
    assert(j == NeighborIndex(Target).dimension());
    return out;
}

/*
 *  Helper function to unpack from a lower dimension to a higher one, using
 *  a Region to fill in the unspecified axes.
 */
template <unsigned BaseDimension, unsigned Target>
Eigen::Matrix<double, BaseDimension, 1> unpack(
        const Eigen::Matrix<double, NeighborIndex(Target).dimension(), 1>& in,
        const Region<BaseDimension>& region)
{
    constexpr NeighborIndex n(Target);
    constexpr auto TargetFloating = n.floating();
    constexpr auto TargetPosition = n.pos();

    Eigen::Matrix<double, BaseDimension, 1> out;
    unsigned j = 0;
    for (unsigned i=0; i < BaseDimension; ++i) {
        if (TargetFloating & (1 << i)) {
            out(i) = in(j++);
        } else if (TargetPosition & (1 << i)) {
            out(i) = region.upper(i);
        } else {
            out(i) = region.lower(i);
        }
    }
    assert(j == NeighborIndex(Target).dimension());
    return out;
}

template <unsigned N>
using MassPoint = Eigen::Matrix<double, N + 1, 1>;

template <unsigned N>
Eigen::Matrix<double, N, 1> unMassPoint(const MassPoint<N>& m) {
    return m.template head<N>() / m(N);
}

/*
 *  Searches between a point inside the model and outside,
 *  returning a point that's approximately on the surface.
 */
template <unsigned N>
Eigen::Matrix<double, N, 2> searchBetween(
        XTreeEvaluator* eval, Tape::Handle tape,
        const Region<N>& region,
        Eigen::Matrix<double, N, 1> inside,
        Eigen::Matrix<double, N, 1> outside)
{
    // Copied from simplex_mesher.cpp
    // TODO: unify all of these various implementations
    assert(tape.get() != nullptr);

    // There's an interesting question of precision + speed tradeoffs,
    // which mostly depend on how well evaluation scales in the
    // ArrayEvaluator.  for now, we'll use the same value as XTree.
    constexpr int SEARCH_COUNT = 4;
    constexpr int POINTS_PER_SEARCH = 16;
    static_assert(POINTS_PER_SEARCH <= ArrayEvaluator::N,
                  "Overflowing ArrayEvaluator data array");

    // Multi-stage binary search for intersection
    for (int s=0; s < SEARCH_COUNT; ++s)
    {
        // Load search points into the evaluator
        Eigen::Array<double, N, POINTS_PER_SEARCH> ps;
        for (int j=0; j < POINTS_PER_SEARCH; ++j)
        {
                const double frac = j / (POINTS_PER_SEARCH - 1.0);
                ps.col(j) = (inside * (1 - frac)) + (outside * frac);
                eval->array.set<N>(ps.col(j), region, j);
        }

        auto out = eval->array.values(POINTS_PER_SEARCH, tape);

        // Skip one point, because the very first point is
        // already known to be inside the shape (but
        // sometimes, due to numerical issues, it registers
        // as outside!)
        for (unsigned j=1; j < POINTS_PER_SEARCH; ++j)
        {
            // We're searching for the first point that's outside of the
            // surface.  There's a special case for the final point in the
            // search, working around  numerical issues where different
            // evaluators disagree with whether points are inside or outside.
            if (out[j] > 0 || j == POINTS_PER_SEARCH - 1 ||
                (out[j] == 0 && !eval->feature.isInside<N>(
                    ps.col(j), region, tape)))
            {
                inside = ps.col(j - 1);
                outside = ps.col(j);
                break;
            }
        }
    }

    Eigen::Matrix<double, N, 2> out;
    out.col(0) = inside;
    out.col(1) = outside;
    return out;
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
HybridTree<N>::HybridTree(HybridTree<N>* parent, unsigned index,
                          const Region<N>& r)
    : XTree<N, HybridTree<N>, HybridLeaf<N>>(parent, index, r)
{
    // Nothing to do here
}

template <unsigned N>
HybridTree<N>::HybridTree()
    : XTree<N, HybridTree<N>, HybridLeaf<N>>()
{
    // Nothing to do here
}

template <unsigned N>
std::unique_ptr<HybridTree<N>> HybridTree<N>::empty()
{
    std::unique_ptr<HybridTree> t(new HybridTree);
    t->type = Interval::UNKNOWN;
    return std::move(t);
}

template <unsigned N>
HybridLeaf<N>::HybridLeaf()
{
    reset();
}

template <unsigned N>
void HybridLeaf<N>::reset()
{
    std::fill(inside.begin(), inside.end(), false);
    std::fill(on_surface.begin(), on_surface.end(), false);
    std::fill(index.begin(), index.end(), 0);

    surface.clear();
    tape.reset();
    for (auto& q : qef) {
        q.reset();
    }
    mass_point.array() = 0.0;
    pos.array() = 0.0;
}

template <unsigned N>
Tape::Handle HybridTree<N>::evalInterval(XTreeEvaluator* eval,
                                         Tape::Handle tape,
                                         const Region<N>& region,
                                         Pool& object_pool)
{
    // Do a preliminary evaluation to prune the tree, storing the interval
    // result and an handle to the pushed tape (which we'll use when recursing)
    auto o = eval->interval.evalAndPush(
            region.lower3().template cast<float>(),
            region.upper3().template cast<float>(),
            tape);

    this->type = Interval::state(o.first);
    if (!eval->interval.isSafe())
    {
        this->type = Interval::AMBIGUOUS;
        return tape;
    }

    if (this->type == Interval::FILLED || this->type == Interval::EMPTY)
    {
        buildLeaf(eval, tape, region, object_pool);
        this->done();
    }
    return o.second;
}


template <unsigned N>
void HybridTree<N>::buildLeaf(XTreeEvaluator* eval,
                              std::shared_ptr<Tape> tape,
                              const Region<N>& region,
                              Pool& object_pool)
{
    assert(this->leaf == nullptr);
    this->leaf = object_pool.next().get();
    this->leaf->tape = tape;

    processCorners(eval, tape, region);
    processSubspaces<1>(eval, tape, region);
    processSubspaces<2>(eval, tape, region);
    if (N == 3) {
        processSubspaces<3>(eval, tape, region);
    }
}

template <unsigned N>
void HybridTree<N>::processCorners(XTreeEvaluator* eval,
                                   Tape::Handle tape,
                                   const Region<N>& region)
{
    std::array<NeighborIndex, ipow(2, N)> targets;
    for (unsigned i=0; i < ipow(2, N); ++i) {
        eval->array.set(region.corner3f(i), i);
        targets[i] = CornerIndex(i).neighbor();
        this->leaf->pos.col(targets[i].i) = region.corner(i);

        // TODO: don't use the full-power evaluator when not needed
        this->leaf->inside[targets[i].i] = eval->feature.isInside(
                region.corner3f(i), tape);
    }
    // Store QEF data for this new vertex
    accumulate(eval, tape, targets.size(), targets.data());
}

template <unsigned BaseDimension, unsigned Target>
void processEdge(HybridTree<BaseDimension>* tree,
                 XTreeEvaluator* eval,
                 Tape::Handle tape,
                 const Region<BaseDimension>& region)
{
    constexpr NeighborIndex edge(Target);
    assert(edge.dimension() == 1);

    constexpr auto TargetDimension = edge.dimension();
    constexpr auto TargetFloating = edge.floating();

    // Now, we'll check to see whether we have any intersections
    // on a child subspace, to decide whether to place the vertex
    // on a sharp feature of the surface or of the distance field.
    bool has_inside = false;
    bool has_outside = false;
    Eigen::Matrix<double, BaseDimension, 1> inside, outside;
    for (unsigned j=0; j < ipow(3, BaseDimension); ++j) {
        if (edge.i != j && edge.contains(NeighborIndex(j))) {
            if (tree->leaf->inside[j]) {
                has_inside = true;
                inside = tree->leaf->pos.col(j);
            } else {
                has_outside = true;
                outside = tree->leaf->pos.col(j);
            }
        }
    }

    if (has_inside && has_outside) {
        // If there's a sign change, then do a binary search to
        // find the exact point of intersection, marking the resulting
        // point as a surface feature.
        tree->leaf->on_surface[edge.i] = true;
        auto surf = searchBetween<BaseDimension>(eval, tape, region,
                                                 inside, outside);
        // TODO: weigh this based on distance
        Eigen::Matrix<double, BaseDimension, 1> out =
            (surf.col(0) + surf.col(1)) / 2;
        tree->placeSubspaceVertex(eval, tape, region, edge, out);
    } else {
        // If the edge doesn't have an obvious sign change,
        // then place a vertex on a sharp feature of the distance
        // field (which will hopefully be a sign change, if a sign
        // change exists along this edge).
        QEF<BaseDimension> qef;
        for (unsigned j=0; j < ipow(3, BaseDimension); ++j) {
            if (edge.i != j && edge.contains(NeighborIndex(j))) {
                qef += tree->leaf->qef[j];
            }
        }
        const auto region_ = region.template subspace<TargetFloating>();
        QEF<TargetDimension> qef_ = qef.template sub<TargetFloating>();

        // Bounded, minimizing towards the center of the region
        // (which is the of the edge in this case)
        auto sol = qef_.solveBounded(region_, 1);
        DEBUG("Solved edge " << edge.i << " with error " << sol.error);

        // Unpack from the reduced-dimension solution to the leaf vertex
        auto out = unpack<BaseDimension, Target>(sol.position, region);
        tree->placeSubspaceVertex(eval,tape, region, edge, out);
    }
}

template <unsigned BaseDimension, int Target>
void process(HybridTree<BaseDimension>* tree,
             XTreeEvaluator* eval,
             Tape::Handle tape,
             const Region<BaseDimension>& region)
{
    static_assert(Target >= 0, "Invalid Target subspace");
    constexpr NeighborIndex n(Target);
    constexpr auto TargetDimension = n.dimension();
    constexpr auto TargetFloating = n.floating();

    // Now, we'll check to see whether we have any intersections
    // on a child subspace, to decide whether to place the vertex
    // on a sharp feature of the surface or of the distance field.
    QEF<BaseDimension> qef_surface, qef_distance;
    MassPoint<BaseDimension> mass_point_surface;
    mass_point_surface.array() = 0.0;

    uint32_t filled_mask = 0;
    unsigned filled_mask_bit = 0;
    for (unsigned j=0; j < ipow(3, BaseDimension); ++j) {
        NeighborIndex m(j);
        if (n.i != j && n.contains(m)) {
            // Build up a bitmask of occupied perimeter subspaces, so that
            // we can check for manifoldness later.
            if (tree->leaf->inside[j]) {
                filled_mask |= (1 << filled_mask_bit);
            }
            filled_mask_bit++;

            // This part is a bit subtle:  vertices on 2D subsurfaces (faces)
            // that are 'surface' vertices don't add any useful data, so we
            // skip them when accumulating QEFs.  This is because their data
            // should be contained within the edge QEFs that went into
            // constructing them, and we don't want to double-count it.
            if (!(m.dimension() == 2 && tree->leaf->on_surface[m.i]) &&
                tree->leaf->intersection(j))
            {
                qef_surface += tree->leaf->qef[j];
                mass_point_surface += tree->leaf->mass_point.col(j);
            }
            if (m.dimension() == 0) {
                qef_distance += tree->leaf->qef[j];
            }
        }
    }

    // First, run the solver to position the vertex on a sharp feature of
    // the distance field itself.  This is more robust, but worse at
    // positioning the final model vertices on corners and edges.
    const auto region_ = region.template subspace<TargetFloating>();

    DEBUG("Solving for subspace " << Target);

    // If we have found one or more intersections and the surface is locally
    // manifold, then we try to position the vertex using Dual Contouring.
    if (mass_point_surface[BaseDimension] != 0 &&
        ManifoldTables<BaseDimension>::manifold(filled_mask))
    {
        // Solve for DC-style position
        QEF<TargetDimension> qef_surf_ = qef_surface
            .template sub<TargetFloating>();
        const auto target_pos_surf = unMassPoint<BaseDimension>(
                mass_point_surface);
        const auto target_pos_surf_ = pack<BaseDimension, Target>(
                target_pos_surf);
        auto sol_surf = qef_surf_.solveDC(target_pos_surf_);
        auto v_surf = unpack<BaseDimension, Target>(sol_surf.position, region);

        // Add a bit of padding + clamping to the DC vertex, in case it
        // ended up just outside of the valid region.  The epsilon in this
        // case is relative to the region size, rather than fixed.
        bool within_region = false;
        if (region.shrink(1 + 1e-9).contains(v_surf, 0)) {
            if (!region.contains(v_surf, 0)) {
                v_surf = v_surf.cwiseMax(region.lower.matrix());
                v_surf = v_surf.cwiseMin(region.upper.matrix());
            }
            within_region = true;
        }
        DEBUG("  found DC vertex at " << v_surf.transpose());
        DEBUG("      rank: " << sol_surf.rank);
        DEBUG("      target pos: " << target_pos_surf.transpose());
        DEBUG("      error: " << sol_surf.error);

        // Then, try solving for a sharp feature on the distance field itself,
        // using the DC-chosen point as a starting point if it's within the
        // cell's boundaries.
        QEF<TargetDimension> qef_dist_ = qef_distance
            .template sub<TargetFloating>();
        auto sol_dist = qef_dist_.solveBounded(region_, 1);
        auto v_dist = unpack<BaseDimension, Target>(sol_dist.position, region);
        DEBUG("  found distance vertex at " << v_dist.transpose());
        DEBUG("      rank: " << sol_dist.rank);
        DEBUG("      error: " << sol_dist.error);

        // If we successfully placed the vertex using Dual Contouring
        // rules, then mark that the resulting vertex is a surface vertex.
        if (within_region && (sol_surf.error <= 1e-12
                    || sol_surf.error / 10.0 < sol_dist.error
                    || (v_dist - v_surf).norm() < 1e-12))
        {
            DEBUG("      Placed DC vertex");
            tree->placeSubspaceVertex(eval, tape, region, n, v_surf);
            tree->leaf->on_surface[n.i] = true;
        } else {
            DEBUG("      Placing distance vertex");
            tree->placeSubspaceVertex(eval, tape, region, n, v_dist);
        }
    } else {
        // Solve the distance-field sharp-feature QEF
        // TODO: should we be optimizing towards the center, or somewhere else?
        QEF<TargetDimension> qef_ = qef_distance.template sub<TargetFloating>();
        auto sol = qef_.solveBounded(region_, 1);

        // Unpack from the reduced-dimension solution to the leaf vertex
        auto out = unpack<BaseDimension, Target>(sol.position, region);

        DEBUG(" placing distance vertex at " << n.i << " " << out.transpose());
        DEBUG("  rank: " << sol.rank);
        DEBUG("  error: " << sol.error);
        tree->placeSubspaceVertex(eval, tape, region, n, out);
    }
}


template <unsigned BaseDimension, unsigned TargetDimension, int Target>
struct Unroller {
    void run(HybridTree<BaseDimension>* tree,
             XTreeEvaluator* eval,
             Tape::Handle tape,
             const Region<BaseDimension>& region)
    {
        if (NeighborIndex(Target).dimension() == TargetDimension) {
            if (TargetDimension > 1) {
                process<BaseDimension, Target>(tree, eval, tape, region);
            } else {
                processEdge<BaseDimension, Target>(tree, eval, tape, region);
            }
        }
        Unroller<BaseDimension, TargetDimension, Target - 1>()
            .run(tree, eval, tape, region);
    }
};

template <unsigned BaseDimension, unsigned TargetDimension>
struct Unroller<BaseDimension, TargetDimension, -1> {
    void run(HybridTree<BaseDimension>*,
                 XTreeEvaluator*,
                 Tape::Handle,
                 const Region<BaseDimension>&)
    {
        // Terminate static unrolling here
    }
};


template <unsigned N>
template<unsigned D>
void HybridTree<N>::processSubspaces(XTreeEvaluator* eval,
                                     Tape::Handle tape,
                                     const Region<N>& region)
{
    Unroller<N, D, ipow(3, N) - 1>().run(this, eval, tape, region);
}

template <unsigned N>
void HybridTree<N>::placeSubspaceVertex(
        XTreeEvaluator* eval, Tape::Handle tape,
        const Region<N>& region,
        NeighborIndex n, const Vec& pos)
{
    // Store this edge's position
    this->leaf->pos.col(n.i) = pos;

    // Expensive check for inside / outsideness of this point (TODO)
    this->leaf->inside[n.i] = eval->feature.isInside<N>(pos, region, tape);

    Eigen::Matrix<double, N, ipow(3, N) * 2> intersections;
    // Check every edge from the new point to its neighbouring subspaces,
    // seeing whether there's a sign change and searching the edge
    // if that's the case
    unsigned num_intersections = 0;
    for (auto& t: EdgeTables<N>::subspaces(n.i)) {
        // If there isn't a sign change along this subspace-to-subspace
        // edge, then we shouldn't search it.
        assert(t.dimension() < n.dimension());
        if (this->leaf->inside[t.i] == this->leaf->inside[n.i]) {
            continue;
        }

        Vec inside, outside;
        if (this->leaf->inside[t.i]) {
            inside = this->leaf->pos.col(t.i);
            outside = this->leaf->pos.col(n.i);
        } else {
            inside = this->leaf->pos.col(n.i);
            outside = this->leaf->pos.col(t.i);
        }
        auto p = searchBetween<N>(eval, tape, region, inside, outside);

        for (unsigned i=0; i < 2; ++i) {
            MassPoint<N> mp;
            mp << p.col(i), 1;
            this->leaf->mass_point.col(n.i) += mp;

            // Store the intersection point into the QEF target list
            intersections.col(num_intersections++) = p.col(i);
        }
    }
    assert(num_intersections <= intersections.cols());

    if (num_intersections) {
        // Pack the target points into the array
        for (unsigned i=0; i < num_intersections; ++i) {
            eval->array.set<N>(intersections.col(i), region, i);
        }

        // This is a bit silly, because every intersection ends up accumulated
        // into the same QEF, but it makes accumulate() more flexible
        std::array<NeighborIndex, ipow(3, N) * 2> targets;
        std::fill(targets.begin(), targets.end(), n);
        assert(targets.size() >= num_intersections);

        accumulate(eval, tape, num_intersections, targets.data());
    } else {
        // If we didn't find a sign change, then store the vertex itself into
        // the subspace QEF.
        eval->array.set<N>(pos, region, 0);
        accumulate(eval, tape, 1, &n);
    }
}

template <unsigned N>
void HybridTree<N>::accumulate(XTreeEvaluator* eval,
                               Tape::Handle tape,
                               unsigned count,
                               NeighborIndex* target)
{
    auto ds = eval->array.derivs(count);
    auto ambig = eval->array.getAmbiguous(count);

    auto push = [&ds, &eval, &target, this](Eigen::Vector3f d, unsigned i) {
        Eigen::Matrix<double, N, 1> d_ =
            d.template head<N>().template cast<double>();
        if (!d_.array().isFinite().all()) {
            d_.array() = 0.0;
        }
        this->leaf->qef[target[i].i].insert(
                eval->array.get(i).template cast<double>()
                                  .template head<N>(),
                d_, ds(3, i));
    };

    for (unsigned i=0; i < count; ++i) {
        if (!ambig[i]) {
            push(ds.col(i).head<3>(), i);
        } else {
            const auto fs = eval->feature.features(
                    eval->array.get(i), tape);
            for (const auto& f : fs) {
                push(f, i);
            }
        }
    }

}

template <unsigned N>
void HybridTree<N>::evalLeaf(XTreeEvaluator* eval,
                             Tape::Handle tape,
                             const Region<N>& region,
                             Pool& object_pool,
                             const HybridNeighbors<N>& neighbors)
{
    (void)neighbors;

    buildLeaf(eval, tape, region, object_pool);

    bool all_empty = true;
    bool all_full  = true;
    for (unsigned i=0; i < ipow(3, N); ++i) {
        all_empty  &= !this->leaf->inside[i];
        all_full   &=  this->leaf->inside[i];
    }

    assert(this->type == Interval::UNKNOWN);
    this->type = all_empty ? Interval::EMPTY
               : all_full  ? Interval::FILLED : Interval::AMBIGUOUS;

    this->done();
}

template <unsigned N>
bool HybridTree<N>::collectChildren(XTreeEvaluator* eval,
                                    Tape::Handle tape,
                                    const Region<N>& region,
                                    Pool& object_pool,
                                    double max_err)
{
    // Wait for collectChildren to have been called N times
    if (this->pending-- != 0)
    {
        return false;
    }

    // Load the children here, to avoid atomics
    std::array<HybridTree<N>*, 1 << N> cs;
    for (unsigned i=0; i < this->children.size(); ++i)
    {
        cs[i] = this->children[i].load(std::memory_order_relaxed);
    }

    // If any children are branches, then we can't collapse.
    // We do this check first, to avoid allocating then freeing a Leaf
    if (std::any_of(cs.begin(), cs.end(),
                    [](HybridTree<N>* o){ return o->isBranch(); }))
    {
        this->done();
        return true;
    }

    // Update filled / empty state from children
    bool all_empty = true;
    bool all_full  = true;
    for (uint8_t i=0; i < cs.size(); ++i)
    {
        auto c = cs[i];
        assert(c != nullptr);

        all_empty &= (c->type == Interval::EMPTY);
        all_full  &= (c->type == Interval::FILLED);
    }

    this->type = all_empty ? Interval::EMPTY
               : all_full  ? Interval::FILLED : Interval::AMBIGUOUS;

    // If this cell is unambiguous, then forget all its branches and return
    if (this->type == Interval::FILLED || this->type == Interval::EMPTY)
    {
        this->releaseChildren(object_pool);
        buildLeaf(eval, tape, region, object_pool);
        this->done();
        return true;
    }

    // Eventually, we'll use these variables to perhaps collapse the tree
    (void)eval;
    (void)tape;
    (void)region;
    (void)max_err;

    this->done();
    return true;
}


template <unsigned N>
void HybridTree<N>::releaseTo(Pool& object_pool) {
    if (this->leaf != nullptr) {
        object_pool.next().put(this->leaf);
        this->leaf = nullptr;
    }

    object_pool.put(this);
}

template <unsigned N>
uint32_t HybridTree<N>::leafLevel() const
{
    assert(!this->isBranch());
    switch (this->type)
    {
        case Interval::FILLED:  // fallthrough
        case Interval::EMPTY:   // fallthrough
        case Interval::AMBIGUOUS:
            assert(this->leaf != nullptr);
            assert(this->region.level >= 0);
            return this->region.level;

        case Interval::UNKNOWN: return UINT32_MAX;
    };
    return 0;
}

/*  This helper struct lets us make a directed acyclic graph of
 *  tasks + neighbors, automatically cleaning them up when no one
 *  is using them anymore.  */
template <unsigned N>
struct AssignIndexTask {
    const HybridTree<N>* target;
    HybridNeighbors<N> neighbors;
    std::shared_ptr<AssignIndexTask<N>> parent;
};

template <unsigned N>
void HybridTree<N>::assignIndices(const BRepSettings& settings) const
{
    (void)settings; // TODO: multithreading and cancellation

    // We do a depth-first search here, flattened into a vector
    // to more easily convert to a multithreaded operation
    // (like the one in simplex_tree.cpp)
    using Task = std::shared_ptr<AssignIndexTask<N>>;
    std::stack<Task, std::vector<Task>> todo;
    auto task = std::make_shared<AssignIndexTask<N>>();
    task->target = this;
    todo.push(task);

    uint32_t index = 0;
    while (todo.size()) {
        const auto task = todo.top();
        todo.pop();

        // If this is a tree which can be subdivided, then push each
        // subtree as a new task.
        if (task->target->isBranch()) {
            for (unsigned i=0; i < task->target->children.size(); ++i) {
                const auto child = task->target->children[i].load();
                auto next = std::make_shared<AssignIndexTask<N>>();
                next->target = child;
                next->neighbors = task->neighbors.push(i, task->target->children);
                next->parent = task;
                todo.push(next);
            }
            continue;
        }

        // Try to assign a new value to every slot in the index array
        //
        // If the index is already assigned skip it.  Otherwise, spread the
        // value to every other index field that represents the same subspace.
        assert(task->target->leaf != nullptr);
        for (unsigned i_=0; i_ < ipow(3, N); ++i_)
        {
            // Skip if the index is already assigned
            if (task->target->leaf->index[i_] != 0) {
                continue;
            }

            // Record the new index here
            const auto my_index = ++index;
            task->target->leaf->index[i_] = my_index;

            // Functor to assign the new index to a given tree + neighbor index
            auto f = [my_index](const HybridTree<N>* t, NeighborIndex n) {
                assert(t->leaf->index[n.i] == 0 ||
                       t->leaf->index[n.i] == my_index);
                t->leaf->index[n.i] = my_index;
            };

            // Apply the functor to local neighbors
            NeighborIndex i(i_);
            task->neighbors.map(i, f);

            // We need to try walking up the tree, looking at the
            // neighbors of parent cells as long as they contain the target
            // vertex.  For example, in this situation:
            //
            //   -------------------------
            //   |           |           |
            //   |           |           |
            //   |           |           |
            //   ------------C------------
            //   |     |  X  |           |
            //   |-----|-----|           |
            //   |     |     |           |
            //   -------------------------
            //   we'd look at neighbors of X's parent cell to find corner C
            //
            //   On the other hand, in this situation:
            //   -------------------------
            //   |           |           |
            //   |           |           |
            //   |           |           |
            //   ------C------------------
            //   |  X  |     |           |
            //   |-----|-----|           |
            //   |     |     |           |
            //   -------------------------
            //   we don't want to back out to the parent cell of X, because
            //   the corner C isn't contained within that parent.
            //
            //   Each cell contains one vertex of the parent's corner cell,
            //   indicated by its parent_index variable.
            for (auto t = task; i.isCorner() && t->parent &&
                                t->target->parent_index == i.pos();
                 t = t->parent)
            {
                t->parent->neighbors.map(i, f);
            }
        }
    }
}

}   // namespace Kernel

/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2019  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/

#include "libfive/eval/evaluator.hpp"
#include "libfive/eval/tape.hpp"

#include "libfive/render/axes.hpp"
#include "libfive/render/brep/hybrid/hybrid_tree.hpp"
#include "libfive/render/brep/hybrid/hybrid_neighbors.hpp"
#include "libfive/render/brep/region.hpp"
#include "libfive/render/brep/settings.hpp"
#include "libfive/render/brep/neighbor_tables.hpp"
#include "libfive/render/brep/edge_tables.hpp"
#include "libfive/render/brep/manifold_tables.hpp"

#include "../xtree.inl"

//#define DEBUG(s) std::cout << s << "\n"
#define DEBUG(s) {}

namespace libfive {

/*
 *  Helper function to pack from a higher dimension to a lower one
 */
template <unsigned BaseDimension, unsigned Target>
Eigen::Matrix<double, NeighborIndex::dimension(Target), 1> pack(
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
        const Eigen::Matrix<double, NeighborIndex::dimension(Target), 1>& in,
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
        Evaluator* eval, const Tape::Handle& tape,
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
                eval->set<N>(ps.col(j), region, j);
        }

        Eigen::Array<float, 1, ArrayEvaluator::N> out;
        out.leftCols(POINTS_PER_SEARCH) = eval->values(
                POINTS_PER_SEARCH, *tape);

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
                (out[j] == 0 && !eval->isInside<N>(
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
    return t;
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
    std::fill(vertex_on_surface.begin(), vertex_on_surface.end(), false);
    std::fill(index.begin(), index.end(), 0);
    std::fill(surface_rank.begin(), surface_rank.end(), 0);

    surface.clear();
    tape.reset();
    for (auto& q : qef) {
        q.reset();
    }
    surface_mass_point.array() = 0.0;
    vertex_pos.array() = 0.0;
}

template <unsigned N>
Tape::Handle HybridTree<N>::evalInterval(Evaluator* eval,
                                         const Tape::Handle& tape,
                                         Pool& object_pool)
{
    // Do a preliminary evaluation to prune the tree, storing the interval
    // result and an handle to the pushed tape (which we'll use when recursing)
    auto o = eval->intervalAndPush(
            this->region.lower3().template cast<float>(),
            this->region.upper3().template cast<float>(),
            tape);

    this->type = o.first.state();
    if (!o.first.isSafe())
    {
        assert(this->type == Interval::AMBIGUOUS);
        return tape;
    }

    if (this->type == Interval::FILLED || this->type == Interval::EMPTY)
    {
        buildLeaf(eval, tape, object_pool);
        this->done();
    }
    return o.second;
}


template <unsigned N>
void HybridTree<N>::buildLeaf(Evaluator* eval,
                              const std::shared_ptr<Tape>& tape,
                              Pool& object_pool)
{
    assert(this->leaf == nullptr);
    this->leaf = object_pool.next().get();
    this->leaf->tape = tape;

    processCorners(eval, tape);
    processSubspaces<1>(eval, tape);
    processSubspaces<2>(eval, tape);
    if (N == 3) {
        processSubspaces<3>(eval, tape);
    }
}

template <unsigned N>
void HybridTree<N>::processCorners(Evaluator* eval,
                                   const Tape::Handle& tape)
{
    for (unsigned i=0; i < ipow(2, N); ++i) {
        const NeighborIndex n = CornerIndex(i).neighbor();
        const auto corner = this->region.corner(i);
        placeDistanceVertex(eval, tape, n, corner);

        DEBUG("Corner " << n.i );
        DEBUG("  placed at " << corner.transpose());
        DEBUG("  inside: " << this->leaf->inside[n.i]);
    }
}

template <unsigned BaseDimension, unsigned Target>
void processEdge(HybridTree<BaseDimension>* tree,
                 Evaluator* eval,
                 const Tape::Handle& tape,
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
    for (const auto& j : EdgeTables<BaseDimension>::subspaces(edge)) {
        if (tree->leaf->inside[j.i]) {
            has_inside = true;
            inside = tree->leaf->vertex_pos.col(j.i);
        } else {
            has_outside = true;
            outside = tree->leaf->vertex_pos.col(j.i);
        }
    }

    if (has_inside && has_outside) {
        // If there's a sign change, then do a binary search to
        // find the exact point of intersection, marking the resulting
        // point as a surface feature.
        tree->leaf->has_surface_qef[edge.i] = true;
        tree->leaf->vertex_on_surface[edge.i] = true;
        tree->leaf->surface_mass_point.col(edge.i).array() = 0.0;
        auto surf = searchBetween<BaseDimension>(eval, tape, region,
                                                 inside, outside);

        // Unpack from inside-outside into mass point, and prepare to call
        // accumulate() to store QEF data for a normalized surface QEF.
        Eigen::Array<double, BaseDimension, ArrayEvaluator::N> positions;
        for (unsigned i=0; i < 2; ++i) {
            MassPoint<BaseDimension> mp;
            mp << surf.col(i), 1;
            tree->leaf->surface_mass_point.col(edge.i) += mp;
            positions.col(i) = surf.col(i);
        }
        std::array<NeighborIndex, 2> targets = {edge, edge};
        tree->accumulate(eval, positions, tape, 2, targets.data(), true);

        // Calculate the rank of the edge intersection.  This is probably
        // 1 (if the cell edge intersects a surface) or 2 (if the cell edge
        // happens to catch a sharp edge on the surface).
        tree->leaf->surface_rank[edge.i] = tree->leaf->qef[edge.i].rankDC();

        // TODO: weigh this based on distance
        Eigen::Matrix<double, BaseDimension, 1> pos;
        pos = (surf.col(0) + surf.col(1)) / 2;
        tree->leaf->vertex_pos.col(edge.i) = pos;

        // TODO: use a less powerful evaluator unless needed
        tree->leaf->inside[edge.i] = eval->isInside<BaseDimension>(
                pos, region, tape);

        DEBUG("Found surface edge " << edge.i);
        DEBUG("  placed at " << pos.transpose());
        DEBUG("  inside: " << tree->leaf->inside[edge.i]);
        DEBUG("  rank: " << tree->leaf->surface_rank[edge.i]);
    } else {
        // If the edge doesn't have an obvious sign change, then place a
        // vertex on a sharp feature of the distance field (which will
        // hopefully be a sign change, if a sign change exists along this edge).
        QEF<BaseDimension> qef;

        // This should hopefully only be the two corners along the edge.
        for (const auto& n: EdgeTables<BaseDimension>::subspaces(edge)) {
            assert(n.dimension() == 0);
            qef += tree->leaf->qef[n.i];
        }
        const auto region_ = region.template subspace<TargetFloating>();
        QEF<TargetDimension> qef_ = qef.template sub<TargetFloating>();

        // Bounded, minimizing towards the center of the region
        // (which is the center of the edge in this case)
        auto sol = qef_.solveBounded(region_, 1);

        // Unpack from the reduced-dimension solution to the leaf vertex
        auto pos = unpack<BaseDimension, Target>(sol.position, region);

        // placeDistanceVertex will check if there's a sign change and store
        // surface QEF data if that's the case.
        tree->placeDistanceVertex(eval, tape, edge, pos);

        DEBUG("Solved distance edge " << edge.i << " with error " << sol.error);
        DEBUG("  placed at " << pos.transpose());
        DEBUG("  inside: " << tree->leaf->inside[edge.i]);
    }
}

template <unsigned BaseDimension, int Target>
void process(HybridTree<BaseDimension>* tree,
             Evaluator* eval,
             const Tape::Handle& tape,
             const Region<BaseDimension>& region)
{
    static_assert(Target >= 0, "Invalid Target subspace");
    constexpr NeighborIndex n(Target);
    constexpr auto TargetDimension = n.dimension();
    constexpr auto TargetFloating = n.floating();

    DEBUG("Solving for subspace " << Target);

    // Now, we'll check to see whether we have any intersections
    // on a child edge subspace, to decide whether to place the vertex
    // on a sharp feature of the surface or of the distance field.
    QEF<BaseDimension> qef_surface, qef_distance;
    MassPoint<BaseDimension> mass_point_surface;
    mass_point_surface.array() = 0.0;

    uint32_t filled_mask = 0;
    unsigned filled_mask_bit = 0;
    unsigned max_surface_rank = 0;
    for (const auto& m : EdgeTables<BaseDimension>::subspaces(n)) {
        // Build up a bitmask of occupied perimeter subspaces, so that
        // we can check for manifoldness later.
        if (tree->leaf->inside[m.i]) {
            filled_mask |= (1 << filled_mask_bit);
        }
        filled_mask_bit++;

        // For the DC-style (surface) QEF accumulation, only select QEFs
        // that have the "has surface qef" flag set.  This prevents
        // double-counting.
        if (tree->leaf->has_surface_qef[m.i]) {
            qef_surface += tree->leaf->qef[m.i];
        }

        // We're going to accumulate the mass point based on all of
        // the max-rank on-surface vertices (or surface QEF mass points).
        if (tree->leaf->has_surface_qef[m.i] ||
            tree->leaf->vertex_on_surface[m.i])
        {
            max_surface_rank = std::max(max_surface_rank,
                                        tree->leaf->surface_rank[m.i]);
        }

        // Similarly, for distance-field vertices, we don't care about
        // any data other than corners, because that's the only source
        // of higher-dimension spaces QEF info.
        if (m.dimension() == 0) {
            qef_distance += tree->leaf->qef[m.i];
        }
    }

    // If that failed, run the solver to position the vertex on a sharp feature
    // of the distance field itself.  This is more robust, but worse at
    // positioning the final model vertices on corners and edges.
    const auto region_ = region.template subspace<TargetFloating>();

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

    // Accumulate max-rank intersections into our local mass point.
    // We only want max-rank intersections, because that improves the
    // final vertex position (see the Dual Contouring Secret Sauce paper
    // for details).
    for (const auto& m : EdgeTables<BaseDimension>::subspaces(n)) {
        if (tree->leaf->surface_rank[m.i] == max_surface_rank) {
            if (tree->leaf->vertex_on_surface[m.i]) {
                Eigen::Matrix<double, BaseDimension + 1, 1> v;
                v << tree->leaf->vertex_pos.col(m.i), 1.0;
                mass_point_surface += v;
            } else if (tree->leaf->has_surface_qef[m.i]) {
                mass_point_surface += tree->leaf->surface_mass_point.col(m.i);
            }
        }
    }

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

        // This is a tricky special case which helps prevent DC vertices
        // from escaping their cells: if an edge vertex is outside of the
        // cell, then we check whether we can slide along its one degree
        // of freedom to put it back inside.
        if (!within_region && TargetDimension == BaseDimension &&
            sol_surf.rank == TargetDimension - 1)
        {
            DEBUG("  DC vertex is out-of-region with one degree of freedom");
            DEBUG("    Attempting to slide it into the region");
            const auto slide_direction = qef_surface.slideDC();
            DEBUG("    Sliding along [" << slide_direction.transpose() << "]");
            bool found = false;
            auto inter = region.intersection(v_surf, slide_direction, &found);
            if (found) {
                DEBUG("    Found intersection");
                DEBUG("      [" << inter.col(0).transpose() << "]");
                DEBUG("      [" << inter.col(1).transpose() << "]");

                v_surf = (inter.col(0) + inter.col(1)) / 2;
                if (region.contains(v_surf, 0)) {
                    DEBUG("    Slid to [" << v_surf.transpose() << "]");
                    within_region = true;
                } else {
                    DEBUG("    Sliding left us outside the region at ["
                           << v_surf.transpose() << "]");
                }
            }
        }

        // If we successfully placed the vertex using Dual Contouring
        // rules, then mark that the resulting vertex is a surface vertex.
        if (within_region && (sol_surf.error <= 1e-12
                    || sol_surf.error / 10.0 < sol_dist.error
                    || (v_dist - v_surf).norm() < 1e-12))
        {
            DEBUG("      Placed DC vertex");
            tree->leaf->has_surface_qef[n.i] = false;
            tree->leaf->vertex_on_surface[n.i] = true;
            tree->leaf->vertex_pos.col(n.i) = v_surf;
            return;
        }
    }

    NeighborIndex constrained_to = n;
    for (const auto& m : EdgeTables<BaseDimension>::subspaces(n)) {
        bool constrained = true;
        for (unsigned i=0; i < BaseDimension; ++i) {
            if ((n.floating() & (1 << i)) && (m.fixed() & (1 << i))) {
                constrained &= (v_dist[i] == tree->leaf->vertex_pos(i, m.i));
            }
        }
        if (constrained && m.dimension() < constrained_to.dimension()) {
            constrained_to = m;
        }
    }
    if (constrained_to.i != n.i && 0) {
        DEBUG("    Placing distance vertex constrained to subspace "
              << constrained_to.i);
#define ASSIGN(p) tree->leaf->p[n.i] = tree->leaf->p[constrained_to.i]
        ASSIGN(inside);
        ASSIGN(has_surface_qef);
        ASSIGN(qef);
        ASSIGN(vertex_on_surface);
        ASSIGN(surface_rank);
#undef ASSIGN
        tree->leaf->vertex_pos.col(n.i) =
            tree->leaf->vertex_pos.col(constrained_to.i);
        tree->leaf->surface_mass_point.col(n.i) =
            tree->leaf->surface_mass_point.col(constrained_to.i);
    } else {
        //  If we failed to place a DC vertex, then we're placing a distance
        //  vertex instead.
        DEBUG("      Placing distance vertex");
        tree->placeDistanceVertex(eval, tape, n, v_dist);
    }
}


template <unsigned BaseDimension, unsigned TargetDimension, int Target>
struct Unroller {
    void run(HybridTree<BaseDimension>* tree,
             Evaluator* eval,
             const Tape::Handle& tape,
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
                 Evaluator*,
                 const Tape::Handle&,
                 const Region<BaseDimension>&)
    {
        // Terminate static unrolling here
    }
};


template <unsigned N>
template<unsigned D>
void HybridTree<N>::processSubspaces(Evaluator* eval,
                                     const Tape::Handle& tape)
{
    Unroller<N, D, ipow(3, N) - 1>().run(this, eval, tape, this->region);
}

template <unsigned N>
void HybridTree<N>::placeDistanceVertex(
        Evaluator* eval, const Tape::Handle& tape,
        NeighborIndex n, const Vec& pos)
{
    // Store the vertex position
    this->leaf->vertex_pos.col(n.i) = pos;

    // Expensive check for inside / outsideness of this point (TODO)
    this->leaf->inside[n.i] = eval->isInside<N>(pos, this->region, tape);
    this->leaf->surface_mass_point.col(n.i).array() = 0.0;

    // For now, we don't care about accumulating QEFs or surface intersections
    // for any subspace vertex of the maximum dimension.  This is because
    // there's only one such vertex per cell, it's the cell's body vertex,
    // and it isn't used to find any other cell positions.
    //
    // We may want to change this behavior once we start thinking about
    // cell collapsing.
    if (n.dimension() == N) {
        return;
    }

    // Check every edge from the new point to its neighbouring subspaces,
    // seeing whether there's a sign change and searching the edge if that's
    // the case.
    unsigned num_intersections = 0;
    Eigen::Matrix<double, N, ArrayEvaluator::N> intersections;
    for (auto& t: EdgeTables<N>::subspaces(n.i)) {
        // If there isn't a sign change along this subspace-to-subspace
        // edge, then we shouldn't search it.
        assert(t.dimension() < n.dimension());
        if (this->leaf->inside[t.i] == this->leaf->inside[n.i]) {
            continue;
        }

        Vec inside, outside;
        if (this->leaf->inside[t.i]) {
            inside = this->leaf->vertex_pos.col(t.i);
            outside = this->leaf->vertex_pos.col(n.i);
        } else {
            inside = this->leaf->vertex_pos.col(n.i);
            outside = this->leaf->vertex_pos.col(t.i);
        }
        auto p = searchBetween<N>(eval, tape, this->region, inside, outside);

        for (unsigned i=0; i < 2; ++i) {
            MassPoint<N> mp;
            mp << p.col(i), 1;
            this->leaf->surface_mass_point.col(n.i) += mp;

            // Store the intersection point into the QEF target list
            intersections.col(num_intersections++) = p.col(i);
        }
    }
    assert(num_intersections <= intersections.cols());

    if (num_intersections) {
        assert(n.dimension() > 0);

        // This is a bit silly, because every intersection ends up accumulated
        // into the same QEF, but it makes accumulate() more flexible
        std::array<NeighborIndex, ipow(3, N) * 2> targets;
        std::fill(targets.begin(), targets.end(), n);
        assert(targets.size() >= num_intersections);

        accumulate(eval, intersections, tape, num_intersections,
                   targets.data(), true);
        this->leaf->has_surface_qef[n.i] = true;
        this->leaf->surface_rank[n.i] = this->leaf->qef[n.i].rankDC();
    } else {
        // If we didn't find a sign change, then store the vertex itself into
        // the subspace QEF.
        intersections.col(0) = pos;
        accumulate(eval, intersections, tape, 1, &n, false);
        this->leaf->has_surface_qef[n.i] = false;
    }
}

template <unsigned N>
void HybridTree<N>::accumulate(
        Evaluator* eval,
        const Eigen::Array<double, N, ArrayEvaluator::N>& positions,
        const Tape::Handle& tape,
        unsigned count,
        NeighborIndex* target,
        bool normalize)
{
    // Unpack into the data array
    for (unsigned i=0; i < count; ++i) {
        eval->set<N>(positions.col(i), this->region, i);
    }
    Eigen::Array<float, 4, ArrayEvaluator::N> ds;
    ds.leftCols(count) = eval->derivs(count);
    auto ambig = eval->getAmbiguous(count, *tape);

    auto push = [&ds, &positions, &target, &normalize, this]
                (Eigen::Vector3f d, unsigned i)
    {
        double value = ds(3, i);
        if (normalize) {
            double norm = d.norm();
            d /= norm;
            value /= norm;
        }
        this->leaf->qef[target[i].i].insert(
                positions.col(i),
                d.template cast<double>().template head<N>(),
                value);
    };

    for (unsigned i=0; i < count; ++i) {
        if (!ambig[i]) {
            push(ds.col(i).head<3>(), i);
        } else {
            const auto fs = eval->features<N>(
                    positions.col(i), this->region, tape);
            for (const auto& f : fs) {
                push(f, i);
            }
        }
    }
}

template <unsigned N>
void HybridTree<N>::evalLeaf(Evaluator* eval,
                             const Tape::Handle& tape,
                             Pool& object_pool,
                             const HybridNeighbors<N>& neighbors)
{
    (void)neighbors;

    buildLeaf(eval, tape, object_pool);

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
bool HybridTree<N>::collectChildren(Evaluator* eval,
                                    const Tape::Handle& tape,
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
        buildLeaf(eval, tape, object_pool);
        this->done();
        return true;
    }

    // Eventually, we'll use these variables to perhaps collapse the tree
    (void)eval;
    (void)tape;
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

}   // namespace libfive

/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <future>
#include <numeric>
#include <functional>
#include <limits>

#include <cmath>

#include <Eigen/StdVector>
#include <boost/lockfree/queue.hpp>

#include "libfive/render/brep/dc/dc_tree.hpp"
#include "libfive/render/brep/object_pool.hpp"
#include "libfive/render/brep/region.hpp"
#include "libfive/render/axes.hpp"
#include "libfive/eval/tape.hpp"

#include "../xtree.cpp"

namespace Kernel {

//  Here's our cutoff value (with a value set in the header)
template <unsigned N> constexpr double DCTree<N>::EIGENVALUE_CUTOFF;

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
DCTree<N>::DCTree(DCTree<N>* parent, unsigned index, Region<N> r)
    : XTree<N, DCTree<N>, DCLeaf<N>>(parent, index, r)
{
    // Nothing to do here
}

template <unsigned N>
DCTree<N>::DCTree()
    : XTree<N, DCTree<N>, DCLeaf<N>>()
{
    // Nothing to do here
}

template <unsigned N>
std::unique_ptr<DCTree<N>> DCTree<N>::empty()
{
    std::unique_ptr<DCTree> t(new DCTree);
    t->type = Interval::EMPTY;
    return std::move(t);
}

template <unsigned N>
DCLeaf<N>::DCLeaf()
{
    reset();
}

template <unsigned N>
void DCLeaf<N>::reset()
{
    level = 0;
    rank = 0;
    corner_mask = 0;
    vertex_count = 0;
    manifold = false;

    std::fill(index.begin(), index.end(), 0);

    for (auto& i : intersections)
    {
        i.reset();
    }

    verts.setZero();
    mass_point.setZero();
    AtA.setZero();
    AtB.setZero();
    BtB = 0;
}

template <unsigned N>
Tape::Handle DCTree<N>::evalInterval(XTreeEvaluator* eval,
                                     Tape::Handle tape,
                                     const Region<N>& region,
                                     Pool&)
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
        this->done();
    }
    return o.second;
}

template <unsigned N>
void DCTree<N>::evalLeaf(XTreeEvaluator* eval,
                        Tape::Handle tape,
                        const Region<N>& region,
                        Pool& object_pool,
                        const DCNeighbors<N>& neighbors)
{
    // Track how many corners have to be evaluated here
    // (if they can be looked up from a neighbor, they don't have
    //  to be evaluated here, which can save time)
    size_t count = 0;

    // Remap from a value in the range [0, count) to a corner index
    // in the range [0, 1 <<N).
    std::array<int, 1 << N> corner_indices;

    // Local array of corners
    std::array<Interval::State, 1 << N> corners;

    // Pack corners into evaluator
    Eigen::Matrix<float, 3, 1 << N> pos;
    for (uint8_t i=0; i < this->children.size(); ++i)
    {
        auto c = neighbors.check(i);
        if (c == Interval::UNKNOWN)
        {
            pos.col(count) = region.corner3f(i);
            eval->array.set(pos.col(count), count);
            corner_indices[count++] = i;
        }
        else
        {
            corners[i] = c;
        }
    }

    // Evaluate the region's corners and check their states
    // We handle evaluation in three phases:
    // 1)  Evaluate the distance field at corners, mark < 0 or > 0
    //     as filled or empty.
    // 2)  For values that are == 0 but not ambiguous (i.e. do not
    //     have a min / max where both branches are possible),
    //     evaluate the derivatives and mark the corner as filled if
    //     there are non-zero derivatives (because that means that we
    //     can find an inside-outside transition).
    // 3)  For values that are == 0 and ambiguous, call isInside
    //     (the heavy hitter of inside-outside checking).
    auto vs = eval->array.values(count, tape);

    // We store ambiguity here, but clear it if the point is inside
    // or outside (so after the loop below, ambig(i) is only set if
    // pos[i] is both == 0 and ambiguous).
    auto ambig = eval->array.getAmbiguous(count, tape);

    // This is a count of how many points there are that == 0
    // but are unambiguous; unambig_remap[z] returns the index
    // into the corner_indices array for a particular unambiguous zero.
    uint8_t unambiguous_zeros = 0;
    std::array<int, 1 << N> unambig_remap;

    // This is phase 1, as described above
    for (uint8_t i=0; i < count; ++i)
    {
        // The Eigen evaluator occasionally disagrees with the
        // deriv (single-point) evaluator, because it has SSE
        // implementations of transcendental functions that can
        // return subtly different results.  If we get a result that
        // is sufficiently close to zero, then fall back to the
        // canonical single-point evaluator to avoid inconsistency.
        if (fabs(vs(i)) < 1e-6)
        {
            vs(i) = eval->deriv.eval(pos.col(i), tape);
        }

        // Handle inside, outside, and (non-ambiguous) on-boundary
        if (vs(i) > 0 || std::isnan(vs(i)))
        {
            corners[corner_indices[i]] = Interval::EMPTY;
            ambig(i) = false;
        }
        else if (vs(i) < 0)
        {
            corners[corner_indices[i]] = Interval::FILLED;
            ambig(i) = false;
        }
        else if (!ambig(i))
        {
            unambig_remap[unambiguous_zeros] = i;
            unambiguous_zeros++;
        }
    }

    // Phase 2: Optimization for non-ambiguous features
    // We can get both positive and negative values out if
    // there's a non-zero gradient. Once again, we need to use
    // single-point evaluation if it's sufficiently close to zero.
    if (unambiguous_zeros)
    {
        for (unsigned i = 0; i < unambiguous_zeros; ++i)
        {
            eval->array.set(pos.col(unambig_remap[i]), i);
        }
        auto ds = eval->array.derivs(unambiguous_zeros, tape);
        for (unsigned i=0; i < unambiguous_zeros; ++i)
        {
            if ((ds.col(i).head<3>().abs() < 1e-6f).any())
            {
                ds.col(i) = eval->deriv.deriv(pos.col(unambig_remap[i]));
                corners[corner_indices[unambig_remap[i]]] =
                    (ds.col(i).template head<3>() != 0).any()
                    ? Interval::FILLED : Interval::EMPTY;
            }
            else
            {
                corners[corner_indices[unambig_remap[i]]] = Interval::FILLED;
            }
        }
    }

    // Phase 3: One last pass for handling ambiguous corners
    for (uint8_t i=0; i < count; ++i)
    {
        if (ambig(i))
        {
            corners[corner_indices[i]] =
                eval->feature.isInside(pos.col(i), tape)
                    ? Interval::FILLED
                    : Interval::EMPTY;
        }
    }

    bool all_full = true;
    bool all_empty = true;

    // Pack corners into filled / empty arrays
    for (uint8_t i=0; i < this->children.size(); ++i)
    {
        all_full  &= (corners[i] == Interval::FILLED);
        all_empty &= (corners[i] == Interval::EMPTY);
    }

    this->type = all_empty ? Interval::EMPTY
               : all_full  ? Interval::FILLED : Interval::AMBIGUOUS;

    // Early exit if this leaf is unambiguous
    if (this->type != Interval::AMBIGUOUS)
    {
        this->done();
        return;
    }

    assert(this->leaf == nullptr);
    this->leaf = object_pool.next().get();
    this->leaf->corner_mask = buildCornerMask(corners);

    // Now, for the fun part of actually placing vertices!
    // Figure out if the leaf is manifold
    this->leaf->manifold = cornersAreManifold(this->leaf->corner_mask);

    // We'll use this vector anytime we need to pass something
    // into the evaluator (which requires a Vector3f)
    Eigen::Vector3f _pos;
    _pos.template tail<3 - N>() = region.perp.template cast<float>();
    auto set = [&](const Vec& v, size_t i){
        _pos.template head<N>() = v.template cast<float>();
        eval->array.set(_pos, i);
    };

    // Iterate over manifold patches, storing one vertex per patch
    const auto& ps = MarchingTable<N>::mt.v[this->leaf->corner_mask];
    while (this->leaf->vertex_count < ps.size() &&
           ps[this->leaf->vertex_count][0].first != -1)
    {
        // Number of edges, total
        unsigned edge_count;

        // Edge indices (as found with mt->e[a][b]) for all edges,
        // with edge_count valid entries.
        std::array<size_t, _edges(N)> edges;

        {   // Within this block, we calculate all edges that haven't
            // already been calculated by neighbors and store them in
            // the appropriate slot of the intersections array.

            // Numbers of edges that need evaluation
            unsigned eval_count;

            // Inside-outside pairs, with eval_count valid pairs
            std::array<std::pair<Vec, Vec>, _edges(N)> targets;

            // Edge indices (as found with mt->e[a][b]) for edges under
            // evaluation, with eval_count valid values.
            std::array<size_t, _edges(N)> eval_edges;

            // Iterate over edges in this patch, storing [inside, outside]
            // in the targets array if the list of intersections can't be
            // re-used from a neighbor.
            for (edge_count=0, eval_count=0;
                 edge_count < ps[this->leaf->vertex_count].size() &&
                     ps[this->leaf->vertex_count][edge_count].first != -1;
                 ++edge_count)
            {
                // Sanity-checking
                assert(corners[ps[this->leaf->vertex_count][edge_count].first]
                       == Interval::FILLED);
                assert(corners[ps[this->leaf->vertex_count][edge_count].second]
                       == Interval::EMPTY);

                // Store the edge index associated with this target
                auto c = ps[this->leaf->vertex_count][edge_count];
                edges[edge_count] = MarchingTable<N>::mt.e[c.first][c.second];

                auto compare = neighbors.check(c.first, c.second);
                // Enable this to turn on sharing of results with neighbors
                if (compare.get() != nullptr)
                {
                    assert(compare->size() > 0);
                    this->leaf->intersections[edges[edge_count]] = compare;
                }
                else
                {
                    // Store inside / outside in targets array, and the edge
                    // index in the eval_edges array.
                    targets[eval_count] = {region.corner(c.first),
                                           region.corner(c.second)};
                    eval_edges[eval_count] = edges[edge_count];

                    assert(eval_edges[eval_count] < this->leaf->intersections.size());
                    eval_count++;
                }

                assert(edges[edge_count] < this->leaf->intersections.size());
            }

            // Next, we search over the target edges, doing an
            // N-fold reduction at each stage to home in on the
            // exact intersection position
            constexpr int SEARCH_COUNT = 4;
            constexpr int POINTS_PER_SEARCH = 16;
            static_assert(
                    _edges(N) * POINTS_PER_SEARCH <= ArrayEvaluator::N,
                    "Potential overflow");

            // Multi-stage binary search for intersection
            for (int s=0; s < SEARCH_COUNT; ++s)
            {
                // Load search points into evaluator
                Eigen::Array<double, N, POINTS_PER_SEARCH * _edges(N)> ps;
                for (unsigned e=0; e < eval_count; ++e)
                {
                    for (int j=0; j < POINTS_PER_SEARCH; ++j)
                    {
                        const double frac = j / (POINTS_PER_SEARCH - 1.0);
                        const unsigned i = j + e*POINTS_PER_SEARCH;
                        ps.col(i) = (targets[e].first * (1 - frac)) +
                                    (targets[e].second * frac);
                        set(ps.col(i), i);
                    }
                }

                // Evaluate, then search for the first outside point
                // and adjust inside / outside to their new positions
                if (eval_count)
                {
                    auto out = eval->array.values(
                            POINTS_PER_SEARCH * eval_count, tape);

                    for (unsigned e=0; e < eval_count; ++e)
                    {
                        // Skip one point, because the very first point is
                        // already known to be inside the shape (but
                        // sometimes, due to numerical issues, it registers
                        // as outside!)
                        for (unsigned j=1; j < POINTS_PER_SEARCH; ++j)
                        {
                            const unsigned i = j + e*POINTS_PER_SEARCH;
                            if (out[i] > 0)
                            {
                                assert(i > 0);
                                targets[e] = {ps.col(i - 1), ps.col(i)};
                                break;
                            }
                            else if (out[i] == 0)
                            {
                                Eigen::Vector3d pos;
                                pos << ps.col(i), region.perp;
                                if (!eval->feature.isInside(
                                            pos.template cast<float>(), tape))
                                {
                                    assert(i > 0);
                                    targets[e] = {ps.col(i - 1), ps.col(i)};
                                    break;
                                }
                            }
                            // Special-case for final point in the search,
                            // working around numerical issues where
                            // different evaluators disagree with whether
                            // points are inside or outside.
                            else if (j == POINTS_PER_SEARCH - 1)
                            {
                                targets[e] = {ps.col(i - 1), ps.col(i)};
                                break;
                            }
                        }
                    }
                }
            }

            // Now, we evaluate the distance field (value + derivatives) at
            // each intersection (which is associated with a specific edge).
            static_assert(_edges(N) * 2 <= ArrayEvaluator::N,
                          "Too many results");
            if (eval_count)
            {
                for (unsigned i=0; i < eval_count; ++i)
                {
                    set(targets[i].first, 2*i);
                    set(targets[i].second, 2*i + 1);
                }
                auto ds = eval->array.derivs(2 * eval_count, tape);
                auto ambig = eval->array.getAmbiguous(2 * eval_count, tape);

                // Iterate over all inside-outside pairs, storing the number
                // of intersections before each inside node (in prev_size),
                // then checking the rank of the pair after each outside
                // node based on the accumulated intersections.
                for (unsigned i=0; i < 2 * eval_count; ++i)
                {
                    // This is the position associated with the intersection
                    // being investigated.
                    Eigen::Vector3d pos;
                    pos << ((i & 1) ? targets[i/2].second
                                    : targets[i/2].first),
                           region.perp;

                    // If this position is unambiguous, then we can use the
                    // derivatives value calculated and stored in ds.
                    if (!ambig(i))
                    {
                        saveIntersection(pos.template head<N>(),
                                         ds.col(i).template cast<double>()
                                                  .template head<N>(),
                                         ds.col(i).w(), eval_edges[i/2]);
                    }
                    // Otherwise, we need to use the feature-finding special
                    // case to find all possible derivatives at this point.
                    else
                    {
                        const auto fs = eval->feature.features(
                                pos.template cast<float>(), tape);

                        for (auto& f : fs)
                        {
                            saveIntersection(pos.template head<N>(),
                                             f.template head<N>()
                                              .template cast<double>(),
                                             ds.col(i).w(), eval_edges[i/2]);
                        }
                    }
                }
            }
        }
        // At this point, every [intersections[e] for e in edges] should be
        // populated with a list of Intersection objects, whether taken
        // from a neighbor or calculated in the code above.

        // Each edge, which contains one or more intersections, is assigned
        // a rank based on the normals of those intersections.
        std::array<int, _edges(N)> edge_ranks;
        std::fill(edge_ranks.begin(), edge_ranks.end(), -1);
        for (unsigned i=0; i < edge_count; ++i)
        {
            // If every intersection was NaN (?!), use rank 0;
            // otherwise, figure out how many normals diverge
            IntersectionVec<N> prev_normals;
            edge_ranks[i] = 0;

            if (this->leaf->intersections[edges[i]].get())
            {
                for (const auto& t : *this->leaf->intersections[edges[i]])
                {
                    if (t.deriv != Vec::Zero())
                    {
                        bool matched = false;
                        for (auto& v : prev_normals)
                        {
                            matched |= (t.deriv.dot(v.deriv) >= 0.9);
                        }
                        if (!matched)
                        {
                            edge_ranks[i]++;
                            prev_normals.push_back(t);
                        }
                    }
                }
            }
        }

        // Reset the mass point, since we may have used it for the previous
        // vertex.
        this->leaf->mass_point = this->leaf->mass_point.Zero();

        {   // Build the mass point from max-rank intersections
            const int max_rank = *std::max_element(
                    edge_ranks.begin(), edge_ranks.end());
            for (unsigned i=0; i < edge_count; ++i)
            {
                assert(edge_ranks[i] != -1);
                if (edge_ranks[i] == max_rank)
                {
                    // Accumulate this intersection in the mass point
                    // by storing the first and last intersection position
                    // (which are guaranteed by construction to be a
                    // just-inside and just-outside position, respectively)
                    Eigen::Matrix<double, N + 1, 1> mp;
                    const auto& ns = this->leaf->intersections[edges[i]];
                    if (ns.get() && ns->size() > 1)
                    {
                        mp << (*ns).front().pos, 1;
                        this->leaf->mass_point += mp;
                        mp << (*ns).back().pos, 1;
                        this->leaf->mass_point += mp;
                    }
                }
            }
        }

        // Count how many intersections are stored, across all of the
        // relevant edges for this vertex.  We use this data to determine
        // the size of the arrays for QEF solving.
        size_t rows = 0;
        for (unsigned i=0; i < edge_count; ++i)
        {
            for (const auto& n : *this->leaf->intersections[edges[i]])
            {
                if (n.deriv != Vec::Zero())
                {
                    rows++;
                }
            }
        }

        // Now, we'll (pretend to) unpack into A and b matrices,
        // then immediately calculate AtA, AtB, and BtB
        //
        //  The A matrix is of the form
        //  [n1x, n1y, n1z]
        //  [n2x, n2y, n2z]
        //  [n3x, n3y, n3z]
        //  ...
        //  (with one row for each sampled point's normal)
        //
        //  The b matrix is of the form
        //  [p1 . n1]
        //  [p2 . n2]
        //  [p3 . n3]
        //  ...
        //  (with one row for each sampled point)
        //
        // Since we're deliberately sampling on either side of the
        // intersection, we subtract out the distance-field value
        // to make the math work out.
        //
        // Instead of actually populating these matrices, we'll immediately
        // construct the compact results AtA, AtB, BtB
        this->leaf->AtA.array() = 0;
        this->leaf->AtB.array() = 0;
        this->leaf->BtB = 0;
        for (unsigned i=0; i < edge_count; ++i)
        {
            if (this->leaf->intersections[edges[i]])
            {
                for (auto& n : *this->leaf->intersections[edges[i]])
                {
                    if (n.deriv != Vec::Zero())
                    {
                        this->leaf->AtA += n.deriv * n.deriv.transpose();
                        const double b = n.deriv.dot(n.pos) - n.value;
                        this->leaf->AtB += n.deriv * b;
                        this->leaf->BtB += b * b;
                    }
                }
            }
        }

        // Find the vertex position, storing into the appropriate column
        // of the vertex array and ignoring the error result (because
        // this is the bottom of the recursion)
        findVertex(this->leaf->vertex_count);

        // Move on to the next vertex
        this->leaf->vertex_count++;
    }
    this->done();
}

template <unsigned N>
void DCTree<N>::saveIntersection(const Vec& pos, const Vec& derivs,
                                const double value, const size_t edge)
{
    const double norm = derivs.matrix().norm();

    // Find normalized derivatives and distance value
    Eigen::Matrix<double, N, 1> dv = derivs / norm;


    // Just-in-time allocation of intersections array
    if (this->leaf->intersections[edge] == nullptr)
    {
        this->leaf->intersections[edge].reset(
                new IntersectionVec<N>);
    }

    // If the point has a valid normal, then store it
    if (dv.array().isFinite().all())
    {
        this->leaf->intersections[edge]->
             push_back({pos, dv, value / norm, 0});
    }
    // Otherwise, store an intersection with a zero normal
    else
    {
        this->leaf->intersections[edge]->
             push_back({pos, Vec::Zero(), 0, 0});
    }
}

template <unsigned N>
uint8_t DCTree<N>::buildCornerMask(
        const std::array<Interval::State, 1 << N>& corners)
{
    uint8_t corner_mask = 0;
    for (unsigned i=0; i < (1 << N); ++i)
    {
        assert(corners[i] != Interval::UNKNOWN);
        corner_mask |= (corners[i] == Interval::FILLED) << i;
    }
    return corner_mask;
}

template <unsigned N>
bool DCTree<N>::collectChildren(XTreeEvaluator* eval,
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
    std::array<DCTree<N>*, 1 << N> cs;
    for (unsigned i=0; i < this->children.size(); ++i)
    {
        cs[i] = this->children[i].load(std::memory_order_relaxed);
    }

    // If any children are branches, then we can't collapse.
    // We do this check first, to avoid allocating then freeing a Leaf
    if (std::any_of(cs.begin(), cs.end(),
                    [](DCTree<N>* o){ return o->isBranch(); }))
    {
        this->done();
        return true;
    }

    // Update corner and filled / empty state from children
    bool all_empty = true;
    bool all_full  = true;
    std::array<Interval::State, 1 << N> corners;
    for (uint8_t i=0; i < cs.size(); ++i)
    {
        auto c = cs[i];
        assert(c != nullptr);

        // Grab corner values from children
        corners[i] = c->cornerState(i);

        all_empty &= (c->type == Interval::EMPTY);
        all_full  &= (c->type == Interval::FILLED);
    }

    this->type = all_empty ? Interval::EMPTY
               : all_full  ? Interval::FILLED : Interval::AMBIGUOUS;

    // If this cell is unambiguous, then forget all its branches and return
    if (this->type == Interval::FILLED || this->type == Interval::EMPTY)
    {
        this->releaseChildren(object_pool);
        this->done();
        return true;
    }

    auto corner_mask = buildCornerMask(corners);

    //  This conditional implements the three checks described in
    //  [Ju et al, 2002] in the section titled
    //      "Simplification with topology safety"
    bool manifold = cornersAreManifold(corner_mask) &&
        std::all_of(cs.begin(), cs.end(),
                [](DCTree<N>* o){ return o->isManifold(); }) &&
        leafsAreManifold(cs, corners);

    // If we're not manifold, then we can't collapse
    if (!manifold)
    {
        this->done();
        return true;
    }

    // We've now passed all of our opportunities to exit without
    // allocating a Leaf, so create one here.
    assert(this->leaf == nullptr);
    this->leaf = object_pool.next().get();
    this->leaf->manifold = true;
    this->leaf->corner_mask = corner_mask;

    // Populate the feature rank as the maximum of all children
    // feature ranks (as seen in DC: The Secret Sauce)
    this->leaf->rank = std::accumulate(cs.begin(), cs.end(), (unsigned)0,
            [](unsigned a, DCTree<N>* b){ return std::max(a, b->rank());} );


    // Accumulate the mass point, QEF matrices, and appropriate intersections.
    for (unsigned i=0; i < cs.size(); ++i)
    {
        const auto& c = cs[i];
        assert(c != nullptr);

        if (c->type == Interval::AMBIGUOUS)
        {
            assert(c->leaf != nullptr);
            if (c->leaf->rank == this->leaf->rank)
            {
                this->leaf->mass_point += c->leaf->mass_point;
            }
            this->leaf->AtA += c->leaf->AtA;
            this->leaf->AtB += c->leaf->AtB;
            this->leaf->BtB += c->leaf->BtB;

            for (auto& edge : edgesFromChild(i))
            {
                if (c->leaf->intersections[edge])
                {
                    assert(!this->leaf->intersections[edge]);
                    this->leaf->intersections[edge] = c->leaf->intersections[edge];
                }
            }
        }
        else
        {
            assert(c->leaf == nullptr);
        }
    }

    // If the vertex error is below a threshold, and the vertex
    // is well-placed in the distance field, then convert into
    // a leaf by erasing all of the child branches
    {
        bool collapsed = false;
        if (findVertex(this->leaf->vertex_count++) < max_err &&
            region.contains(vert(0), 1e-6))
        {
            Eigen::Vector3f v;
            v << vert(0).template cast<float>(),
                 region.perp.template cast<float>();
            if (fabs(eval->feature.eval(v, Tape::getBase(tape, v))) < max_err)
            {
                // Store this tree's depth based on the region's level
                this->leaf->level = region.level;

                // Then, erase all of the children and mark that we collapsed
                this->releaseChildren(object_pool);
                collapsed = true;
            }
        }
        if (!collapsed)
        {
            object_pool.next().put(this->leaf);
            this->leaf = nullptr;
        }
    }

    this->done();
    return true;
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
double DCTree<N>::findVertex(unsigned index)
{
    assert(this->leaf != nullptr);
    Eigen::SelfAdjointEigenSolver<Eigen::Matrix<double, N, N>> es(
            this->leaf->AtA);
    assert(this->leaf->mass_point(N) > 0);

    // We need to find the pseudo-inverse of AtA.
    auto eigenvalues = es.eigenvalues().real();

    // Truncate near-singular eigenvalues in the SVD's diagonal matrix
    Eigen::Matrix<double, N, N> D = Eigen::Matrix<double, N, N>::Zero();
    for (unsigned i=0; i < N; ++i)
    {
        D.diagonal()[i] = (fabs(eigenvalues[i]) < EIGENVALUE_CUTOFF)
            ? 0 : (1 / eigenvalues[i]);
    }

    // Get rank from eigenvalues
    if (!this->isBranch())
    {
        assert(index > 0 || this->leaf->rank == 0);
        this->leaf->rank = D.diagonal().count();
    }

    // SVD matrices
    auto U = es.eigenvectors().real().eval(); // = V

    // Pseudo-inverse of A
    auto AtAp = (U * D * U.transpose()).eval();

    // Solve for vertex position (minimizing distance to center)
    Vec center = this->leaf->mass_point.template head<N>() /
                 this->leaf->mass_point(N);
    Vec v = AtAp * (this->leaf->AtB - (this->leaf->AtA * center)) + center;

    // Store this specific vertex in the verts matrix
    this->leaf->verts.col(index) = v;

    // Return the QEF error
    return (v.transpose() * this->leaf->AtA * v -
            2*v.transpose() * this->leaf->AtB)[0]
            + this->leaf->BtB;
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
typename DCTree<N>::Vec DCTree<N>::vert(unsigned i) const
{
    assert(this->leaf != nullptr);
    assert(i < this->leaf->vertex_count);
    return this->leaf->verts.col(i);
}

template <unsigned N>
std::shared_ptr<IntersectionVec<N>> DCTree<N>::intersection(
        unsigned a, unsigned b) const
{
    assert(MarchingTable<N>::mt.e[a][b] != -1);
    return intersection(MarchingTable<N>::mt.e[a][b]);
}

template <unsigned N>
std::shared_ptr<IntersectionVec<N>> DCTree<N>::intersection(
        unsigned edge) const
{
    assert(this->leaf != nullptr);
    return this->leaf->intersections[edge];
}

template <unsigned N>
void DCTree<N>::setIntersectionPtr(
    unsigned edge, const std::shared_ptr<IntersectionVec<N>>& ptr) const
{
    assert(this->leaf != nullptr);
    auto& dest = this->leaf->intersections[edge];
    assert(*dest == *ptr);
    if (dest != ptr)
    {
        dest = ptr;
    }
}

template <unsigned N>
Interval::State DCTree<N>::cornerState(uint8_t i) const
{
    switch (this->type)
    {
        case Interval::AMBIGUOUS:
            assert(this->leaf != nullptr);
            return (this->leaf->corner_mask & (1 << i))
                ? Interval::FILLED : Interval::EMPTY;

        case Interval::UNKNOWN: assert(false);

        case Interval::FILLED:  // fallthrough
        case Interval::EMPTY:   assert(this->leaf == nullptr);
                                return this->type;
    };
    return this->type;
}

template <unsigned N>
bool DCTree<N>::isManifold() const
{
    assert(!this->isBranch());
    switch (this->type)
    {
        case Interval::AMBIGUOUS:
            assert(this->leaf != nullptr);
            return this->leaf->manifold;

        case Interval::UNKNOWN: assert(false);

        case Interval::FILLED:  // fallthrough
        case Interval::EMPTY:   assert(this->leaf == nullptr);
                                return true;
    };
    return true;
}

template <unsigned N>
uint8_t DCTree<N>::cornerMask() const
{
    assert(!this->isBranch());
    switch (this->type)
    {
        case Interval::AMBIGUOUS:
            assert(this->leaf != nullptr);
            return this->leaf->corner_mask;

        case Interval::UNKNOWN: assert(false);

        case Interval::FILLED:  assert(this->leaf == nullptr);
                                return ((1 << (1 << N)) - 1);
        case Interval::EMPTY:   assert(this->leaf == nullptr);
                                return 0;
    };
    return 0;
}

template <unsigned N>
unsigned DCTree<N>::level() const
{
    assert(!this->isBranch());
    switch (this->type)
    {
        case Interval::AMBIGUOUS:
            assert(this->leaf != nullptr);
            return this->leaf->level;

        case Interval::UNKNOWN: assert(false);

        case Interval::FILLED:  // fallthrough
        case Interval::EMPTY:   assert(this->leaf == nullptr);
                                return 0;
    };
    return 0;
}

template <unsigned N>
unsigned DCTree<N>::rank() const
{
    assert(!this->isBranch());
    switch (this->type)
    {
        case Interval::AMBIGUOUS:
            assert(this->leaf != nullptr);
            return this->leaf->rank;

        case Interval::UNKNOWN: assert(false);

        case Interval::FILLED:  // fallthrough
        case Interval::EMPTY:   assert(this->leaf == nullptr);
                                return 0;
    };
    return 0;
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
std::array<unsigned, 2 * N> DCTree<N>::edgesFromChild(unsigned childIndex)
{
    std::array<unsigned, 2 * N> out;
    for (unsigned i=0; i < N; ++i)
    {
        auto otherCorner = childIndex ^ (1 << i);
        assert(MarchingTable<N>::mt.e[childIndex][otherCorner] >= 0);
        assert(MarchingTable<N>::mt.e[otherCorner][childIndex] >= 0);
        out[2 * i] = MarchingTable<N>::mt.e[childIndex][otherCorner];
        out[2 * i + 1] = MarchingTable<N>::mt.e[otherCorner][childIndex];
    }
    return out;
}

template <unsigned N>
void DCTree<N>::releaseTo(Pool& object_pool) {
    if (this->leaf != nullptr) {
        object_pool.next().put(this->leaf);
        this->leaf = nullptr;
    }

    object_pool.put(this);
}

}   // namespace Kernel

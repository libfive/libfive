/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include <future>
#include <numeric>
#include <functional>
#include <limits>

#include <cmath>

#include <Eigen/StdVector>

#include "libfive/render/brep/xtree.hpp"
#include "libfive/render/axes.hpp"

namespace Kernel {

//  Here's our cutoff value (with a value set in the header)
template <unsigned N> constexpr double XTree<N>::EIGENVALUE_CUTOFF;

//  Allocating static var for marching cubes table
template <unsigned N>
std::unique_ptr<const Marching::MarchingTable<N>> XTree<N>::mt;

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
std::unique_ptr<const XTree<N>> XTree<N>::build(
        Tree t, Region<N> region, double min_feature,
        double max_err, bool multithread)
{
    std::atomic_bool cancel(false);
    const std::map<Tree::Id, float> vars;
    return build(t, vars, region, min_feature, max_err, multithread, cancel);
}

template <unsigned N>
std::unique_ptr<const XTree<N>> XTree<N>::build(
            Tree t, const std::map<Tree::Id, float>& vars,
            Region<N> region, double min_feature,
            double max_err, bool multithread,
            std::atomic_bool& cancel)
{
    if (multithread)
    {
        std::vector<XTreeEvaluator, Eigen::aligned_allocator<XTreeEvaluator>> es;
        es.reserve(1 << N);
        for (unsigned i=0; i < (1 << N); ++i)
        {
            es.emplace_back(XTreeEvaluator(t, vars));
        }
        return build(es.data(), region, min_feature, max_err, true, cancel);
    }
    else
    {
        XTreeEvaluator e(t, vars);
        return build(&e, region, min_feature, max_err, false, cancel);
    }
}

template <unsigned N>
std::unique_ptr<const XTree<N>> XTree<N>::build(
        XTreeEvaluator* es,
        Region<N> region, double min_feature,
        double max_err, bool multithread,
        std::atomic_bool& cancel)
{
    // Lazy initialization of marching squares / cubes table
    if (mt.get() == nullptr)
    {
        mt = Marching::buildTable<N>();
    }

    // Sanity-checking of min_feature argument
    if (!(min_feature > 0))
    {
        std::cerr << "XTree<" << N << ">::build: min_feature must be > 0 (not "
                  << min_feature << ")" << std::endl;
        return nullptr;
    }

    auto out = new XTree(es, region, min_feature, max_err,
                         multithread, cancel, Neighbors<N>());

    // Return an empty XTree when cancelled
    // (to avoid potentially ambiguous or mal-constructed trees situations)
    if (cancel.load())
    {
        delete out;
        out = nullptr;
    }

    return std::unique_ptr<const XTree<N>>(out);
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
XTree<N>::XTree(XTreeEvaluator* eval, Region<N> region,
                double min_feature, double max_err, bool multithread,
                std::atomic_bool& cancel, Neighbors<N> neighbors)
    : region(region), _mass_point(Eigen::Matrix<double, N + 1, 1>::Zero()),
      AtA(Eigen::Matrix<double, N, N>::Zero()),
      AtB(Eigen::Matrix<double, N, 1>::Zero())
{
    if (cancel.load())
    {
        return;
    }

    // Clear all indices
    std::fill(index.begin(), index.end(), 0);

    const bool do_recurse = ((region.upper - region.lower) > min_feature).any();

    // Do a preliminary evaluation to prune the tree
    Interval::I i(-1, 1);
    if (do_recurse)
    {
        i = eval->interval.evalAndPush(region.lower3().template cast<float>(),
                                       region.upper3().template cast<float>());
    }

    if (Interval::isFilled(i))
    {
        type = Interval::FILLED;
    }
    else if (Interval::isEmpty(i))
    {
        type = Interval::EMPTY;
    }
    // If the cell wasn't empty or filled, attempt to subdivide and recurse
    else
    {
        bool all_empty = true;
        bool all_full  = true;

        // Recurse until volume is too small
        if (do_recurse)
        {
            auto rs = region.subdivide();

            if (multithread)
            {
                // Evaluate every child in a separate thread
                std::array<std::future<XTree<N>*>, 1 << N> futures;

                assert(children.size() == futures.size());

                for (unsigned i=0; i < children.size(); ++i)
                {
                    futures[i] = std::async(std::launch::async,
                        [&eval, &rs, &cancel, i, min_feature, max_err]()
                        { return new XTree(
                                eval + i, rs[i], min_feature, max_err,
                                false, cancel, Neighbors<N>()); });
                }
                for (unsigned i=0; i < children.size(); ++i)
                {
                    children[i].reset(futures[i].get());
                }
            }
            // Single-threaded recursive construction
            else
            {
                for (uint8_t i=0; i < children.size(); ++i)
                {
                    // Populate child recursively
                    children[i].reset(new XTree<N>(
                                eval, rs[i], min_feature, max_err,
                                false, cancel, neighbors.push(i, children)));
                }
            }

            // Abort early if children could have been mal-constructed
            // by an early cancel operation
            if (cancel.load())
            {
                if (do_recurse)
                {
                    eval->interval.pop();
                }
                return;
            }

            // Update corner and filled / empty state from children
            for (uint8_t i=0; i < children.size(); ++i)
            {
                // Grab corner values from children
                corners[i] = children[i]->corners[i];
                corner_positions.row(i) = children[i]->corner_positions.row(i);

                all_empty &= children[i]->type == Interval::EMPTY;
                all_full  &= children[i]->type == Interval::FILLED;
            }
        }
        // Terminate recursion here
        else
        {
            // Store the corner positions
            for (unsigned i=0; i < (1 << N); ++i)
            {
                Eigen::Array<double, 1, N> out;
                for (unsigned axis=0; axis < N; ++axis)
                {
                    out(axis) = (i & (1 << axis)) ? region.upper(axis)
                                                  : region.lower(axis);
                }
                corner_positions.row(i) = out;
            }

            // Pack corners into evaluator
            Eigen::Matrix<float, 3, 1 << N> pos;

            // Track how many corners have to be evaluated here
            // (if they can be looked up from a neighbor, they don't have
            //  to be evaluated here, which can save time)
            size_t count = 0;

            // Remap from a value in the range [0, count) to a corner index
            // in the range [0, 1 <<N).
            std::array<int, 1 << N> corner_indices;

            for (uint8_t i=0; i < children.size(); ++i)
            {
                auto c = neighbors.check(i);
                if (c == Interval::UNKNOWN)
                {
                    pos.col(count) << cornerPos(i).template cast<float>(),
                                      region.perp.template cast<float>();
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
            auto vs = eval->array.values(count);

            // We store ambiguity here, but clear it if the point is inside
            // or outside (so after the loop below, ambig(i) is only set if
            // pos[i] is both == 0 and ambiguous).
            auto ambig = eval->array.getAmbiguous(count);

            // This is a count of how many points there are that == 0
            // but are unambiguous; unambig_remap[z] returns the index
            // into the pos[] array for a particular unambiguous zero.
            uint8_t unambiguous_zeros = 0;
            std::array<int, 1 << N> unambig_remap;

            // This is phase 1, as described above
            for (uint8_t i=0; i < count; ++i)
            {
                // Handle inside, outside, and (non-ambiguous) on-boundary
                if (vs(i) > 0 || !std::isfinite(vs(i)))
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
                    eval->array.set(pos.col(i), unambiguous_zeros);
                    unambig_remap[unambiguous_zeros] = corner_indices[i];
                    unambiguous_zeros++;
                }
            }

            // Phase 2: Optimization for non-ambiguous features
            // We can get both positive and negative values out if
            // there's a non-zero gradient.
            if (unambiguous_zeros)
            {
                auto ds = eval->array.derivs(unambiguous_zeros);
                for (unsigned i=0; i < unambiguous_zeros; ++i)
                {
                    corners[unambig_remap[i]] =
                        (ds.col(i).template head<3>() != 0).any()
                            ? Interval::FILLED : Interval::EMPTY;
                }
            }

            // Phase 3: One last pass for handling ambiguous corners
            for (uint8_t i=0; i < count; ++i)
            {
                if (ambig(i))
                {
                    corners[corner_indices[i]] =
                        eval->feature.isInside(pos.col(i))
                            ? Interval::FILLED
                            : Interval::EMPTY;
                }
            }

            // Pack corners into filled / empty arrays
            for (uint8_t i=0; i < children.size(); ++i)
            {
                all_full  = all_full && corners[i];
                all_empty = all_empty && !corners[i];
            }
        }
        type = all_empty ? Interval::EMPTY
             : all_full  ? Interval::FILLED : Interval::AMBIGUOUS;
    }

    // If this cell is unambiguous, then fill its corners with values and
    // forget all its branches; these may be no-ops, but they're idempotent
    if (type == Interval::FILLED || type == Interval::EMPTY)
    {
        std::fill(corners.begin(), corners.end(), type);
        std::for_each(children.begin(), children.end(),
            [](std::unique_ptr<const XTree<N>>& o) { o.reset(); });
        manifold = true;
    }

    // Build and store the corner mask
    for (unsigned i=0; i < children.size(); ++i)
    {
        corner_mask |= (corners[i] == Interval::FILLED) << i;
    }

    // Branch checking and simplifications
    if (isBranch())
    {
        // Store this tree's depth as a function of its children
        level = std::accumulate(children.begin(), children.end(), (unsigned)0,
            [](const unsigned& a, const std::unique_ptr<const XTree<N>>& b)
            { return std::max(a, b->level);} ) + 1;

        // If all children are non-branches, then we could collapse
        if (std::all_of(children.begin(), children.end(),
                        [](const std::unique_ptr<const XTree<N>>& o)
                        { return !o->isBranch(); }))
        {
            //  This conditional implements the three checks described in
            //  [Ju et al, 2002] in the section titled
            //      "Simplification with topology safety"
            manifold = cornersAreManifold() &&
                std::all_of(children.begin(), children.end(),
                        [](const std::unique_ptr<const XTree<N>>& o)
                        { return o->manifold; }) &&
                leafsAreManifold();

            // Attempt to collapse this tree by positioning the vertex
            // in the summed QEF and checking to see if the error is small
            if (manifold)
            {
                // Populate the feature rank as the maximum of all children
                // feature ranks (as seen in DC: The Secret Sauce)
                rank = std::accumulate(
                        children.begin(), children.end(), (unsigned)0,
                        [](unsigned a, const std::unique_ptr<const XTree<N>>& b)
                            { return std::max(a, b->rank);} );

                // Accumulate the mass point and QEF matrices
                for (const auto& c : children)
                {
                    if (c->rank == rank)
                    {
                        _mass_point += c->_mass_point;
                    }
                    AtA += c->AtA;
                    AtB += c->AtB;
                    BtB += c->BtB;
                }
                assert(region.contains(massPoint()));

                // If the vertex error is below a threshold, and the vertex
                // is well-placed in the distance field, then convert into
                // a leaf by erasing all of the child branches
                if (findVertex(vertex_count++) < max_err &&
                    fabs(eval->feature.baseEval(vert3().template cast<float>()))
                        < max_err)
                {
                    std::for_each(children.begin(), children.end(),
                        [](std::unique_ptr<const XTree<N>>& o) { o.reset(); });
                }
                else
                {
                    vertex_count = 0;
                }
            }
        }
    }
    else if (type == Interval::AMBIGUOUS)
    {
        // Figure out if the leaf is manifold
        manifold = cornersAreManifold();

        // We'll use this vector anytime we need to pass something
        // into the evaluator (which requires a Vector3f)
        Eigen::Vector3f _pos;
        _pos.template tail<3 - N>() = region.perp.template cast<float>();
        auto set = [&](const Vec& v, size_t i){
            _pos.template head<N>() = v.template cast<float>();
            eval->array.set(_pos, i);
        };

        // Iterate over manifold patches, storing one vertex per patch
        const auto& ps = mt->v[corner_mask];
        while (vertex_count < ps.size() && ps[vertex_count][0].first != -1)
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
                     edge_count < ps[vertex_count].size() &&
                         ps[vertex_count][edge_count].first != -1;
                     ++edge_count)
                {
                    // Sanity-checking
                    assert(corners[ps[vertex_count][edge_count].first]
                           == Interval::FILLED);
                    assert(corners[ps[vertex_count][edge_count].second]
                           == Interval::EMPTY);

                    // Store the edge index associated with this target
                    auto c = ps[vertex_count][edge_count];
                    edges[edge_count] = mt->e[c.first][c.second];

                    auto compare = neighbors.check(c.first, c.second);
                    // Enable this to turn on sharing of results with neighbors
                    if (compare != nullptr && compare->size() > 0)
                    {
                        intersections[edges[edge_count]] = *compare;
                    }
                    else
                    {
                        // Store inside / outside in targets array, and the edge
                        // index in the eval_edges array.
                        targets[eval_count] = {cornerPos(c.first),
                                               cornerPos(c.second)};
                        eval_edges[eval_count] = edges[edge_count];

                        assert(eval_edges[eval_count] < intersections.size());
                        eval_count++;
                    }

                    assert(edges[edge_count] < intersections.size());
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
                                POINTS_PER_SEARCH * eval_count);

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
                                                pos.template cast<float>()))
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
                    auto ds = eval->array.derivs(2 * eval_count);
                    auto ambig = eval->array.getAmbiguous(2 * eval_count);

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
                            const Eigen::Array<double, N, 1> derivs = ds.col(i)
                                .template cast<double>().template head<N>();
                            const double norm = derivs.matrix().norm();

                            // Find normalized derivatives and distance value
                            Eigen::Matrix<double, N, 1> dv = derivs / norm;
                            if (dv.array().isFinite().all())
                            {
                                intersections[eval_edges[i/2]]
                                    .push_back({pos.template head<N>(),
                                                dv, ds.col(i).w() / norm});
                            }
                        }
                        // Otherwise, we need to use the feature-finding special
                        // case to find all possible derivatives at this point.
                        else
                        {
                            const auto fs = eval->feature.features(
                                    pos.template cast<float>());

                            for (auto& f : fs)
                            {
                                // Unpack 3D derivatives into XTree-specific
                                // dimensionality, and find normal.
                                const Eigen::Array<double, N, 1> derivs = f
                                    .template head<N>()
                                    .template cast<double>();
                                const double norm = derivs.matrix().norm();

                                // Find normalized derivatives and distance
                                // value (from the earlier evaluation)
                                Eigen::Matrix<double, N, 1> dv = derivs / norm;
                                if (dv.array().isFinite().all())
                                {
                                    intersections[eval_edges[i/2]]
                                        .push_back({pos.template head<N>(),
                                                dv, ds.col(i).w() / norm});
                                }
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

                for (const auto& t : intersections[edges[i]])
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
            _mass_point = _mass_point.Zero();


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
                        const auto& inter = intersections[edges[i]];
                        const auto size = inter.size();
                        if (size >= 1)
                        {
                            mp << intersections[edges[i]][0].pos, 1;
                            _mass_point += mp;
                            mp << intersections[edges[i]][size - 1].pos, 1;
                            _mass_point += mp;
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
                rows += intersections[edges[i]].size();
            }

            // Now, we'll unpack into A and b matrices
            //
            //  The A matrix is of the form
            //  [n1x, n1y, n1z]
            //  [n2x, n2y, n2z]
            //  [n3x, n3y, n3z]
            //  ...
            //  (with one row for each sampled point's normal)
            Eigen::Matrix<double, Eigen::Dynamic, N> A(rows, N);

            //  The b matrix is of the form
            //  [p1 . n1]
            //  [p2 . n2]
            //  [p3 . n3]
            //  ...
            //  (with one row for each sampled point)
            Eigen::Matrix<double, Eigen::Dynamic, 1> b(rows, 1);

            // Load samples into the QEF arrays
            //
            // Since we're deliberately sampling on either side of the
            // intersection, we subtract out the distance-field value
            // to make the math work out.
            unsigned r=0;
            for (unsigned i=0; i < edge_count; ++i)
            {
                for (unsigned j=0; j < intersections[edges[i]].size(); ++j)
                {
                    A.row(r) << intersections[edges[i]][j].deriv
                                                .transpose();
                    b(r) = A.row(r).dot(intersections[edges[i]][j].pos) -
                           intersections[edges[i]][j].value;
                    r++;
                }
            }

            // Save compact QEF matrices
            auto At = A.transpose().eval();
            AtA = At * A;
            AtB = At * b;
            BtB = b.transpose() * b;

            // Find the vertex position, storing into the appropriate column
            // of the vertex array and ignoring the error result (because
            // this is the bottom of the recursion)
            findVertex(vertex_count++);
        }
    }

    // ...and we're done.
    if (do_recurse)
    {
        eval->interval.pop();
    }
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
double XTree<N>::findVertex(unsigned index)
{
    Eigen::EigenSolver<Eigen::Matrix<double, N, N>> es(AtA);
    assert(_mass_point(N) > 0);

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
    if (!isBranch())
    {
        assert(index > 0 || rank == 0);
        rank = D.diagonal().count();
    }

    // SVD matrices
    auto U = es.eigenvectors().real().eval(); // = V

    // Pseudo-inverse of A
    auto AtAp = (U * D * U.transpose()).eval();

    // Solve for vertex position (minimizing distance to center)
    auto center = massPoint();
    Vec v = AtAp * (AtB - (AtA * center)) + center;

    // Store this specific vertex in the verts matrix
    verts.col(index) = v;

    // Return the QEF error
    return (v.transpose() * AtA * v - 2*v.transpose() * AtB)[0] + BtB;
}

////////////////////////////////////////////////////////////////////////////////

template <unsigned N>
Eigen::Vector3d XTree<N>::vert3(unsigned index) const
{
    Eigen::Vector3d out;
    out << vert(index), region.perp.template cast<double>();
    return out;
}

template <unsigned N>
typename XTree<N>::Vec XTree<N>::massPoint() const
{
    return _mass_point.template head<N>() / _mass_point(N);
}

}   // namespace Kernel

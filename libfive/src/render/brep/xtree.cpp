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

    auto out = new XTree(es, region, min_feature, max_err, multithread, cancel);

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
                std::atomic_bool& cancel)
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

    // Do a preliminary evaluation to prune the tree
    auto i = eval->interval.evalAndPush(region.lower3().template cast<float>(),
                                        region.upper3().template cast<float>());

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
        if (((region.upper - region.lower) > min_feature).any())
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
                                false, cancel); });
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
                                false, cancel));
                }
            }

            // Abort early if children could have been mal-constructed
            // by an early cancel operation
            if (cancel.load())
            {
                eval->interval.pop();
                return;
            }

            // Update corner and filled / empty state from children
            for (uint8_t i=0; i < children.size(); ++i)
            {
                // Grab corner values from children
                corners[i] = children[i]->corners[i];

                all_empty &= children[i]->type == Interval::EMPTY;
                all_full  &= children[i]->type == Interval::FILLED;
            }
        }
        // Terminate recursion here
        else
        {
            // Pack corners into evaluator
            std::array<Eigen::Vector3f, 1 << N> pos;
            for (uint8_t i=0; i < children.size(); ++i)
            {
                pos[i] << cornerPos(i).template cast<float>(),
                          region.perp.template cast<float>();
                eval->array.set(pos[i], i);
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
            auto vs = eval->array.values(children.size());

            // We store ambiguity here, but clear it if the point is inside
            // or outside (so after the loop below, ambig(i) is only set if
            // pos[i] is both == 0 and ambiguous).
            auto ambig = eval->array.getAmbiguous(children.size());

            // This is a count of how many points there are that == 0
            // but are unambiguous; unambig_remap[z] returns the index
            // into the pos[] array for a particular unambiguous zero.
            uint8_t unambiguous_zeros = 0;
            std::array<int, 1 << N> unambig_remap;

            // This is phase 1, as described above
            for (uint8_t i=0; i < children.size(); ++i)
            {
                // Handle inside, outside, and (non-ambiguous) on-boundary
                if (vs(i) > 0 || !std::isfinite(vs(i)))
                {
                    corners[i] = Interval::EMPTY;
                    ambig(i) = false;
                }
                else if (vs(i) < 0)
                {
                    corners[i] = Interval::FILLED;
                    ambig(i) = false;
                }
                else if (!ambig(i))
                {
                    eval->array.set(pos[i], unambiguous_zeros);
                    unambig_remap[unambiguous_zeros] = i;
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
            for (uint8_t i=0; i < children.size(); ++i)
            {
                if (ambig(i))
                {
                    corners[i] = eval->feature.isInside(pos[i])
                        ? Interval::FILLED : Interval::EMPTY;
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

        // Here, we'll prepare to store position, {normal, value} pairs
        // for every crossing and feature
        typedef std::pair<Vec, Eigen::Matrix<double, N + 1, 1>> Intersection;
        std::vector<Intersection, Eigen::aligned_allocator<Intersection>>
            intersections;

        // RAM is cheap, so reserve a bunch of space here to avoid
        // the need for re-allocating later on
        intersections.reserve(_edges(N) * 2);

        // We'll use this vector anytime we need to pass something
        // into the evaluator (which requires a Vector3f)
        Eigen::Vector3f _pos;
        _pos.template tail<3 - N>() = region.perp.template cast<float>();
        auto set = [&](const Vec& v, size_t i){
            _pos.template head<N>() = v.template cast<float>();
            eval->array.set(_pos, i);
        };

        // Iterate over manifold patches for this corner case
        const auto& ps = mt->v[corner_mask];
        while (vertex_count < ps.size() && ps[vertex_count][0].first != -1)
        {
            // Iterate over edges in this patch, storing [inside, outside]
            unsigned target_count;
            std::array<std::pair<Vec, Vec>, _edges(N)> targets;
            std::array<int, _edges(N)> ranks;
            std::fill(ranks.begin(), ranks.end(), -1);

            for (target_count=0; target_count < ps[vertex_count].size() &&
                          ps[vertex_count][target_count].first != -1; ++target_count)
            {
                // Sanity-checking
                assert(corners[ps[vertex_count][target_count].first]
                       == Interval::FILLED);
                assert(corners[ps[vertex_count][target_count].second]
                       == Interval::EMPTY);

                // Store inside / outside in targets array
                targets[target_count] = {cornerPos(ps[vertex_count][target_count].first),
                                  cornerPos(ps[vertex_count][target_count].second)};
            }

            // We do an N-fold reduction at each stage
            constexpr int SEARCH_COUNT = 4;
            constexpr int POINTS_PER_SEARCH = 16;
            static_assert(_edges(N) * POINTS_PER_SEARCH <= ArrayEvaluator::N,
                          "Potential overflow");

            // Multi-stage binary search for intersection
            for (int s=0; s < SEARCH_COUNT; ++s)
            {
                // Load search points into evaluator
                Eigen::Array<double, N, POINTS_PER_SEARCH * _edges(N)> ps;
                for (unsigned e=0; e < target_count; ++e)
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
                auto out = eval->array.values(
                        POINTS_PER_SEARCH * target_count);

                for (unsigned e=0; e < target_count; ++e)
                {
                    // Skip one point, because the very first point is
                    // already known to be inside the shape (but sometimes,
                    // due to numerical issues, it registers as outside!)
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
                        // Special-case for final point in the search, working
                        // around numerical issues where different evaluators
                        // disagree with whether points are inside or outside.
                        else if (j == POINTS_PER_SEARCH - 1)
                        {
                            targets[e] = {ps.col(i - 1), ps.col(i)};
                            break;
                        }
                    }
                }
            }

            // Reset mass point and intersections
            _mass_point = _mass_point.Zero();
            intersections.clear();

            // Store mass point and prepare for a bulk evaluation
            static_assert(_edges(N) * 2 <= ArrayEvaluator::N,
                          "Too many results");
            for (unsigned i=0; i < target_count; ++i)
            {
                set(targets[i].first, 2*i);
                set(targets[i].second, 2*i + 1);
            }

            // Evaluate values and derivatives
            auto ds = eval->array.derivs(2 * target_count);
            auto ambig = eval->array.getAmbiguous(2 * target_count);

            // Iterate over all inside-outside pairs, storing the number
            // of intersections before each inside node (in prev_size), then
            // checking the rank of the pair after each outside node based
            // on the accumulated intersections.
            size_t prev_size;
            for (unsigned i=0; i < 2 * target_count; ++i)
            {
                if (!(i & 1))
                {
                    prev_size = intersections.size();
                }

                if (!ambig(i))
                {
                    const Eigen::Array<double, N, 1> derivs = ds.col(i)
                        .template cast<double>().template head<N>();
                    const double norm = derivs.matrix().norm();

                    // Find normalized derivatives and distance value
                    Eigen::Matrix<double, N + 1, 1> dv;
                    dv << derivs / norm, ds.col(i).w() / norm;
                    if (dv.array().isFinite().all())
                    {
                        intersections.push_back({
                            (i & 1) ? targets[i/2].second : targets[i/2].first,
                            dv});
                    }
                }
                else
                {
                    // Load the ambiguous position and find its features
                    Eigen::Vector3f pos;
                    pos << ((i & 1) ? targets[i/2].second : targets[i/2].first)
                                .template cast<float>(),
                           region.perp.template cast<float>();
                    const auto fs = eval->feature.features(pos);

                    for (auto& f : fs)
                    {
                        // Unpack 3D derivatives into XTree-specific
                        // dimensionality, and find normal.
                        const Eigen::Array<double, N, 1> derivs = f
                            .template head<N>()
                            .template cast<double>();
                        const double norm = derivs.matrix().norm();

                        // Find normalized derivatives and distance value
                        // (from the earlier evaluation)
                        Eigen::Matrix<double, N + 1, 1> dv;
                        dv << derivs / norm, ds.col(i).w() / norm;
                        if (dv.array().isFinite().all())
                        {
                            intersections.push_back({
                                (i & 1) ? targets[i/2].second
                                        : targets[i/2].first, dv});
                        }
                    }
                }

                if (i & 1)
                {
                    // If every intersection was NaN (?!), use rank 0;
                    // otherwise, figure out how many normals diverge
                    if (prev_size == intersections.size())
                    {
                        ranks[i / 2] = 0;
                    }
                    else
                    {
                        ranks[i / 2] = 1;
                        Vec prev_normal = intersections[prev_size]
                            .second
                            .template head<N>();
                        for (unsigned p=prev_size + 1;
                                      p < intersections.size(); ++p)
                        {
                            // Accumulate rank based on cosine distance
                            ranks[i / 2] += intersections[p]
                                .second
                                .template head<N>()
                                .dot(prev_normal) < 0.9;
                        }
                    }
                }
            }

            {   // Build the mass point from max-rank intersections
                const int max_rank = *std::max_element(
                        ranks.begin(), ranks.end());
                for (unsigned i=0; i < target_count; ++i)
                {
                    assert(ranks[i] != -1);
                    if (ranks[i] == max_rank)
                    {
                        // Accumulate this intersection in the mass point
                        Eigen::Matrix<double, N + 1, 1> mp;
                        mp << targets[i].first, 1;
                        _mass_point += mp;
                        mp << targets[i].second, 1;
                        _mass_point += mp;
                    }
                }
            }

            // Now, we'll unpack into A and b matrices
            //
            //  The A matrix is of the form
            //  [n1x, n1y, n1z]
            //  [n2x, n2y, n2z]
            //  [n3x, n3y, n3z]
            //  ...
            //  (with one row for each sampled point's normal)
            Eigen::Matrix<double, Eigen::Dynamic, N> A(intersections.size(), N);

            //  The b matrix is of the form
            //  [p1 . n1]
            //  [p2 . n2]
            //  [p3 . n3]
            //  ...
            //  (with one row for each sampled point)
            Eigen::Matrix<double, Eigen::Dynamic, 1> b(intersections.size(), 1);

            // Load samples into the QEF arrays
            //
            // Since we're deliberately sampling on either side of the
            // intersection, we subtract out the distance-field value
            // to make the math work out.
            for (unsigned i=0; i < intersections.size(); ++i)
            {
                A.row(i) << intersections[i].second
                                            .template head<N>()
                                            .transpose();
                b(i) = A.row(i).dot(intersections[i].first) -
                       intersections[i].second(N);
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
    eval->interval.pop();
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

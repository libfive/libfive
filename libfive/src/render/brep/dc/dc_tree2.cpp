/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include "dc_tree.inl"

namespace libfive {

template <>
bool DCTree<2>::cornersAreManifold(const uint8_t corner_mask)
{
    const static bool corner_table[] =
        {1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1};
    return corner_table[corner_mask];
}

////////////////////////////////////////////////////////////////////////////////
// Specializations for quadtree
template <>
bool DCTree<2>::leafsAreManifold(
        const std::array<DCTree<2>*, 1 << 2>& cs,
        const std::array<Interval::State, 1 << 2>& corners)
{
    /*  See detailed comment in Octree::leafsAreManifold */
    const bool edges_safe =
        (cs[0]->cornerState(Axis::X) == corners[0] ||
         cs[0]->cornerState(Axis::X) == corners[Axis::X])
    &&  (cs[0]->cornerState(Axis::Y) == corners[0] ||
         cs[0]->cornerState(Axis::Y) == corners[Axis::Y])
    &&  (cs[Axis::X]->cornerState(Axis::X|Axis::Y) == corners[Axis::X] ||
         cs[Axis::X]->cornerState(Axis::X|Axis::Y) == corners[Axis::X|Axis::Y])
    &&  (cs[Axis::Y]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y] ||
         cs[Axis::Y]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y|Axis::X]);

    const bool faces_safe =
        (cs[0]->cornerState(Axis::Y|Axis::X) == corners[0] ||
         cs[0]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y] ||
         cs[0]->cornerState(Axis::Y|Axis::X) == corners[Axis::X] ||
         cs[0]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y|Axis::X]);

    return edges_safe && faces_safe;
}

// Explicit initialization of templates
template class DCTree<2>;
template struct DCLeaf<2>;

///-------------------------------------------------------------------------------------
///-------------------------------------------------------------------------------------
///-------------------------------------------------------------------------------------
///-------------------------------------------------------------------------------------

PseudoDCTree2::PseudoDCTree2(PseudoDCTree2* parent, unsigned index, const Region<2>& r)
    : XTree<2, PseudoDCTree2, DCLeaf<2>>(parent, index, r)
{
    // Nothing to do here
}

PseudoDCTree2::PseudoDCTree2()
{
    // Nothing to do here
}

PseudoDCTree2::PseudoDCTree2(Interval::State t)
{
    this->type = t;
}

std::unique_ptr<PseudoDCTree2> PseudoDCTree2::empty()
{
    std::unique_ptr<PseudoDCTree2> t(new PseudoDCTree2);
    t->type = Interval::EMPTY;
    return t;
}

double PseudoDCTree2::findVertex(unsigned index) {
    assert(this->leaf->mass_point(2) > 0);
    Vec center = this->leaf->mass_point.template head<2>() / this->leaf->mass_point(2);

    // Store this specific vertex in the verts matrix
    this->leaf->verts.col(index) = center;
    return std::numeric_limits<double>::max();
}

Tape::Handle PseudoDCTree2::evalInterval(Evaluator* eval,
                                     const Tape::Handle& tape,
                                     Pool& pool)
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
        // If this is unambiguous, then we're actually installed the singleton
        // into the parent, so we can release this tree to the pool.  This is
        // a bit scary, because it's still used in WorkerPool, but the pool
        // won't change it until we're done operating on it.
        if (this->done()) {
            releaseTo(pool);
        }

        if (tape != o.second) {
            eval->getDeck()->claim(std::move(o.second));
            return nullptr;
        }
    }
    return o.second;
}

void PseudoDCTree2::evalLeaf(Evaluator* eval,
                        const Tape::Handle& tape,
                        Pool& object_pool,
                        const PseudoDCNeighbors2& neighbors)
{
    // Track how many corners have to be evaluated here
    // (if they can be looked up from a neighbor, they don't have
    //  to be evaluated here, which can save time)
    size_t count = 0;

    // Remap from a value in the range [0, count) to a corner index
    // in the range [0, 1 <<N).
    std::array<int, 1 << 2> corner_indices;

    // Local array of corners
    std::array<Interval::State, 1 << 2> corners;

    // Pack corners into evaluator
    Eigen::Matrix<float, 3, 1 << 2> pos;
    for (uint8_t i=0; i < this->children.size(); ++i)
    {
        auto c = neighbors.check(i);
        if (c == Interval::UNKNOWN)
        {
            pos.col(count) = this->region.corner3f(i);
            eval->set(pos.col(count), count);
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
    auto vs = eval->values(count, *tape);

    // We store ambiguity here, but clear it if the point is inside
    // or outside (so after the loop below, ambig(i) is only set if
    // pos[i] is both == 0 and ambiguous).
    auto ambig = eval->getAmbiguous(count, *tape);

    // This is a count of how many points there are that == 0
    // but are unambiguous; unambig_remap[z] returns the index
    // into the corner_indices array for a particular unambiguous zero.
    uint8_t unambiguous_zeros = 0;
    std::array<int, 1 << 2> unambig_remap;

    // This is phase 1, as described above
    for (uint8_t i=0; i < count; ++i)
    {
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
            eval->set(pos.col(unambig_remap[i]), i);
        }
        auto ds = eval->derivs(unambiguous_zeros, *tape);
        for (unsigned i=0; i < unambiguous_zeros; ++i)
        {
            corners[corner_indices[unambig_remap[i]]] =
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
                eval->isInside(pos.col(i), tape)
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
        if (this->done()) {
            releaseTo(object_pool);
        }
        return;
    }

    assert(this->leaf == nullptr);
    this->leaf = object_pool.next().get();
    this->leaf->corner_mask = buildCornerMask(corners);

    // Now, for the fun part of actually placing vertices!
    // Figure out if the leaf is manifold
    this->leaf->manifold = cornersAreManifold(this->leaf->corner_mask);

    // Iterate over manifold patches, storing one vertex per patch
    const auto& ps = MarchingTable<2>::v(this->leaf->corner_mask);
    while (this->leaf->vertex_count < ps.size() &&
           ps[this->leaf->vertex_count][0].first != -1)
    {
        // Number of edges, total
        unsigned edge_count;

        // Edge indices (as found with mt->e[a][b]) for all edges,
        // with edge_count valid entries.
        std::array<size_t, _edges(2)> edges;

        {   // Within this block, we calculate all edges that haven't
            // already been calculated by neighbors and store them in
            // the appropriate slot of the intersections array.

            // 2umbers of edges that need evaluation
            unsigned eval_count;

            // Inside-outside pairs, with eval_count valid pairs
            std::array<std::pair<Vec, Vec>, _edges(2)> targets;

            // Edge indices (as found with mt->e[a][b]) for edges under
            // evaluation, with eval_count valid values.
            std::array<size_t, _edges(2)> eval_edges;

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
                edges[edge_count] = MarchingTable<2>::e(c.first)[c.second];

                auto compare = neighbors.check(c.first, c.second);
                // Enable this to turn on sharing of results with neighbors
                if (compare != nullptr)
                {
                    this->leaf->intersections[edges[edge_count]] = compare;
                }
                else
                {
                    // Store inside / outside in targets array, and the edge
                    // index in the eval_edges array.
                    targets[eval_count] = {this->region.corner(c.first),
                                           this->region.corner(c.second)};
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
                    _edges(2) * POINTS_PER_SEARCH <= ArrayEvaluator::N,
                    "Potential overflow");

            // Multi-stage binary search for intersection
            for (int s=0; s < SEARCH_COUNT; ++s)
            {
                // Load search points into evaluator
                Eigen::Array<double, 2, POINTS_PER_SEARCH * _edges(2)> ps;
                for (unsigned e=0; e < eval_count; ++e)
                {
                    for (int j=0; j < POINTS_PER_SEARCH; ++j)
                    {
                        const double frac = j / (POINTS_PER_SEARCH - 1.0);
                        const unsigned i = j + e*POINTS_PER_SEARCH;
                        ps.col(i) = (targets[e].first * (1 - frac)) +
                                    (targets[e].second * frac);
                        eval->set<2>(ps.col(i), this->region, i);
                    }
                }

                // Evaluate, then search for the first outside point
                // and adjust inside / outside to their new positions
                if (eval_count)
                {
                    // Store the results here, because calling isInside
                    // invalidates the output array.
                    Eigen::Array<float, 1, ArrayEvaluator::N> out;
                    out.leftCols(POINTS_PER_SEARCH * eval_count) =
                        eval->values(
                            POINTS_PER_SEARCH * eval_count, *tape);

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
                                if (!eval->isInside<2>(ps.col(i), this->region,
                                                               tape))
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
            static_assert(_edges(2) * 2 <= ArrayEvaluator::N,
                          "Too many results");
            if (eval_count)
            {
                for (unsigned i=0; i < eval_count; ++i)
                {
                    eval->set<2>(targets[i].first, this->region, 2*i);
                    eval->set<2>(targets[i].second, this->region, 2*i + 1);
                }

                // Copy the results to a local array, to avoid invalidating
                // the results array when we call features() below.
                Eigen::Array<float, 4, ArrayEvaluator::N> ds;
                ds.leftCols(2 * eval_count) = eval->derivs(
                        2 * eval_count, *tape);
                auto ambig = eval->getAmbiguous(2 * eval_count, *tape);

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
                           this->region.perp;

                    // If this position is unambiguous, then we can use the
                    // derivatives value calculated and stored in ds.
                    if (!ambig(i))
                    {
                        saveIntersection(pos.template head<2>(),
                                         ds.col(i).template cast<double>()
                                                  .template head<2>(),
                                         ds.col(i).w(), eval_edges[i/2],
                                         object_pool);
                    }
                    // Otherwise, we need to use the feature-finding special
                    // case to find all possible derivatives at this point.
                    else
                    {
                        const auto fs = eval->features(
                                pos.template cast<float>(), tape);

                        for (auto& f : fs)
                        {
                            saveIntersection(pos.template head<2>(),
                                             f.template head<2>()
                                              .template cast<double>(),
                                             ds.col(i).w(), eval_edges[i/2],
                                             object_pool);
                        }
                    }
                }
            }
        }
        // At this point, every [intersections[e] for e in edges] should be
        // populated with an Intersection object, whether taken from a neighbor
        // or calculated in the code above.

        // Reset the mass point, since we may have used it for the previous
        // vertex.
        this->leaf->mass_point = this->leaf->mass_point.Zero();

        {   // Build the mass point from max-rank intersections
            int max_rank = 0;
            for (unsigned i=0; i < edge_count; ++i) {
                if (this->leaf->intersections[edges[i]]) {
                    auto r = this->leaf->intersections[edges[i]]->get_rank();
                    if (r > max_rank) {
                        max_rank = r;
                    }
                }
            }

            for (unsigned i=0; i < edge_count; ++i)
            {
                if (this->leaf->intersections[edges[i]] &&
                    this->leaf->intersections[edges[i]]->get_rank() == max_rank)
                {
                    this->leaf->mass_point +=
                        this->leaf->intersections[edges[i]]
                                  ->normalized_mass_point();
                }
            }
        }

        // Now, we'll (pretend to) unpack into A and b matrices,
        // then immediately calculate AtA, AtB, and BtB
        //
        // (Note: this has been moved to the Intersection class, but
        //  the explanation below is still valid)
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
                this->leaf->AtA += this->leaf->intersections[edges[i]]->AtA;
                this->leaf->AtB += this->leaf->intersections[edges[i]]->AtB;
                this->leaf->BtB += this->leaf->intersections[edges[i]]->BtB;
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

void PseudoDCTree2::saveIntersection(const Vec& pos, const Vec& derivs,
                                 const double value, const size_t edge,
                                 Pool& object_pool)
{
    // Just-in-time allocation of intersections array
    if (this->leaf->intersections[edge] == nullptr)
    {
        this->leaf->intersections[edge] = object_pool.next().next().get();
    }
    this->leaf->intersections[edge]->push(pos, derivs, value);
}

uint8_t PseudoDCTree2::buildCornerMask(
        const std::array<Interval::State, 1 <<2>& corners)
{
    uint8_t corner_mask = 0;
    for (unsigned i=0; i < (1 << 2); ++i)
    {
        assert(corners[i] != Interval::UNKNOWN);
        corner_mask |= (corners[i] == Interval::FILLED) << i;
    }
    return corner_mask;
}

void PseudoDCTree2::collectChildren(Evaluator* eval,
                                const Tape::Handle& tape,
                                Pool& object_pool,
                                double max_err)
{
    // Load the children here, to avoid atomics
    std::array<PseudoDCTree2*, 1 <<2 > cs;
    for (unsigned i=0; i < this->children.size(); ++i)
    {
        cs[i] = this->children[i].load(std::memory_order_relaxed);
    }

    // If any children are branches, then we can't collapse.
    // We do this check first, to avoid allocating then freeing a Leaf
    if (std::any_of(cs.begin(), cs.end(),
                    [](PseudoDCTree2* o){ return o->isBranch(); }))
    {
        this->done();
        return;
    }

    // Update corner and filled / empty state from children
    bool all_empty = true;
    bool all_full  = true;
    std::array<Interval::State, 1 <<2> corners;
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

        // this->done will swap us with a singleton, so we can release
        // this tree to the pool right away
        if (this->done()) {
            releaseTo(object_pool);
        }
        return;
    }

    auto corner_mask = buildCornerMask(corners);

    //  This conditional implements the three checks described in
    //  [Ju et al, 2002] in the section titled
    //      "Simplification with topology safety"
    bool manifold = cornersAreManifold(corner_mask) &&
        std::all_of(cs.begin(), cs.end(),
                [](PseudoDCTree2* o){ return o->isManifold(); }) &&
        leafsAreManifold(cs, corners);

    // If we're not manifold, then we can't collapse
    if (!manifold)
    {
        this->done();
        return;
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
            [](unsigned a, PseudoDCTree2* b){ return std::max(a, b->rank());} );

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
#if LIBFIVE_LINEAR_ERROR
        if (findVertex(this->leaf->vertex_count++) < max_err * max_err &&
#else
        if (findVertex(this->leaf->vertex_count++) < max_err &&
#endif
            this->region.contains(vert(0), 1e-6))
        {
            Eigen::Vector3f v;
            v << vert(0).template cast<float>(),
                 this->region.perp.template cast<float>();
            if (fabs(eval->value(v, *tape->getBase(v))) < max_err)
            {
                // Store this tree's depth based on the region's level
                this->leaf->level = this->region.level;

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
}

////////////////////////////////////////////////////////////////////////////////

typename PseudoDCTree2::Vec PseudoDCTree2::vert(unsigned i) const
{
    assert(this->leaf != nullptr);
    assert(i < this->leaf->vertex_count);
    return this->leaf->verts.col(i);
}

Intersection<2>* PseudoDCTree2::intersection(unsigned a, unsigned b) const
{
    assert(MarchingTable<2>::e(a)[b] != -1);
    return intersection(MarchingTable<2>::e(a)[b]);
}

Intersection<2>* PseudoDCTree2::intersection(unsigned edge) const
{
    assert(this->leaf != nullptr);
    return this->leaf->intersections[edge];
}

Interval::State PseudoDCTree2::cornerState(uint8_t i) const
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

bool PseudoDCTree2::isManifold() const
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

uint8_t PseudoDCTree2::cornerMask() const
{
    assert(!this->isBranch());
    switch (this->type)
    {
        case Interval::AMBIGUOUS:
            assert(this->leaf != nullptr);
            return this->leaf->corner_mask;

        case Interval::UNKNOWN: assert(false);

        case Interval::FILLED:  assert(this->leaf == nullptr);
                                return ((1 << (1 << 2)) - 1);
        case Interval::EMPTY:   assert(this->leaf == nullptr);
                                return 0;
    };
    return 0;
}

unsigned PseudoDCTree2::level() const
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

unsigned PseudoDCTree2::rank() const
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

std::array<unsigned, 2 * 2> PseudoDCTree2::edgesFromChild(unsigned childIndex)
{
    std::array<unsigned, 2 * 2> out;
    for (unsigned i=0; i < 2; ++i)
    {
        auto otherCorner = childIndex ^ (1 << i);
        assert(MarchingTable<2>::e(childIndex)[otherCorner] >= 0);
        assert(MarchingTable<2>::e(otherCorner)[childIndex] >= 0);
        out[2 * i] = MarchingTable<2>::e(childIndex)[otherCorner];
        out[2 * i + 1] = MarchingTable<2>::e(otherCorner)[childIndex];
    }
    return out;
}

void PseudoDCTree2::releaseTo(Pool& object_pool) {
    if (this->leaf != nullptr) {
        object_pool.next().put(this->leaf);
        this->leaf = nullptr;
    }
    if (!isSingleton(this)) {
        object_pool.put(this);
    }
}

////////////////////////////////////////////////////////////////////////////////

bool PseudoDCTree2::checkConsistency() const
{
    return checkConsistency(PseudoDCNeighbors2());
}

bool PseudoDCTree2::checkConsistency(const PseudoDCNeighbors2& neighbors) const
{
    //if (this->isBranch()) {
    //    for (unsigned i=0; i < this->children.size(); ++i) {
    //        auto next = neighbors.push(i, this->children);
    //        if (!this->children[i].load()->checkConsistency(next)) {
    //            return false;
    //        }
    //    }
    //} else {
    //    for (unsigned i=0; i < this->children.size(); ++i) {
    //        auto r = neighbors.checkConsistency(i, cornerState(i));
    //        if (r.first != nullptr) {
    //            std::cerr << "Mismatch detected:\n"
    //                << "  Tree A:\n"
    //                << "    [" << this->region.lower.transpose() << "]\n"
    //                << "    [" << this->region.upper.transpose() << "]\n"
    //                << "    type: " << this->type << "\n"
    //                << "    branch: "
    //                    << (this->isBranch() ? "yes" : "no") << "\n"
    //                << "    rank: " << this->rank() << "\n"
    //                << "    corner " << i << " at ["
    //                    << this->region.corner(i).transpose()
    //                    << "]: " << cornerState(i) << "\n"
    //                << "  Tree B:\n"
    //                << "    [" << r.first->region.lower.transpose() << "]\n"
    //                << "    [" << r.first->region.upper.transpose() << "]\n"
    //                << "    type: " << r.first->type << "\n"
    //                << "    branch: "
    //                    << (r.first->isBranch() ? "yes" : "no") << "\n"
    //                << "    rank: " << r.first->rank() << "\n"
    //                << "    corner " << r.second << " at ["
    //                    << r.first->region.corner(r.second).transpose() << "]: "
    //                    << r.first->cornerState(r.second) << "\n";
    //            return false;
    //        }
    //    }
    //}
    return true;
}

bool PseudoDCTree2::cornersAreManifold(const uint8_t corner_mask)
{
    const static bool corner_table[] =
        {1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1};
    return corner_table[corner_mask];
}

////////////////////////////////////////////////////////////////////////////////
// Specializations for quadtree
bool PseudoDCTree2::leafsAreManifold(
        const std::array<PseudoDCTree2*, 1 << 2>& cs,
        const std::array<Interval::State, 1 << 2>& corners)
{
    /*  See detailed comment in Octree::leafsAreManifold */
    const bool edges_safe =
        (cs[0]->cornerState(Axis::X) == corners[0] ||
         cs[0]->cornerState(Axis::X) == corners[Axis::X])
    &&  (cs[0]->cornerState(Axis::Y) == corners[0] ||
         cs[0]->cornerState(Axis::Y) == corners[Axis::Y])
    &&  (cs[Axis::X]->cornerState(Axis::X|Axis::Y) == corners[Axis::X] ||
         cs[Axis::X]->cornerState(Axis::X|Axis::Y) == corners[Axis::X|Axis::Y])
    &&  (cs[Axis::Y]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y] ||
         cs[Axis::Y]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y|Axis::X]);

    const bool faces_safe =
        (cs[0]->cornerState(Axis::Y|Axis::X) == corners[0] ||
         cs[0]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y] ||
         cs[0]->cornerState(Axis::Y|Axis::X) == corners[Axis::X] ||
         cs[0]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y|Axis::X]);

    return edges_safe && faces_safe;
}

}   // namespace libfive

/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <future>
#include <numeric>
#include <cmath>

#include <glm/geometric.hpp>
#include <glm/gtc/random.hpp>
#include <Eigen/Dense>

#include "ao/kernel/render/xtree.hpp"

template <class T, int dims>
T* XTree<T, dims>::Render(Tree* t, const Region& r, uint32_t flags,
                          bool multithread)
{
    auto rp = r.powerOfTwo(dims).view();

    if (multithread && rp.canSplitEven(dims))
    {
        std::list<std::future<T*>> futures;

        // Start up a set of future rendering every branch of the octree
        for (auto region : rp.splitEven(dims))
        {
            auto e = new Evaluator(t);

            futures.push_back(std::async(std::launch::async,
                [e, region, flags](){
                    auto out = new T(e, region, flags);
                    delete e;
                    return out;}));
        }

        // Wait for all of the tasks to finish running in the background
        std::array<T*, 1 << dims> sub;
        int index = 0;
        for (auto& f : futures)
        {
            f.wait();
            sub[index++] = f.get();
        }

        Evaluator e(t);
        return new T(&e, sub, rp, flags);
    }

    else
    {
        Evaluator e(t);
        return new T(&e, rp, flags);
    }
}

////////////////////////////////////////////////////////////////////////////////

template <class T, int dims>
XTree<T, dims>::XTree(const Subregion& r)
    : XTree(r, false)
{
    // Nothing to do here (delegating constructor)
}

template <class T, int dims>
XTree<T, dims>::XTree(const Subregion& r, bool jitter)
    : X(r.X.lower(), r.X.upper()),
      Y(r.Y.lower(), r.Y.upper()),
      Z(r.Z.lower(), r.Z.upper()),
      jitter(jitter)

{
    // Nothing to do here (delegating constructor)
}

template <class T, int dims>
XTree<T, dims>::XTree(Evaluator* e, const Subregion& r, uint32_t flags)
    : XTree(r, !(flags & NO_JITTER))
{
    populateChildren(e, r, flags);
}

template <class T, int dims>
XTree<T, dims>::XTree(const std::array<T*, 1 << dims>& cs, const Subregion& r)
    : XTree(r, false)
{
    for (uint8_t i=0; i < cs.size(); ++i)
    {
        children[i].reset(cs[i]);
    }
    type = BRANCH;
}

template <class T, int dims>
void XTree<T, dims>::populateChildren(Evaluator* e, const Subregion& r,
                                      uint32_t flags)
{
    // The cell is a LEAF cell until proven otherwise
    type = LEAF;

    // If we can recurse, then it may become a BRANCH cell
    if (r.canSplit())
    {
        // First, do interval evaluation to see if the cell should be checked
        Interval out = e->eval(r.X.bounds, r.Y.bounds, r.Z.bounds);
        if (out.upper() < 0)
        {
            type = FULL;
            for (auto& c : corners)
            {
                c = true;
            }
        }
        else if (out.lower() >= 0)
        {
            type = EMPTY;
            for (auto& c : corners)
            {
                c = false;
            }
        }
        else
        {   // If the cell wasn't empty or filled, recurse
            e->push();
            auto rs = r.splitEven(dims);
            for (uint8_t i=0; i < children.size(); ++i)
            {
                children[i].reset(new T(e, rs[i], flags));
            }
            type = BRANCH;
            e->pop();
        }
    }

    // Otherwise, calculate corner values
    if (type == LEAF)
    {
        // Pack into evaluator
        for (uint8_t i=0; i < children.size(); ++i)
        {
            auto c = pos(i);
            e->set(c.x, c.y, c.z, i);
        }

        // Do the evaluation
        const float* fs = e->values(children.size());

        // And unpack from evaluator
        for (uint8_t i=0; i < children.size(); ++i)
        {
            corners[i] = fs[i] < 0;
        }
    }
}

template <class T, int dims>
void XTree<T, dims>::finalize(Evaluator* e, uint32_t flags)
{
    // Find this Octree's level
    level = (type == BRANCH)
        ?  std::accumulate(children.begin(), children.end(), (unsigned)0,
                [](const unsigned& a, const std::unique_ptr<T>& b)
                    { return std::max(a, b->level);} ) + 1
        : 0;

    if (type == BRANCH)
    {
        // Grab corner values from children
        for (uint8_t i=0; i < children.size(); ++i)
        {
            corners[i] = children[i]->corners[i];
        }

        // Collapse branches if the COLLAPSE flag is set
        if (flags & COLLAPSE)
        {
            // This populate the vert member if the branch is collapsed
            // into a LEAF node.
            collapseBranch(e);
        }
    }
    // Always try to convert a LEAF to an EMPTY / FILLED node
    // Otherwise, populate the leaf vertex data
    else if (!collapseLeaf())
    {
        findVertex(e);
    }
}

template <class T, int dims>
void XTree<T, dims>::findIntersections(Evaluator* eval)
{
    // If this is a leaf cell, check every edge and use binary search to
    // find intersections on edges that have mismatched signs
    if (type == LEAF)
    {
        for (auto e : static_cast<T*>(this)->cellEdges())
        {
            if (corner(e.first) != corner(e.second))
            {
                if (corner(e.first))
                {
                    searchEdge(pos(e.first), pos(e.second), eval);
                }
                else
                {
                    searchEdge(pos(e.second), pos(e.first), eval);
                }
            }
        }
    }
    // If this is a branch cell, then accumulate intersections from all the
    // children with the max rank (de-duplicating to avoid weighting
    // intersections incorrectly)
    else if (type == BRANCH)
    {
        // pts stores a list of unique points (within some epsilon)
        std::list<glm::vec3> pts;

        // Compute epsilon from the cell size and edge search count
        const float epsilon = std::max(
                {X.upper() - X.lower(),
                 Y.upper() - Y.lower(),
                 Z.upper() - Z.lower()}) / (4 << SEARCH_COUNT);

        // Find the max rank among children, then only accumulate
        // intersections from children with that rank
        const unsigned max_rank = std::accumulate(
                children.begin(), children.end(), (unsigned)0,
                [](const unsigned& a, const std::unique_ptr<T>& b)
                    { return std::max(a, b->rank);} );

        for (uint8_t i=0; i < children.size(); ++i)
        {
            if (child(i)->rank == max_rank)
            {
                for (auto n : child(i)->intersections)
                {
                    // Only store this intersection point if it hasn't
                    // been stored already (to a given epsilon of
                    // floating-point error)
                    if (!std::any_of(pts.begin(), pts.end(),
                            [&](glm::vec3 p)
                            { return glm::length(n.pos - p) < epsilon; }))
                    {
                        pts.push_back(n.pos);
                        intersections.push_back(n);
                    }
                    mass_point += child(i)->mass_point;
                }
            }
        }
    }
}

template <class T, int dims>
void XTree<T, dims>::collapseBranch(Evaluator* e)
{
    bool all_empty = true;
    bool all_full  = true;
    bool collapsible = true;

    for (const auto& c : children)
    {
        all_empty   &= c->type == EMPTY;
        all_full    &= c->type == FULL;
        collapsible &= c->type != BRANCH;
    }

    if (all_empty)
    {
        type = EMPTY;
    }
    else if (all_full)
    {
        type = FULL;
    }
    else if (collapsible)
    {
        //  This conditional implements the three checks described in
        //  [Ju et al, 2002] in the section titled
        //      "Simplification with topology safety"
        if (this->cornerTopology() &&
            std::all_of(children.begin(), children.end(),
                    [](const std::unique_ptr<T>& o)
                    { return o->manifold; }) &&
            static_cast<T*>(this)->leafTopology() && findVertex(e) < 1e-8)
        {
            type = LEAF;
        }
    }

    // If this cell is no longer a branch, remove its children
    if (type != BRANCH)
    {
        std::for_each(children.begin(), children.end(),
            [](std::unique_ptr<T>& o) { o.reset(); });
    }
}

template <class T, int dims>
bool XTree<T, dims>::collapseLeaf()
{
    bool filled = corners[0];
    for (auto c : corners)
    {
        if (c != filled)
        {
            return false;
        }
    }

    type = filled ? FULL : EMPTY;
    return true;
}

template <class T, int dims>
float XTree<T, dims>::findVertex(Evaluator* e)
{
    findIntersections(e);

    // Find the center of intersection positions
    glm::vec3 center = glm::vec3(mass_point.x, mass_point.y, mass_point.z) /
                       mass_point.w;

    // For non-manifold leaf nodes, put the vertex at the mass point.
    // As described in "Dual Contouring: The Secret Sauce", this improves
    // mesh quality.
    if (type == LEAF)
    {
        manifold = this->cornerTopology();
        if (!manifold)
        {
            vert = center;
            return std::numeric_limits<float>::infinity();
        }
    }


    /*  The A matrix is of the form
     *  [n1x, n1y, n1z]
     *  [n2x, n2y, n2z]
     *  [n3x, n3y, n3z]
     *  ...
     *  (with one row for each Hermite intersection)
     */
    Eigen::MatrixX3f A(intersections.size(), 3);
    for (unsigned i=0; i < intersections.size(); ++i)
    {
        auto d = intersections[i].norm;
        A.row(i) << Eigen::Vector3f(d.x, d.y, d.z).transpose();
    }

    /*  The B matrix is of the form
     *  [p1 . n1]
     *  [p2 . n2]
     *  [p3 . n3]
     *  ...
     *  (with one row for each Hermite intersection)
     *
     *  Positions are pre-emtively shifted so that the center of the contoru
     *  is at 0, 0, 0 (since the least-squares fix minimizes distance to the
     *  origin); we'll unshift afterwards.
     */
    Eigen::VectorXf B(intersections.size(), 1);
    for (unsigned i=0; i < intersections.size(); ++i)
    {
        B.row(i) << glm::dot(intersections[i].norm,
                             intersections[i].pos - center);
    }

    // Use singular value decomposition to solve the least-squares fit.
    Eigen::JacobiSVD<Eigen::MatrixX3f> svd(A, Eigen::ComputeFullU |
                                              Eigen::ComputeFullV);

    // Truncate singular values below 0.1
    auto singular = svd.singularValues();
    svd.setThreshold(0.1 / singular.maxCoeff());
    rank = svd.rank();

    // Solve the equation and convert back to cell coordinates
    Eigen::Vector3f solution = svd.solve(B);
    vert = glm::vec3(solution.x(), solution.y(), solution.z()) + center;

    // Clamp vertex to be within the bounding box
    vert.x = std::min(X.upper(), std::max(vert.x, X.lower()));
    vert.y = std::min(Y.upper(), std::max(vert.y, Y.lower()));
    vert.z = std::min(Z.upper(), std::max(vert.z, Z.lower()));

    // Find and return QEF residual
    auto m = A * solution - B;
    return m.transpose() * m;
}


template <class T, int dims>
bool XTree<T, dims>::cornerTopology() const
{
    uint8_t index = 0;
    for (uint8_t i=0; i < children.size(); ++i)
    {
        if (corners[i])
        {
            index |= (1 << i);
        }
    }

    return static_cast<const T*>(this)->cornerTable()[index];
}

template<class T, int dims>
void XTree<T, dims>::searchEdge(glm::vec3 a, glm::vec3 b, Evaluator* e)
{
    // We do an N-fold reduction at each stage
    constexpr int _N = 4;
    constexpr int N = (1 << _N);
    constexpr int ITER = SEARCH_COUNT / _N;

    const float len = glm::length(a - b);

    // Binary search for intersection
    for (int i=0; i < ITER; ++i)
    {
        glm::vec3 ps[N];
        for (int j=0; j < N; ++j)
        {
            float frac = j / (N - 1.0);
            ps[j] = (a * (1 - frac)) + (b * frac);
            e->setRaw(ps[j].x, ps[j].y, ps[j].z, j);
        }

        auto out = e->values(N);
        for (int j=0; j < N; ++j)
        {
            if (out[j] >= 0)
            {
                a = ps[j - 1];
                b = ps[j];
                break;
            }
        }
    }

    // pos is an array of positions to get normals for
    // If jitter is disabled, it only contains the intersection
    std::vector<glm::vec3> pos = {a};

    // If jitter is enabled, add a cloud of nearby points
    if (jitter)
    {
        static_assert(JITTER_COUNT < Result::N, "JITTER_COUNT is too large");

        const float r = len / 10.0f;
        while (pos.size() < JITTER_COUNT)
        {
            if (dims == 3)
            {
                pos.push_back(a + glm::sphericalRand(r));
            }
            else if (dims == 2)
            {
                pos.push_back(a + glm::vec3(glm::circularRand(r), 0.0f));
            }
        }
    }

    size_t count = 0;
    for (auto p : pos)
    {
        e->setRaw(p.x, p.y, p.z, count++);
        mass_point += glm::vec4(p.x, p.y, p.z, 1.0f);
    }

    // Get set of derivative arrays
    auto ds = e->derivs(count);

    // Extract gradient from set of arrays
    for (unsigned i=0; i < count; ++i)
    {
        glm::vec3 g(std::get<1>(ds)[i], std::get<2>(ds)[i], std::get<3>(ds)[i]);
        intersections.push_back({pos[i], glm::normalize(g)});
    }
}

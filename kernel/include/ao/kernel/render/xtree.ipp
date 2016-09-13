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
    : X(r.X.lower(), r.X.upper()),
      Y(r.Y.lower(), r.Y.upper()),
      Z(r.Z.lower(), r.Z.upper())
{
    // Nothing to do here (delegating constructor)
}

template <class T, int dims>
XTree<T, dims>::XTree(Evaluator* e, const Subregion& r, uint32_t flags)
    : XTree(r)
{
    populateChildren(e, r, flags);
}

template <class T, int dims>
XTree<T, dims>::XTree(const std::array<T*, 1 << dims>& cs, const Subregion& r)
    : XTree(r)
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
            collapseBranch();
        }
    }
    // Always try to convert a LEAF to an EMPTY / FILLED node
    // Otherwise, populate the leaf vertex data
    else if (!collapseLeaf())
    {
        findLeafMatrices(e);
        findVertex();
    }
}

template <class T, int dims>
void XTree<T, dims>::findIntersections(Evaluator* eval)
{
    assert(type == LEAF);

    // If this is a leaf cell, check every edge and use binary search to
    // find intersections on edges that have mismatched signs
    for (auto e : static_cast<T*>(this)->cellEdges())
    {
        if (corner(e.first) != corner(e.second))
        {
            Intersection i = (corner(e.first))
                ? searchEdge(pos(e.first), pos(e.second), eval)
                : searchEdge(pos(e.second), pos(e.first), eval);
            intersections.push_back(i);
        }
    }
}


template <class T, int dims>
void XTree<T, dims>::findBranchMatrices()
{
    // Find the max rank among children, then only accumulate
    // intersections from children with that rank
    rank = std::accumulate(
            children.begin(), children.end(), (unsigned)0,
            [](const unsigned& a, const std::unique_ptr<T>& b)
                { return std::max(a, b->rank);} );

    for (const auto& c : children)
    {
        if (c->rank == rank)
        {
            mass_point += c->mass_point;
        }
        AtA += c->AtA;
        AtB += c->AtB;
        BtB += c->BtB;
    }
}

template <class T, int dims>
void XTree<T, dims>::collapseBranch()
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
            static_cast<T*>(this)->leafTopology())
        {
            findBranchMatrices();
            if (findVertex() < 1e-8)
            {
                type = LEAF;
            }
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
void XTree<T, dims>::findLeafMatrices(Evaluator* e)
{
    findIntersections(e);

    /*  The A matrix is of the form
     *  [n1x, n1y, n1z]
     *  [n2x, n2y, n2z]
     *  [n3x, n3y, n3z]
     *  ...
     *  (with one row for each Hermite intersection)
     */
    Eigen::MatrixX3d A(intersections.size(), 3);

    /*  The B matrix is of the form
     *  [p1 . n1]
     *  [p2 . n2]
     *  [p3 . n3]
     *  ...
     *  (with one row for each Hermite intersection)
     */
    Eigen::VectorXd B(intersections.size(), 1);
    for (unsigned i=0; i < intersections.size(); ++i)
    {
        auto d = intersections[i].norm;
        A.row(i) << Eigen::Vector3d(d.x, d.y, d.z).transpose();
        B.row(i) << glm::dot(intersections[i].norm, intersections[i].pos);
    }

    auto At = A.transpose();

    AtA = At * A;
    AtB = At * B;
    BtB = B.transpose() * B;

    // Only find Eigenvalues (this is to calculate rank)
    Eigen::EigenSolver<Eigen::Matrix3d> es(AtA, false);
    auto eigenvalues = es.eigenvalues().real();
    double s_max = eigenvalues.cwiseAbs().maxCoeff();

    // Truncate near-singular eigenvalues
    rank = 3;
    for (unsigned i=0; i < 3; ++i)
    {
        if (std::abs(eigenvalues[i]) / s_max < 0.1)
        {
            rank--;
        }
    }
}

template <class T, int dims>
float XTree<T, dims>::findVertex()
{
    // For non-manifold leaf nodes, put the vertex at the mass point.
    // As described in "Dual Contouring: The Secret Sauce", this improves
    // mesh quality.
    if (type == LEAF)
    {
        manifold = this->cornerTopology();
        if (!manifold)
        {
            vert = glm::vec3(mass_point.x, mass_point.y, mass_point.z) /
                   mass_point.w;
            return std::numeric_limits<float>::infinity();
        }
    }

    // We need to find the pseudo-inverse of AtA.
    Eigen::EigenSolver<Eigen::Matrix3d> es(AtA);
    auto eigenvalues = es.eigenvalues().real();
    double s_max = eigenvalues.cwiseAbs().maxCoeff();

    // Truncate near-singular eigenvalues in the SVD's diagonal matrix
    Eigen::Matrix3d D(Eigen::Matrix3d::Zero());
    for (unsigned i=0; i < 3; ++i)
    {
        D.diagonal()[i] = (std::abs(eigenvalues[i]) / s_max < 0.1)
            ? 0 : (1 / eigenvalues[i]);
    }

    // SVD matrices
    auto U = es.eigenvectors().real(); // = V

    // Pseudo-inverse of A
    auto AtAp = U * D * U.transpose();

    // Solve for vertex (minimizing distance to center)
    auto p = Eigen::Vector3d(mass_point.x, mass_point.y, mass_point.z) /
             mass_point.w;
    auto v = AtAp * (AtB - AtA * p) + p;

    // Convert out of Eigen's format and store
    vert = glm::vec3(v[0], v[1], v[2]);

    float err = (v.transpose() * AtA * v - 2*v.transpose() * AtB)[0] + BtB;
    return err;
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
Intersection XTree<T, dims>::searchEdge(glm::vec3 a, glm::vec3 b, Evaluator* e)
{
    // We do an N-fold reduction at each stage
    constexpr int _N = 4;
    constexpr int N = (1 << _N);
    constexpr int ITER = SEARCH_COUNT / _N;

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

    // Accumulate intersection in mass point
    mass_point += glm::vec4(a, 1.0f);

    // Get derivatives
    e->setRaw(a.x, a.y, a.z, 0);
    auto ds = e->derivs(1);

    // Extract gradient from normal arrays
    glm::vec3 g(std::get<1>(ds)[0], std::get<2>(ds)[0], std::get<3>(ds)[0]);
    return {a, glm::normalize(g)};
}

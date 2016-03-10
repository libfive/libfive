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
#include <iostream>
#include <numeric>
#include <set>
#include <future>

#include <Eigen/Dense>
#include <glm/geometric.hpp>

#include "ao/kernel/render/octree.hpp"
#include "ao/kernel/render/region.hpp"

#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/tree/tree.hpp"

Octree::Octree(const Subregion& r)
    : X(r.X.lower(), r.X.upper()),
      Y(r.Y.lower(), r.Y.upper()),
      Z(r.Z.lower(), r.Z.upper())
{
    // Nothing to do here
}

Octree::Octree(Evaluator* e, const Subregion& r, uint32_t flags)
    : Octree(r)
{
    populateChildren(e, r, flags);
    finalize(e, flags);
}

Octree::Octree(Evaluator* e, const std::array<Octree*, 8>& cs,
               const Subregion& r, uint32_t flags)
    : Octree(r)
{
    for (uint8_t i=0; i < 8; ++i)
    {
        children[i].reset(cs[i]);
    }
    type = BRANCH;
    finalize(e, flags);
}

void Octree::finalize(Evaluator* e, uint32_t flags)
{
    findIntersections(e);

    // Find this Octree's level
    level = (type == BRANCH)
        ?  std::accumulate(children.begin(), children.end(), (unsigned)0,
                [](const unsigned& a, const std::unique_ptr<Octree>& b)
                    { return std::max(a, b->level);} ) + 1
        : 0;

    if (type == BRANCH)
    {
        // Grab corner values from children
        for (uint8_t i=0; i < 8; ++i)
        {
            corners[i] = children[i]->corners[i];
        }

        // Collapse branches if the COLLAPSE flag is set
        if (flags & COLLAPSE)
        {
            collapseBranch();
        }
    }
    else
    {
        // Always convert leafs to empty / filled cells
        collapseLeaf();
    }

    if (type == LEAF && std::isnan(vert.x))
    {
        findVertex();
    }
}
////////////////////////////////////////////////////////////////////////////////

void Octree::populateChildren(Evaluator* e, const Subregion& r,
                              uint32_t flags)
{
    // Subdivide and recurse if possible
    if (r.canSplit())
    {
        auto rs = r.octsect();
        for (uint8_t i=0; i < 8; ++i)
        {
            children[i].reset(new Octree(e, rs[i], flags));
        }
        type = BRANCH;
    }
    // Otherwise, calculate corner values
    else
    {
        for (uint8_t i=0; i < 8; ++i)
        {
            auto c = pos(i);
            e->set(c.x, c.y, c.z, i);
        }
        const float* fs = e->values(8);
        for (uint8_t i=0; i < 8; ++i)
        {
            corners[i] = fs[i] < 0;
        }
        type = LEAF;
    }
}

void Octree::collapseBranch()
{
    // If all of the children are leafs, then collapse into a single LEAF cell
    if (std::all_of(children.begin(), children.end(),
            [](std::unique_ptr<Octree>& o){ return o->type == EMPTY; }))
    {
        type = EMPTY;
    }
    // If all of the children are full, then collapse into a single FULL cell
    else if (std::all_of(children.begin(), children.end(),
            [](std::unique_ptr<Octree>& o){ return o->type == FULL; }))
    {
        type = FULL;
    }
    // If all of the children are non-branches, then check to see whether we
    // can collapse into a single LEAF cell.
    else if (std::all_of(children.begin(), children.end(),
            [](std::unique_ptr<Octree>& o){ return o->type != BRANCH; }))
    {
        //  This conditional implements the three checks described in
        //  [Ju et al, 2002] in the section titled
        //      "Simplification with topology safety"
        if (cornerTopology() && std::all_of(children.begin(), children.end(),
                    [](const std::unique_ptr<Octree>& o)
                    { return o->cornerTopology(); }) &&
            leafTopology() && findVertex() < 1e-8)
        {
            type = LEAF;
        }
    }

    // If this cell is no longer a branch, remove its children
    if (type != BRANCH)
    {
        std::for_each(children.begin(), children.end(),
            [](std::unique_ptr<Octree>& o) { o.reset(); });
    }
}

void Octree::collapseLeaf()
{
    if (std::all_of(corners.begin(), corners.end(),
            [](bool c){ return !c; }))
    {
        type = EMPTY;
    }
    else if (std::all_of(corners.begin(), corners.end(),
            [](bool c){ return c; }))
    {
        type = FULL;
    }
}

glm::vec3 Octree::pos(uint8_t i) const
{
    return {i & AXIS_X ? X.upper() : X.lower(),
            i & AXIS_Y ? Y.upper() : Y.lower(),
            i & AXIS_Z ? Z.upper() : Z.lower()};
}

Octree* Octree::Render(Tree* t, const Region& r, uint32_t flags,
                       bool multithread)
{
    auto rp = r.powerOfTwo().view();

    if (multithread && rp.canOctsect())
    {
        std::list<std::future<Octree*>> futures;

        // Start up a set of future rendering every branch of the octree
        for (auto region : rp.octsect())
        {
            auto e = new Evaluator(t);

            futures.push_back(std::async(std::launch::async,
                [e, region, flags](){
                    auto out = new Octree(e, region, flags);
                    delete e;
                    return out;}));
        }

        // Wait for all of the tasks to finish running in the background
        std::array<Octree*, 8> sub;
        int index = 0;
        for (auto& f : futures)
        {
            f.wait();
            sub[index++] = f.get();
        }

        Evaluator e(t);
        return new Octree(&e, sub, rp, flags);
    }

    else
    {
        Evaluator e(t);
        return new Octree(&e, rp, flags);
    }
}

////////////////////////////////////////////////////////////////////////////////

// These are the twelve edges of an octree cell
const std::pair<unsigned, unsigned> Octree::cellEdges[12] =
    {{0, AXIS_X}, {0, AXIS_Y}, {0, AXIS_Z},
     {AXIS_X, AXIS_X|AXIS_Y}, {AXIS_X, AXIS_X|AXIS_Z},
     {AXIS_Y, AXIS_Y|AXIS_X}, {AXIS_Y, AXIS_Y|AXIS_Z},
     {AXIS_X|AXIS_Y, AXIS_X|AXIS_Y|AXIS_Z},
     {AXIS_Z, AXIS_Z|AXIS_X}, {AXIS_Z, AXIS_Z|AXIS_Y},
     {AXIS_Z|AXIS_X, AXIS_Z|AXIS_X|AXIS_Y},
     {AXIS_Z|AXIS_Y, AXIS_Z|AXIS_Y|AXIS_X}};


Octree::Intersection Octree::searchEdge(
        glm::vec3 a, glm::vec3 b, Evaluator* eval)
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
            eval->set(ps[j].x, ps[j].y, ps[j].z, j);
        }

        auto out = eval->values(N);
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

    // Calculate value and gradient at the given point
    eval->set(a.x, a.y, a.z, 0);

    // Get set of derivative arrays
    auto ds = eval->derivs(1);

    // Extract gradient from set of arrays
    glm::vec3 g(std::get<1>(ds)[0], std::get<2>(ds)[0], std::get<3>(ds)[0]);

    return {a, glm::normalize(g)};
}

void Octree::findIntersections(Evaluator* eval)
{
    // If this is a leaf cell, check every edge and use binary search to
    // find intersections on edges that have mismatched signs
    if (type == LEAF)
    {
        for (auto e : cellEdges)
        {
            if (corner(e.first) != corner(e.second))
            {
                if (corner(e.first))
                {
                    intersections.push_back(
                            searchEdge(pos(e.first), pos(e.second), eval));
                }
                else
                {
                    intersections.push_back(
                        searchEdge(pos(e.second), pos(e.first), eval));
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
                [](const unsigned& a, const std::unique_ptr<Octree>& b)
                    { return std::max(a, b->rank);} );

        for (uint8_t i=0; i < 8; ++i)
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
                }
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

/*
 *  Vertex positioning is based on [Kobbelt et al, 2001]
 */
float Octree::findVertex()
{
    // Find the center of intersection positions
    glm::vec3 center = std::accumulate(
            intersections.begin(), intersections.end(), glm::vec3(),
            [](const glm::vec3& a, const Intersection& b)
                { return a + b.pos; }) / float(intersections.size());

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

////////////////////////////////////////////////////////////////////////////////

bool Octree::cornerTopology() const
{
    /* Implements the test from [Gerstner et al, 2000],
     * as described in [Ju et al, 2002].
     *
     * The code to generate the table is given below:

def safe(index):
    f = [(index & (1 << i)) != 0 for i in range(8)]
    edges = [(0,1), (0,2), (2,3), (1,3),
             (4,5), (4,6), (6,7), (5,7),
             (0,4), (2,6), (1,5), (3,7)]

    def merge(a, b):
        merged = [(e[0] if e[0] != a else b,
                   e[1] if e[1] != a else b) for e in edges]
        return [e for e in merged if e[0] != e[1]]

    while True:
        for e in edges:
            if f[e[0]] == f[e[1]]:
                edges = merge(e[0], e[1])
                break
        else:
            break
    s = set(map(lambda t: tuple(sorted(t)),edges))
    return len(s) <= 1

out = ""
for i,s in enumerate([safe(i) for i in range(256)]):
    if out == "": out += "{"
    else: out += ","

    if i and i % 32 == 0:
        out += '\n '

    if s: out += "1"
    else: out += "0"
out += "}"
print(out)

    */
    static std::vector<bool> corner_table =
        {1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,0,1,0,1,0,1,0,0,0,1,0,1,0,1,
         1,0,1,1,0,0,0,1,0,0,1,1,0,0,1,1,1,1,1,1,0,1,0,1,0,0,1,1,0,0,0,1,
         1,0,0,0,1,1,0,1,0,0,0,0,1,1,1,1,1,1,0,1,1,1,0,1,0,0,0,0,1,1,0,1,
         0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,1,0,0,0,0,0,0,0,1,
         1,0,0,0,0,0,0,0,1,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
         1,0,1,1,0,0,0,0,1,0,1,1,1,0,1,1,1,1,1,1,0,0,0,0,1,0,1,1,0,0,0,1,
         1,0,0,0,1,1,0,0,1,0,1,0,1,1,1,1,1,1,0,0,1,1,0,0,1,0,0,0,1,1,0,1,
         1,0,1,0,1,0,0,0,1,0,1,0,1,0,1,1,1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1};

    uint8_t index = 0;
    for (uint8_t i=0; i < 8; ++i)
    {
        if (corners[i])
        {
            index |= (1 << i);
        }
    }

    return corner_table[index];
}

bool Octree::leafTopology() const
{
    /*  - The sign in the middle of a coarse edge must agree with the sign of at
     *    least one of the edge’s two endpoints.
     *  - The sign in the middle of a coarse face must agree with the sign of at
     *    least one of the face’s four corners.
     *  - The sign in the middle of a coarse cube must agree with the sign of at
     *    least one of the cube’s eight corners.
     *  [Ju et al, 2002]    */

    // Check the signs in the middle of leaf cell edges
    const bool edges_safe =
        (child(0)->corner(AXIS_Z) == corner(0) ||
         child(0)->corner(AXIS_Z) == corner(AXIS_Z))
    &&  (child(0)->corner(AXIS_X) == corner(0) ||
         child(0)->corner(AXIS_X) == corner(AXIS_X))
    &&  (child(0)->corner(AXIS_Y) == corner(0) ||
         child(0)->corner(AXIS_Y) == corner(AXIS_Y))

    &&  (child(AXIS_X)->corner(AXIS_X|AXIS_Y) == corner(AXIS_X) ||
         child(AXIS_X)->corner(AXIS_X|AXIS_Y) == corner(AXIS_X|AXIS_Y))
    &&  (child(AXIS_X)->corner(AXIS_X|AXIS_Z) == corner(AXIS_X) ||
         child(AXIS_X)->corner(AXIS_X|AXIS_Z) == corner(AXIS_X|AXIS_Z))

    &&  (child(AXIS_Y)->corner(AXIS_Y|AXIS_X) == corner(AXIS_Y) ||
         child(AXIS_Y)->corner(AXIS_Y|AXIS_X) == corner(AXIS_Y|AXIS_X))
    &&  (child(AXIS_Y)->corner(AXIS_Y|AXIS_Z) == corner(AXIS_Y) ||
         child(AXIS_Y)->corner(AXIS_Y|AXIS_Z) == corner(AXIS_Y|AXIS_Z))

    &&  (child(AXIS_X|AXIS_Y)->corner(AXIS_X|AXIS_Y|AXIS_Z) ==
                               corner(AXIS_X|AXIS_Y) ||
         child(AXIS_X|AXIS_Y)->corner(AXIS_X|AXIS_Y|AXIS_Z) ==
                               corner(AXIS_X|AXIS_Y|AXIS_Z))

    &&  (child(AXIS_Z)->corner(AXIS_Z|AXIS_X) == corner(AXIS_Z) ||
         child(AXIS_Z)->corner(AXIS_Z|AXIS_X) == corner(AXIS_Z|AXIS_X))
    &&  (child(AXIS_Z)->corner(AXIS_Z|AXIS_Y) == corner(AXIS_Z) ||
         child(AXIS_Z)->corner(AXIS_Z|AXIS_Y) == corner(AXIS_Z|AXIS_Y))

    &&  (child(AXIS_Z|AXIS_X)->corner(AXIS_Z|AXIS_X|AXIS_Y) ==
                               corner(AXIS_Z|AXIS_X) ||
         child(AXIS_Z|AXIS_X)->corner(AXIS_Z|AXIS_X|AXIS_Y) ==
                               corner(AXIS_Z|AXIS_X|AXIS_Y))

    &&  (child(AXIS_Z|AXIS_Y)->corner(AXIS_Z|AXIS_Y|AXIS_X) ==
                               corner(AXIS_Z|AXIS_Y) ||
         child(AXIS_Z|AXIS_Y)->corner(AXIS_Z|AXIS_Y|AXIS_X) ==
                               corner(AXIS_Z|AXIS_Y|AXIS_X));

    const bool faces_safe =
        (child(0)->corner(AXIS_X|AXIS_Z) == corner(0) ||
         child(0)->corner(AXIS_X|AXIS_Z) == corner(AXIS_X) ||
         child(0)->corner(AXIS_X|AXIS_Z) == corner(AXIS_Z) ||
         child(0)->corner(AXIS_X|AXIS_Z) == corner(AXIS_X|AXIS_Z))
    &&  (child(0)->corner(AXIS_Y|AXIS_Z) == corner(0) ||
         child(0)->corner(AXIS_Y|AXIS_Z) == corner(AXIS_Y) ||
         child(0)->corner(AXIS_Y|AXIS_Z) == corner(AXIS_Z) ||
         child(0)->corner(AXIS_Y|AXIS_Z) == corner(AXIS_Y|AXIS_Z))
    &&  (child(0)->corner(AXIS_Y|AXIS_X) == corner(0) ||
         child(0)->corner(AXIS_Y|AXIS_X) == corner(AXIS_Y) ||
         child(0)->corner(AXIS_Y|AXIS_X) == corner(AXIS_X) ||
         child(0)->corner(AXIS_Y|AXIS_X) == corner(AXIS_Y|AXIS_X))

    && (child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_X) == corner(AXIS_X) ||
        child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_X) == corner(AXIS_X|AXIS_Z) ||
        child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_X) == corner(AXIS_X|AXIS_Y) ||
        child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_X) ==
                                     corner(AXIS_X|AXIS_Y|AXIS_Z))
    && (child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_Y) == corner(AXIS_Y) ||
        child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_Y) == corner(AXIS_Y|AXIS_Z) ||
        child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_Y) == corner(AXIS_Y|AXIS_X) ||
        child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_Y) ==
                                     corner(AXIS_Y|AXIS_Z|AXIS_X))
    && (child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_Z) == corner(AXIS_Z) ||
        child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_Z) == corner(AXIS_Z|AXIS_Y) ||
        child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_Z) == corner(AXIS_Z|AXIS_X) ||
        child(AXIS_X|AXIS_Y|AXIS_Z)->corner(AXIS_Z) ==
                                     corner(AXIS_Z|AXIS_Y|AXIS_X));

    const bool center_safe =
        child(0)->corner(AXIS_X|AXIS_Y|AXIS_Z) == corner(0) ||
        child(0)->corner(AXIS_X|AXIS_Y|AXIS_Z) == corner(AXIS_X) ||
        child(0)->corner(AXIS_X|AXIS_Y|AXIS_Z) == corner(AXIS_Y) ||
        child(0)->corner(AXIS_X|AXIS_Y|AXIS_Z) == corner(AXIS_X|AXIS_Y) ||
        child(0)->corner(AXIS_X|AXIS_Y|AXIS_Z) == corner(AXIS_Z) ||
        child(0)->corner(AXIS_X|AXIS_Y|AXIS_Z) == corner(AXIS_Z|AXIS_X) ||
        child(0)->corner(AXIS_X|AXIS_Y|AXIS_Z) == corner(AXIS_Z|AXIS_Y) ||
        child(0)->corner(AXIS_X|AXIS_Y|AXIS_Z) == corner(AXIS_Z|AXIS_X|AXIS_Y);

    return edges_safe && faces_safe && center_safe;
}

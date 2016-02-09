#include <iostream>
#include <numeric>
#include <set>

#include <Eigen/Dense>
#include <glm/geometric.hpp>

#include "ao/kernel/render/octree.hpp"
#include "ao/kernel/render/region.hpp"

#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/tree/tree.hpp"

Octree::Octree(Evaluator* e, const Subregion& r)
    : X(r.X.lower(), r.X.upper()),
      Y(r.Y.lower(), r.Y.upper()),
      Z(r.Z.lower(), r.Z.upper())
{
    populateChildren(e, r);
    findIntersections(e);

    // Find this Octree's level
    level = (type == BRANCH)
        ?  std::accumulate(children.begin(), children.end(), (unsigned)0,
                [](const unsigned& a, const std::unique_ptr<Octree>& b)
                    { return std::max(a, b->level);} ) + 1
        : 0;

    if (type == BRANCH)
    {
        collapseBranch();
    }
    else
    {
        collapseLeaf();
    }

    if (type == LEAF && std::isnan(vert.x))
    {
        findVertex();
    }
}

void Octree::populateChildren(Evaluator* e, const Subregion& r)
{
    // Subdivide and recurse if possible
    if (r.canSplit())
    {
        auto rs = r.octsect();
        for (uint8_t i=0; i < 8; ++i)
        {
            children[i].reset(new Octree(e, rs[i]));
            corners[i] = children[i]->corners[i];
        }
        type = BRANCH;
    }
    // Otherwise, calculate corner values
    else
    {
        for (uint8_t i=0; i < 8; ++i)
        {
            auto c = pos(i);
            e->setPoint<float>(c.x, c.y, c.z, i);
        }
        const float* fs = e->evalCore<float>(8);
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

Octree* Octree::Render(Tree* t, const Region& r)
{
    Evaluator e(t);
    return new Octree(&e, r.powerOfTwo());
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
    auto p = (a + b) / 2.0f;
    auto d = (a - b) / 4.0f;

    // Binary search for intersection
    for (int i=0; i < SEARCH_COUNT; ++i)
    {
        if (eval->eval(p.x, p.y, p.z) < 0)
        {
            p -= d;
        }
        else
        {
            p += d;
        }
        d /= 2;
    }

    // Calculate value and gradient at the given point
    auto g = eval->eval(Gradient(p.x, {1, 0, 0}),
                        Gradient(p.y, {0, 1, 0}),
                        Gradient(p.z, {0, 0, 1}));

    return {p, glm::normalize(g.d)};
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
     * The lookup-table is generated by util/topology.py    */
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

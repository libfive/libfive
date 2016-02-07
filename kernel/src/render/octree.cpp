#include <set>
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
        collapseBranch();
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
        collapseLeaf();
    }
}

void Octree::collapseBranch()
{
    bool empty = true;
    bool full = true;

    for (uint8_t i=0; i < 8; ++i)
    {
        empty &= (children[i]->type == EMPTY);
        full  &= (children[i]->type == FULL);
    }

    if (empty || full)
    {
        assert(empty != full);

        for (uint8_t i=0; i < 8; ++i)
        {
            children[i].reset();
        }
        type = empty ? EMPTY : FULL;
    }
}

void Octree::collapseLeaf()
{
    bool empty = true;
    bool full = true;

    for (uint8_t i=0; i < 8; ++i)
    {
        empty &= !corners[i];
        full  &=  corners[i];
    }

    if (empty || full)
    {
        assert(empty != full);
        type = empty ? EMPTY : FULL;
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
    {{0, Octree::AXIS_X}, {0, Octree::AXIS_Y}, {0, Octree::AXIS_Z},
     {Octree::AXIS_X, Octree::AXIS_X|Octree::AXIS_Y},
     {Octree::AXIS_X, Octree::AXIS_X|Octree::AXIS_Z},
     {Octree::AXIS_Y, Octree::AXIS_Y|Octree::AXIS_X},
     {Octree::AXIS_Y, Octree::AXIS_Y|Octree::AXIS_Z},
     {Octree::AXIS_X|Octree::AXIS_Y,
      Octree::AXIS_X|Octree::AXIS_Y|Octree::AXIS_Z},
     {Octree::AXIS_Z, Octree::AXIS_Z|Octree::AXIS_X},
     {Octree::AXIS_Z, Octree::AXIS_Z|Octree::AXIS_Y},
     {Octree::AXIS_Z|Octree::AXIS_X,
      Octree::AXIS_Z|Octree::AXIS_X|Octree::AXIS_Y},
     {Octree::AXIS_Z|Octree::AXIS_Y,
      Octree::AXIS_Z|Octree::AXIS_Y|Octree::AXIS_X}};

// Performs binary search between two points
// eval(a) should be < 0 (inside the shape) and eval(b) should be outside
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

    return {p, eval->eval(Gradient(p.x, 1, 0, 0),
                          Gradient(p.y, 0, 1, 0),
                          Gradient(p.z, 0, 0, 1))};
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
    // children (de-duplicating to avoid weighting intersections incorrectly)
    else if (type == BRANCH)
    {
        // pts stores a list of unique points (within some epsilon)
        std::list<glm::vec3> pts;

        // Compute epsilon from the cell size and edge search count
        const float epsilon = std::max(
                {X.upper() - X.lower(),
                 Y.upper() - Y.lower(),
                 Z.upper() - Z.lower()}) / (4 << SEARCH_COUNT);

        for (uint8_t i=0; i < 8; ++i)
        {
            for (auto n : child(i)->intersections)
            {
                // Only store this intersection point if it hasn't been stored
                // already (to a given epsilon of floating-point error)
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


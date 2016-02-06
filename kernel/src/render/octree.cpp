#include "ao/kernel/render/octree.hpp"
#include "ao/kernel/render/region.hpp"

#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/tree/tree.hpp"

Octree::Octree(Evaluator* e, const Subregion& r)
    : X(r.X.lower(), r.X.upper()),
      Y(r.Y.lower(), r.Y.upper()),
      Z(r.Z.lower(), r.Z.upper()),
      type(populateChildren(e, r)),
      intersections(findIntersections(e))
{
    // Nothing to do here
}

Octree::Type Octree::populateChildren(Evaluator* e, const Subregion& r)
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
        return collapseBranch();
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
        return collapseLeaf();
    }
}

Octree::Type Octree::collapseBranch()
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
        return empty ? EMPTY : FULL;
    }
    return BRANCH;
}

Octree::Type Octree::collapseLeaf()
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
        return empty ? EMPTY : FULL;
    }
    return LEAF;
}

Octree* Octree::Render(Tree* t, const Region& r)
{
    Evaluator e(t);
    return new Octree(&e, r.powerOfTwo());
}

glm::vec3 Octree::pos(uint8_t i) const
{
    return {i & AXIS_X ? X.upper() : X.lower(),
            i & AXIS_Y ? Y.upper() : Y.lower(),
            i & AXIS_Z ? Z.upper() : Z.lower()};
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
    for (int i=0; i < 8; ++i)
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

std::vector<Octree::Intersection> Octree::findIntersections(
        Evaluator* eval) const
{
    std::vector<Intersection> intersections;
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
    else if (type == BRANCH)
    {
        // Accumulate intersections from child branches
        for (uint8_t i=0; i < 8; ++i)
        {
            intersections.insert(intersections.end(),
                    child(i)->intersections.begin(),
                    child(i)->intersections.end());
        }
    }
    return intersections;
}


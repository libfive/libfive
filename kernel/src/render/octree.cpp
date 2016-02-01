#include "ao/kernel/render/octree.hpp"
#include "ao/kernel/render/region.hpp"

#include "ao/kernel/eval/evaluator.hpp"
#include "ao/kernel/tree/tree.hpp"

Octree::Octree(Evaluator* e, const Subregion& r)
    : X(r.X.lower(), r.X.upper()),
      Y(r.Y.lower(), r.Y.upper()),
      Z(r.Z.lower(), r.Z.upper()),
      type(populateChildren(e, r))
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
    return {i & 4 ? X.upper() : X.lower(),
            i & 2 ? Y.upper() : Y.lower(),
            i & 1 ? Z.upper() : Z.lower()};
}

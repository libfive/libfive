#include "catch.hpp"

#include "ao/tree/tree.hpp"

#include "ao/render/octree.hpp"
#include "ao/render/region.hpp"

#include "util/shapes.hpp"

using namespace Kernel;

TEST_CASE("Octree coordinates")
{
    Tree t = sphere(1);

    Region r({-1, 1}, {-1, 1}, {-1, 1}, 1);
    std::unique_ptr<Octree> out(Octree::render(t, r));
    REQUIRE(out->getType() == Octree::BRANCH);

    // Check that all child pointers are populated
    for (int i=0; i < 8; ++i)
    {
        CAPTURE(i);
        CAPTURE(out->child(i));
        REQUIRE(out->child(i) != nullptr);
    }

    // Check that Subregion::octsect and Octree::pos have consistent ordering
    for (int i=0; i < 8; ++i)
    {
        REQUIRE(out->pos(i) == out->child(i)->pos(i));
    }
}

TEST_CASE("Octree values")
{
    Tree t = sphere(1);

    Region r({-1, 1}, {-1, 1}, {-1, 1}, 1);
    REQUIRE(r.X.values.size() == 2);

    std::unique_ptr<Octree> out(Octree::render(t, r));

    // Check that values and gradients are correct
    for (int i=0; i < 8; ++i)
    {
        REQUIRE(!out->corner(i));
    }
}

TEST_CASE("Vertex positioning on sphere")
{
    const float radius = 0.5;
    Tree t = sphere(radius);

    Region r({-1, 1}, {-1, 1}, {-1, 1}, 4);

    std::unique_ptr<Octree> out(Octree::render(t, r));

    // Walk every leaf node in the octree, keeping track of the
    // minimum and maximum vertex radius
    float rmax = -std::numeric_limits<float>::infinity();
    float rmin =  std::numeric_limits<float>::infinity();

    // Queue of octrees to process
    std::list<const Octree*> targets = {out.get()};
    while (targets.size())
    {
        const Octree* o = targets.front();
        targets.pop_front();

        if (o->getType() == Octree::BRANCH)
        {
            for (unsigned i=0; i < 8; ++i)
            {
                targets.push_back(o->child(i));
            }
        }
        else if (o->getType() == Octree::LEAF)
        {
            float r = o->getVertex().norm();
            rmax = std::max(r, rmax);
            rmin = std::max(r, rmin);
        }
    }

    REQUIRE(rmin > radius*0.9);
    REQUIRE(rmax < radius*1.1);
}


TEST_CASE("Vertex positioning on sliced box")
{
    auto t = max(max(max(max(-Tree::X(), Tree::X() - 1),
                         max(-Tree::Y(), Tree::Y() - 1)),
                         max(-Tree::Z(), Tree::Z() - 1)),
                 1.5 - Tree::X() - Tree::Y() - Tree::Z());

    Region r({-2, 2}, {-2, 2}, {-2, 2}, 4);
    std::unique_ptr<Octree> out(Octree::render(t, r));

    auto eval = Evaluator(t);
    float worst = 0;
    std::list<const Octree*> targets = {out.get()};
    while (targets.size())
    {
        const Octree* o = targets.front();
        targets.pop_front();

        if (o->getType() == Octree::BRANCH)
        {
            for (unsigned i=0; i < 8; ++i)
            {
                targets.push_back(o->child(i));
            }
        }
        else if (o->getType() == Octree::LEAF)
        {
            worst = fmax(worst, fabs(eval.eval(o->getVertex().cast<float>())));
        }
    }
    REQUIRE(worst < 0.001);
}

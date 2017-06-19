#include "catch.hpp"

#include "ao/tree/tree.hpp"

#include "ao/render/quadtree.hpp"
#include "ao/render/region.hpp"

using namespace Kernel;

TEST_CASE("Vertex positioning on sliced rectangle")
{
    auto t = max(max(max(-Tree::X(), Tree::X() - 0.5),
                     max(-Tree::Y(), Tree::Y() - 0.5)),
                 0.5 - Tree::X() - Tree::Y());

    Region r({-1, 1}, {-1, 1}, {0, 0}, 4);
    std::unique_ptr<Quadtree> out(Quadtree::render(t, r));

    auto eval = Evaluator(t);
    float worst = 0;
    std::list<const Quadtree*> targets = {out.get()};
    while (targets.size())
    {
        const Quadtree* o = targets.front();
        targets.pop_front();

        if (o->getType() == Quadtree::BRANCH)
        {
            for (unsigned i=0; i < 4; ++i)
            {
                targets.push_back(o->child(i));
            }
        }
        else if (o->getType() == Quadtree::LEAF)
        {
            std::cout << "[" << o->pos(0).head<2>().transpose() << "]" 
                << "[" << o->pos(Axis::AXIS_X | Axis::AXIS_Y).head<2>().transpose() << "]" << std::endl;
            std::cout << o->getVertex().head<2>().transpose() << std::endl;
            printf("%f\n\n",eval.eval(o->getVertex().cast<float>()));
            worst = fmax(worst, fabs(eval.eval(o->getVertex().cast<float>())));
        }
    }
    REQUIRE(worst < 0.001);
}

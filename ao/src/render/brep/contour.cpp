#include "ao/render/brep/contour.hpp"
#include "ao/render/brep/xtree.hpp"
#include "ao/render/brep/scaffold.hpp"
#include "ao/render/brep/ms.hpp"
#include "ao/render/brep/dual.hpp"

namespace Kernel {

std::unique_ptr<Contour> Contour::render(const Tree t, const Region<2>& r)
{
    std::unique_ptr<Evaluator> eval(new Evaluator(t));

    // Create a padded scaffolding for the XTree
    const auto scaffold = Scaffold<2>(eval.get(), r, 4, true);

    // Create the quadtree on the scaffold
    auto xtree = XTree<2>(eval.get(), scaffold);

    // Perform marching squares
    SquareMarcher ms(eval.get());
    Dual<2>::walk(xtree, ms);

    auto out = new Contour;
    out->bbox = r;

    return std::unique_ptr<Contour>(out);
}

}   // namespace Kernel

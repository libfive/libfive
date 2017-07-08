#include "ao/render/brep/contour.hpp"
#include "ao/render/brep/xtree.hpp"
#include "ao/render/brep/scaffold.hpp"
#include "ao/render/brep/ms.hpp"
#include "ao/render/brep/dual.hpp"

namespace Kernel {

std::unique_ptr<Contour> Contour::render(const Tree t, const Region<2>& _r)
{
    const int subdiv = 4;

    // Figure out an expanded region such that the outer shell of cells
    // (at the given subdivision level) are outside of the target region
    //
    // This forces the creation of QEF cells / vertices on the model boundary.
    const auto size = _r.upper - _r.lower;
    const auto expanded = size * (1 << subdiv) / ((1 << subdiv) - 2.0);
    const auto center = (_r.upper + _r.lower) / 2;

    const auto r = Region<2>(center - expanded/2, center + expanded/2);

    std::unique_ptr<Evaluator> eval(new Evaluator(t));
    auto xtree = XTree<2>(eval.get(), Scaffold<2>(eval.get(), r, subdiv));

    SquareMarcher ms(eval.get());
    Dual<2>::walk(xtree, ms);
}

}   // namespace Kernel

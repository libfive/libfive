#include <iostream>
#include "ao/render/brep/scaffold.hpp"

namespace Kernel {

template <unsigned N>
Scaffold<N>::Scaffold(Evaluator* eval, Region<N> r, unsigned depth, bool pad)
    : region(r), type(Interval::AMBIGUOUS)
{

    // Figure out an expanded region such that the outer shell of cells
    // (at the given subdivision level) are outside of the target region
    //
    // This forces the creation of QEF cells / vertices on the model boundary.
    if (pad)
    {
        if (depth > 1)
        {
            const auto size = r.upper - r.lower;
            const auto expanded = size * (1 << depth) / ((1 << depth) - 2.0);
            const auto center = r.center();

            region = Region<N>(center - expanded/2, center + expanded/2,
                               r.perp);
        }
        else
        {
            std::cerr << "Scaffold::Scaffold: cannot pad with depth=1"
                      << std::endl;
        }
    }

    eval->set(region.lower3(), region.upper3());
    const auto i = eval->interval();
    eval->push();

    if (depth == 0)
    {
        type = Interval::state(i);
    }
    else
    {
        auto rs = region.subdivide();
        for (unsigned i=0; i < (1 << N); ++i)
        {
            children[i].reset(new Scaffold<N>(eval, rs[i], depth - 1));
        }
    }
    eval->pop();
}

// Explicit initialization of templates
template class Scaffold<2>;
template class Scaffold<3>;

}   // namespace Kernel

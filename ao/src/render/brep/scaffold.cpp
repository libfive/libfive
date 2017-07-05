#include "ao/render/brep/scaffold.hpp"

namespace Kernel {

template <unsigned N>
Scaffold<N>::Scaffold(Evaluator* eval, Region<N> region, unsigned depth)
    : region(region), type(AMBIGUOUS)
{
    eval->set(region.lower3(), region.upper3());
    const auto i = eval->interval();
    eval->push();

    if (depth == 0)
    {
        if (i.upper() < 0)
        {
            type = FULL;
        }
        else if (i.lower() > 0)
        {
            type = EMPTY;
        }
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

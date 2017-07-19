#pragma once

#include "ao/render/brep/region.hpp"
#include "ao/eval/evaluator.hpp"

namespace Kernel {

template <unsigned N>
class Interpolator
{
public:
    /*  Adopt this typedef for terseness */
    typedef typename Region<N>::Pt Pt;
    typedef typename Region<N>::Perp Perp;

    /*
     *  Main constructor
     *  The evaluator must outlive the Interpolator
     */
    Interpolator(Evaluator* eval, Perp perp=Perp::Zero())
        : eval(eval), perp(perp) {}

    /*
     *  Interpolate between two points, one of which is inside
     *  and the other is outside the model
     */
    Pt between(Pt inside, Pt outside);

    /*
     *  Push the Evaluator into the given region
     *  (which may speed up evaluation within that region)
     */
    void push(Region<N> region);

    /*
     *  Pop the evaluator out of the current region
     */
    void pop();

protected:
    Evaluator* eval;
    Perp perp;

    static constexpr int SEARCH_COUNT = 16;
};

}   // namespace Kernel

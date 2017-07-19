#include <iostream>
#include "ao/render/brep/interpolator.hpp"

namespace Kernel {

template <unsigned N>
typename Interpolator<N>::Pt Interpolator<N>::between(Pt inside, Pt outside)
{
    // We do an F-fold reduction at each stage
    constexpr int _F = 4;
    constexpr int F = (1 << _F);
    constexpr int ITER = SEARCH_COUNT / _F;

    {   // Check for inside/outside correctness
        Eigen::Array3f vec;
        vec.template tail<3 - N>() = perp;

        vec.template head<N>() = inside;
        eval->setRaw(vec, 0);
        vec.template head<N>() = outside;
        eval->setRaw(vec, 1);

        auto vs = eval->values(2);
        bool correct = (vs[0] < 0 && vs[1] >= 0);
        if (!correct)
        {
            std::cerr << "Interpolator::between: bad directionality"
                      << std::endl;
        }
        assert(correct);
    }

    // Binary search for intersection
    for (int i=0; i < ITER; ++i)
    {
        Eigen::Vector3f ps[F];
        for (int j=0; j < F; ++j)
        {
            float frac = j / (F - 1.0);
            ps[j].template head<N>() = (inside * (1 - frac)) + (outside * frac);
            ps[j].template tail<3 - N>() = perp;
            eval->setRaw(ps[j], j);
        }

        auto out = eval->values(F);
        for (int j=0; j < F; ++j)
        {
            if (Interval::isEmpty(out[j] >= 0))
            {
                inside = ps[j - 1].template head<N>();
                outside = ps[j].template head<N>();
                break;
            }
        }
    }

    return inside;
}

template class Interpolator<2>;
template class Interpolator<3>;

}   // namespace Kernel

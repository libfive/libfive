#pragma once

#include "ao/render/brep/marcher.hpp"

namespace Kernel {

class SquareMarcher : public Marcher<2>
{
public:
    SquareMarcher(Evaluator* e) : Marcher<2>(e) {}
    void operator()(const std::array<XTree<2>*, 4>& ts);

protected:
    typedef std::pair<int8_t, int8_t> Edge;
    static const Edge cases[16][2][2];
};

}   // namespace Kernel

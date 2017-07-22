#pragma once

#include "ao/render/brep/marcher.hpp"

namespace Kernel {

class TetMarcher : public Marcher<3>
{
public:
    TetMarcher(Evaluator* e) : Marcher<3>(e) {}

    void operator()(const std::array<XTree<3>*, 8>& ts);
};

}   // namespace Kernel


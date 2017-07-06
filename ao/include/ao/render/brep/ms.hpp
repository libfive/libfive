#pragma once

#include "ao/render/brep/xtree.hpp"

namespace Kernel {

class SquareMarcher
{
public:
    SquareMarcher(Evaluator* e) : eval(e) {}

    void operator()(const std::array<XTree<2>*, 4>& ts);

    typedef Eigen::Vector2f Pt;
    std::list<std::pair<Pt, Pt>> segments;

protected:
    Evaluator* eval;

    enum Edge { LEFT, RIGHT, UPPER, LOWER, NONE };
    static Edge cases[16][2][2];
    static uint8_t edges[4][2][2];
};

}   // namespace Kernel

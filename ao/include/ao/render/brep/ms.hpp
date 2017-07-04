#pragma once

#include "ao/render/brep/xtree.hpp"

namespace Kernel {

class SquareMarcher
{
public:
    SquareMarcher(Evaluator* e) : eval(e) {}

    void operator()(const XTree<2>& a, const XTree<2>& b,
                    const XTree<2>& c, const XTree<2>& d);

    typedef Eigen::Vector2f Pt;
    std::list<std::pair<Pt, Pt>> segments;

protected:
    Evaluator* eval;

    enum Edge { LEFT, RIGHT, UPPER, LOWER, NONE };
    static Edge cases[16][2][2];
    static uint8_t edges[4][2][2];
};

}   // namespace Kernel

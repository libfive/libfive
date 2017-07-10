#pragma once

#include "ao/render/brep/xtree.hpp"
#include "ao/render/brep/interpolator.hpp"

namespace Kernel {

class SquareMarcher
{
public:
    SquareMarcher(Evaluator* e) : eval(e), interp(e) {}

    void operator()(const std::array<XTree<2>*, 4>& ts);

    typedef std::pair<const XTree<2>*, const XTree<2>*> Key;
    typedef Eigen::Vector2f Pt;

    std::map<Key, Pt> points;
    std::list<std::pair<Key, Key>> segments;

protected:
    Evaluator* eval;
    Interpolator<2> interp;

    enum Edge { LEFT, RIGHT, UPPER, LOWER, NONE };
    static Edge cases[16][2][2];
    static uint8_t edges[4][2];
};

}   // namespace Kernel

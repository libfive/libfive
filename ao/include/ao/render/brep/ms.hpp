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
    typedef std::pair<SquareMarcher::Edge, SquareMarcher::Edge> Segment;
    static Segment cases[16][2];
    static std::pair<uint8_t, uint8_t> edges[4];
};

}   // namespace Kernel

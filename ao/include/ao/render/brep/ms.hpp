#pragma once

#include "ao/render/brep/xtree.hpp"
#include "ao/render/brep/interpolator.hpp"

namespace Kernel {

class SquareMarcher
{
public:
    SquareMarcher(Evaluator* e) : eval(e), interp(e) {}

    void operator()(const std::array<XTree<2>*, 4>& ts);

    /*  We store a map from dual edge pairs into point array */
    typedef std::pair<const XTree<2>*, const XTree<2>*> Key;
    std::map<Key, uint32_t> indices;

    /*  Flat array of point positions  */
    std::vector<Eigen::Vector2f> pts;

    /*  Segments, as a list of indices into point array */
    std::list<std::pair<uint32_t, uint32_t>> segments;

protected:
    Evaluator* eval;
    Interpolator<2> interp;

    typedef std::pair<int8_t, int8_t> Edge;
    static Edge cases[16][2][2];
};

}   // namespace Kernel

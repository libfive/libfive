#pragma once

#include "ao/render/brep/xtree.hpp"
#include "ao/render/brep/interpolator.hpp"

namespace Kernel {

template <unsigned N>
class Marcher
{
public:
    Marcher(Evaluator* e) : eval(e), interp(e) {}

    void push(const Region<N>& r)
    {
        eval->set(r.lower3(), r.upper3());
        eval->interval();
        eval->push();
    }

    void pop() { eval->pop(); }

    /*  We store a map from dual edge pairs into point array */
    typedef std::pair<const XTree<N>*, const XTree<N>*> Key;
    std::map<Key, uint32_t> indices;

    /*  Flat array of point positions  */
    std::vector<Eigen::Matrix<float, N, 1>> verts;

    /*  [N-1]-dimensional objects (line segments, triangles) */
    std::vector<std::array<uint32_t, N>> branes;

protected:
    Evaluator* eval;
    Interpolator<N> interp;
};

}   // namespace Kernel

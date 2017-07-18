#pragma once

#include "ao/render/brep/xtree.hpp"
#include "ao/render/brep/interpolator.hpp"

namespace Kernel {

class TetMarcher
{
public:
    TetMarcher(Evaluator* e) : eval(e), interp(e) {}

    void operator()(const std::array<XTree<3>*, 8>& ts);

    /*  We store a map from dual edge pairs into point array */
    typedef std::pair<const XTree<3>*, const XTree<3>*> Key;
    std::map<Key, uint32_t> indices;

    /*  Flat array of vertex positions  */
    std::vector<Eigen::Vector3f> verts;
    /*  Triangles, as a list of indices into verts array */
    std::vector<std::tuple<uint32_t, uint32_t, uint32_t>> tris;

protected:
    Evaluator* eval;
    Interpolator<3> interp;
};

}   // namespace Kernel


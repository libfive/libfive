#pragma once
#include <iostream>

#include <array>
#include <cstdint>
#include <Eigen/Eigen>

#include "ao/render/brep/region.hpp"
#include "ao/eval/evaluator.hpp"

namespace Kernel {

template <unsigned N>
class XTree
{
public:
    XTree(Evaluator* eval, Region<N> region);

    /*  Boilerplate for an object that contains an Eigen struct  */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    /*  The region filled by this XTree */
    Region<N> region;

    /*  Children pointers, if this is a branch  */
    std::array<std::unique_ptr<XTree<N>>, 1 << N> children;

    /*  Vertex location, if this is a leaf  */
    Eigen::Array<float, N, 1> vert;

protected:
    /*
     *  Searches for a vertex within the XTree cell, using a QEF
     *  and `R` samples per axis.
     *
     *  Returns true if a vertex is found with sufficiently low error,
     *  otherwise false.
     */
    template <unsigned R=4>
    bool findVertex(Evaluator* eval);
};

}   // namespace Kernel

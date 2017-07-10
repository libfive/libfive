#pragma once
#include <iostream>

#include <array>
#include <cstdint>
#include <Eigen/Eigen>

#include "ao/render/brep/region.hpp"
#include "ao/eval/evaluator.hpp"
#include "ao/eval/interval.hpp"

namespace Kernel {

template <unsigned N> class Scaffold;

template <unsigned N>
class XTree
{
public:
    /*
     *  Constructs an octree or quadtree by subdividing a region
     */
    XTree(Evaluator* eval, Region<N> region);

    /*
     *  Constructs an octree or quadtree on a scaffold, subdividing
     *  leaf cells that are part of the dual grid
     */
    XTree(Evaluator* eval, const Scaffold<N>& sca);

    /*
     *  Checks whether this tree splits
     */
    bool isBranch() const { return children[0].get() != nullptr; }

    /*
     *  Looks up a child, returning *this if this isn't a branch
     */
    const XTree<N>& child(unsigned i) const
    { return isBranch() ? *children[i] : *this; }
    XTree<N>& child(unsigned i)
    { return isBranch() ? *children[i] : *this; }

    /*  Boilerplate for an object that contains an Eigen struct  */
    EIGEN_MAKE_ALIGNED_OPERATOR_NEW

    /*  The region filled by this XTree */
    Region<N> region;

    /*  Children pointers, if this is a branch  */
    std::array<std::unique_ptr<XTree<N>>, 1 << N> children;

    /*  Vertex location, if this is a leaf  */
    Eigen::Array<float, N, 1> vert;

    /*  Error for QEF solving (-1 if unsolved)  */
    float err = -1;

    /*  Used when doing scaffolding construction */
    Interval::State type = Interval::UNKNOWN;

    /*
     *  Unpack the vertex into a 3-element array
     *  (using the perpendicular coordinates)
     */
    Eigen::Vector3f vert3() const;

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

    constexpr static float MAX_ERROR = 1e-6;
};

}   // namespace Kernel

#pragma once

#include <array>

#include "ao/render/brep/region.hpp"
#include "ao/eval/evaluator.hpp"

namespace Kernel {

/*
 *  A scaffold is an evenly divided quad or octree that goes to a particular
 *  depth.  Every leaf-level cell is marked as empty or full
 *
 *  This allows a particular kind of optimization in dual marching cubes,
 *  where we can avoid recursing into cells that don't have any sign-changing
 *  edges to their neighbors.
 *
 *  It also lets us enforce an initial subdivision level on the quad/octree,
 *  which is useful to set up correctly subdivided boundary cells.
 */
template <unsigned N>
class Scaffold
{
public:
    Scaffold(Evaluator* eval, Region<N> region, unsigned depth);

    /*  The region filled by this Scaffold */
    Region<N> region;

    /*  Children pointers, if this is a branch  */
    std::array<std::unique_ptr<Scaffold<N>>, 1 << N> children;

    /*  Type, if this isn't a branch  */
    enum { EMPTY, FULL, AMBIGUOUS } type;
};

};

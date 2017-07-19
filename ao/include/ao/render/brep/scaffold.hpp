#pragma once

#include <array>

#include "ao/render/brep/region.hpp"
#include "ao/eval/interval.hpp"
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
    /*
     *  Constructs a scaffolding
     *
     *  If pad is true, then the region is expanded so that the outermost
     *  cells of the scaffold are just outside the original region.  This
     *  is useful for creating outside-of-model sections for rendering.
     *
     *  Note that this kind of padding will lower the effective resolution
     *  within the target region, by an amount that depends on the depth.
     */
    Scaffold(Evaluator* eval, Region<N> region, unsigned depth, bool pad=false);

    /*  The region filled by this Scaffold */
    Region<N> region;

    /*  Children pointers, if this is a branch  */
    std::array<std::unique_ptr<Scaffold<N>>, 1 << N> children;

    /*  Type, if this isn't a branch  */
    Interval::State type;
};

};

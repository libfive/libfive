#include "ao/render/quadtree.hpp"

////////////////////////////////////////////////////////////////////////////////

namespace Kernel {

Quadtree::Quadtree(const Subregion& r) : XTree(r)
{
    // Nothing to do here
}

Quadtree::Quadtree(Evaluator* e, const Subregion& r)
    : XTree(e, r)
{
    finalize(e);
}

Quadtree::Quadtree(Evaluator* e, const std::array<Quadtree*, 4>& cs,
       const Subregion& r)
    : XTree(cs, r)
{
    finalize(e);
}

// These are the four edges of a quadtree cell
const std::vector<std::pair<unsigned, unsigned>> Quadtree::_cellEdges =
    {{0, AXIS_X}, {0, AXIS_Y},
     {AXIS_X, AXIS_X|AXIS_Y},  {AXIS_Y, AXIS_Y|AXIS_X}};

const std::vector<bool> Quadtree::_cornerTable =
        {1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1};

bool Quadtree::leafTopology() const
{
    /*  See detailed comment in Octree::leafTopology */
    const bool edges_safe =
        (child(0)->corner(AXIS_X) == corner(0) ||
         child(0)->corner(AXIS_X) == corner(AXIS_X))
    &&  (child(0)->corner(AXIS_Y) == corner(0) ||
         child(0)->corner(AXIS_Y) == corner(AXIS_Y))
    &&  (child(AXIS_X)->corner(AXIS_X|AXIS_Y) == corner(AXIS_X) ||
         child(AXIS_X)->corner(AXIS_X|AXIS_Y) == corner(AXIS_X|AXIS_Y))
    &&  (child(AXIS_Y)->corner(AXIS_Y|AXIS_X) == corner(AXIS_Y) ||
         child(AXIS_Y)->corner(AXIS_Y|AXIS_X) == corner(AXIS_Y|AXIS_X));

    const bool faces_safe =
        (child(0)->corner(AXIS_Y|AXIS_X) == corner(0) ||
         child(0)->corner(AXIS_Y|AXIS_X) == corner(AXIS_Y) ||
         child(0)->corner(AXIS_Y|AXIS_X) == corner(AXIS_X) ||
         child(0)->corner(AXIS_Y|AXIS_X) == corner(AXIS_Y|AXIS_X));

    return edges_safe && faces_safe;
}

}   // namespace Kernel

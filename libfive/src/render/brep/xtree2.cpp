#include "xtree.cpp"

namespace Kernel {

template <>
bool XTree<2>::cornersAreManifold() const
{
    const static bool corner_table[] =
        {1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1};
    return corner_table[corner_mask];
}

////////////////////////////////////////////////////////////////////////////////
// Specializations for quadtree
template <>
bool XTree<2>::leafsAreManifold() const
{
    /*  See detailed comment in Octree::leafTopology */
    const bool edges_safe =
        (child(0)->cornerState(Axis::X) == cornerState(0) ||
         child(0)->cornerState(Axis::X) == cornerState(Axis::X))
    &&  (child(0)->cornerState(Axis::Y) == cornerState(0) ||
         child(0)->cornerState(Axis::Y) == cornerState(Axis::Y))
    &&  (child(Axis::X)->cornerState(Axis::X|Axis::Y) == cornerState(Axis::X) ||
         child(Axis::X)->cornerState(Axis::X|Axis::Y) == cornerState(Axis::X|Axis::Y))
    &&  (child(Axis::Y)->cornerState(Axis::Y|Axis::X) == cornerState(Axis::Y) ||
         child(Axis::Y)->cornerState(Axis::Y|Axis::X) == cornerState(Axis::Y|Axis::X));

    const bool faces_safe =
        (child(0)->cornerState(Axis::Y|Axis::X) == cornerState(0) ||
         child(0)->cornerState(Axis::Y|Axis::X) == cornerState(Axis::Y) ||
         child(0)->cornerState(Axis::Y|Axis::X) == cornerState(Axis::X) ||
         child(0)->cornerState(Axis::Y|Axis::X) == cornerState(Axis::Y|Axis::X));

    return edges_safe && faces_safe;
}

template <>
const std::vector<std::pair<uint8_t, uint8_t>>& XTree<2>::edges() const
{
    static const std::vector<std::pair<uint8_t, uint8_t>> es =
        {{0, Axis::X}, {0, Axis::Y},
         {Axis::X, Axis::X|Axis::Y}, {Axis::Y, Axis::Y|Axis::X}};
    return es;
}

// Explicit initialization of template
template class XTree<2>;

}   // namespace Kernel

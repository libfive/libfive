#include "dc_tree.cpp"

namespace Kernel {

template <>
bool DCTree<2>::cornersAreManifold(const uint8_t corner_mask)
{
    const static bool corner_table[] =
        {1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1};
    return corner_table[corner_mask];
}

////////////////////////////////////////////////////////////////////////////////
// Specializations for quadtree
template <>
bool DCTree<2>::leafsAreManifold(
        const std::array<DCTree<2>*, 1 << 2>& cs,
        const std::array<Interval::State, 1 << 2>& corners)
{
    /*  See detailed comment in Octree::leafsAreManifold */
    const bool edges_safe =
        (cs[0]->cornerState(Axis::X) == corners[0] ||
         cs[0]->cornerState(Axis::X) == corners[Axis::X])
    &&  (cs[0]->cornerState(Axis::Y) == corners[0] ||
         cs[0]->cornerState(Axis::Y) == corners[Axis::Y])
    &&  (cs[Axis::X]->cornerState(Axis::X|Axis::Y) == corners[Axis::X] ||
         cs[Axis::X]->cornerState(Axis::X|Axis::Y) == corners[Axis::X|Axis::Y])
    &&  (cs[Axis::Y]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y] ||
         cs[Axis::Y]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y|Axis::X]);

    const bool faces_safe =
        (cs[0]->cornerState(Axis::Y|Axis::X) == corners[0] ||
         cs[0]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y] ||
         cs[0]->cornerState(Axis::Y|Axis::X) == corners[Axis::X] ||
         cs[0]->cornerState(Axis::Y|Axis::X) == corners[Axis::Y|Axis::X]);

    return edges_safe && faces_safe;
}

template <>
const std::vector<std::pair<uint8_t, uint8_t>>& DCTree<2>::edges() const
{
    static const std::vector<std::pair<uint8_t, uint8_t>> es =
        {{0, Axis::X}, {0, Axis::Y},
         {Axis::X, Axis::X|Axis::Y}, {Axis::Y, Axis::Y|Axis::X}};
    return es;
}

// Explicit initialization of template
template class DCTree<2>;

}   // namespace Kernel

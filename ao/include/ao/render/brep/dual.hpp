#pragma once

#include "ao/render/brep/xtree.hpp"
#include "ao/render/axes.hpp"

namespace Kernel {

/*
 *  Class to walk a dual grid for a quad or octree
 *  t needs operator(const std::array<XTree<N>*, N>& trees) defined
 */
template <unsigned N>
class Dual
{
public:
    template<typename V>
    static void walk(const XTree<N>& tree, V& v);
};

////////////////////////////////////////////////////////////////////////////////
// 2D Implementation
template <typename V>
void vert2(const XTree<2>& a, const XTree<2>& b,
           const XTree<2>& c, const XTree<2>& d, V& v)
{
    if (a.isBranch() || b.isBranch() || c.isBranch() || d.isBranch())
    {
        vert2(a.child(Axis::X | Axis::Y),
              b.child(Axis::Y), c.child(Axis::X),
              d.child(0), v);
    }
    else
    {
        v({{&a, &b, &c, &d}});
    }
}

template <typename V, Axis::Axis A>
void edge2(const XTree<2>& a, const XTree<2>& b, V& v)
{
    if (a.isBranch() || b.isBranch())
    {
        uint8_t perp = (Axis::X | Axis::Y) ^ A;
        edge2<V, A>(a.child(A), b.child(0), v);
        edge2<V, A>(a.child(A|perp), b.child(perp), v);

        if (A == Axis::X)
        {
            vert2(a.child(A), b.child(0), a.child(A|perp), b.child(perp), v);
        }
        else if (A == Axis::Y)
        {
            vert2(a.child(A), a.child(A|perp), b.child(0), b.child(perp), v);
        }
    }
}

template <>
template <typename V>
void Dual<2>::walk(const XTree<2>& t, V& v)
{
    if (t.isBranch())
    {
        // Recurse down every subface in the quadtree
        for (auto& c : t.children)
        {
            if (c.get())
            {
                walk(*c, v);
            }
        }

        //  Then, call edge on every pair of cells
        edge2<V, Axis::X>(*t.children[0], *t.children[Axis::X], v);
        edge2<V, Axis::X>(*t.children[Axis::Y], *t.children[Axis::Y | Axis::X], v);
        edge2<V, Axis::Y>(*t.children[0], *t.children[Axis::Y], v);
        edge2<V, Axis::Y>(*t.children[Axis::X], *t.children[Axis::X | Axis::Y], v);

        // Finally, recurse down towards the center of the cell
        vert2(*t.children[0],        *t.children[Axis::X],
              *t.children[Axis::Y],  *t.children[Axis::X|Axis::Y], v);
    }
}
////////////////////////////////////////////////////////////////////////////////

template <>
template <typename V>
void Dual<3>::walk(const XTree<3>& t, V& v)
{
    (void)t;
    (void)v;
    // TODO
}

}   // namespace Kernel

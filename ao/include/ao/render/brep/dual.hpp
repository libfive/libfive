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
    static void walk(XTree<N>& tree, V& v);
};

////////////////////////////////////////////////////////////////////////////////
// 2D Implementation
template <typename V>
void vert2(XTree<2>& a, XTree<2>& b,
           XTree<2>& c, XTree<2>& d, V& v)
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
void edge2(XTree<2>& a, XTree<2>& b, V& v)
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
void Dual<2>::walk(XTree<2>& t, V& v)
{
    if (t.isBranch())
    {
        // Recurse down every subface in the quadtree
        for (auto& c : t.children)
        {
            walk(*c, v);
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

template <typename V, Axis::Axis A>
void edge3(XTree<3>& a, XTree<3>& b, XTree<3>& c, XTree<3>& d, V& v)
{
    if (a.isBranch() || b.isBranch() || c.isBranch() || d.isBranch())
    {
        constexpr auto Q = Axis::Q(A);
        constexpr auto R = Axis::R(A);

        edge3<V, A>(a.child(Q|R), b.child(R), c.child(Q), d.child(0), v);
        edge3<V, A>(a.child(Q|R|A), b.child(R|A), c.child(Q|A), d.child(A), v);
    }
}

template <typename V, Axis::Axis A>
void face3(XTree<3>& a, XTree<3>& b, V& v)
{
    if (a.isBranch() || b.isBranch())
    {
        constexpr auto Q = Axis::Q(A);
        constexpr auto R = Axis::R(A);

        face3<V, A>(a.child(A), b.child(0), v);
        face3<V, A>(a.child(Q|A), b.child(Q), v);
        face3<V, A>(a.child(R|A), b.child(R), v);
        face3<V, A>(a.child(Q|R|A), b.child(Q|R), v);

        edge3<V, Q>(a.child(A), a.child(R|A), b.child(0), b.child(R), v);
        edge3<V, Q>(a.child(Q|A), a.child(Q|R|A), b.child(Q), b.child(Q|R), v);

        edge3<V, R>(a.child(A), b.child(0), a.child(A|Q), b.child(Q), v);
        edge3<V, R>(a.child(R|A), b.child(R), a.child(R|A|Q), b.child(R|Q), v);
    }
}

template <>
template <typename V>
void Dual<3>::walk(XTree<3>& t, V& v)
{
    if (t.isBranch())
    {
        // Recurse, calling the cell procedure for every child
        for (auto& c : t.children)
        {
            walk(*c, v);
        }

        // Then call the face procedure on every pair of cells
        face3<V, Axis::X>(t.child(0), t.child(Axis::X), v);
        face3<V, Axis::X>(t.child(Axis::Y), t.child(Axis::Y | Axis::X), v);
        face3<V, Axis::X>(t.child(Axis::Z), t.child(Axis::Z | Axis::X), v);
        face3<V, Axis::X>(t.child(Axis::Y | Axis::Z), t.child(Axis::Y | Axis::Z | Axis::X), v);

        face3<V, Axis::Y>(t.child(0), t.child(Axis::Y), v);
        face3<V, Axis::Y>(t.child(Axis::X), t.child(Axis::X | Axis::Y), v);
        face3<V, Axis::Y>(t.child(Axis::Z), t.child(Axis::Z | Axis::Y), v);
        face3<V, Axis::Y>(t.child(Axis::X | Axis::Z), t.child(Axis::X | Axis::Z | Axis::Y), v);

        face3<V, Axis::Z>(t.child(0), t.child(Axis::Z), v);
        face3<V, Axis::Z>(t.child(Axis::X), t.child(Axis::X | Axis::Z), v);
        face3<V, Axis::Z>(t.child(Axis::Y), t.child(Axis::Y | Axis::Z), v);
        face3<V, Axis::Z>(t.child(Axis::X | Axis::Y), t.child(Axis::X | Axis::Y | Axis::Z), v);

        // Finally, call the edge function 6 times
        edge3<V, Axis::Z>(t.child(0),
             t.child(Axis::X),
             t.child(Axis::Y),
             t.child(Axis::X | Axis::Y), v);
        edge3<V, Axis::Z>(t.child(Axis::Z),
             t.child(Axis::X | Axis::Z),
             t.child(Axis::Y | Axis::Z),
             t.child(Axis::X | Axis::Y | Axis::Z), v);

        edge3<V, Axis::X>(t.child(0),
             t.child(Axis::Y),
             t.child(Axis::Z),
             t.child(Axis::Y | Axis::Z), v);
        edge3<V, Axis::X>(t.child(Axis::X),
             t.child(Axis::Y | Axis::X),
             t.child(Axis::Z | Axis::X),
             t.child(Axis::Y | Axis::Z | Axis::X), v);

        edge3<V, Axis::Y>(t.child(0),
             t.child(Axis::Z),
             t.child(Axis::X),
             t.child(Axis::Z | Axis::X), v);
        edge3<V, Axis::Y>(t.child(Axis::Y),
             t.child(Axis::Z | Axis::Y),
             t.child(Axis::X | Axis::Y),
             t.child(Axis::Z | Axis::X | Axis::Y), v);
    }
}

}   // namespace Kernel

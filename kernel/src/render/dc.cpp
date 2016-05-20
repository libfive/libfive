/*
 *  Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>
 *
 *  This file is part of the Ao library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  Ao is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with Ao.  If not, see <http://www.gnu.org/licenses/>.
 */
#include <map>

#include "ao/kernel/format/mesh.hpp"
#include "ao/kernel/render/region.hpp"
#include "ao/kernel/render/octree.hpp"

////////////////////////////////////////////////////////////////////////////////

/*
 *  Helper struct that can be passed around when meshing
 */
namespace DC
{
struct Worker
{
    /*
     *  Mutually recursive functions to get a mesh from an Octree
     */
    void cell(const Octree* c);
    void face(const Octree* a, const Octree* b, Octree::Axis axis);
    void edge(const Octree* a, const Octree* b,
              const Octree* c, const Octree* d, Octree::Axis axis);

    /*
     *  Write out the given quad into the mesh
     */
    void quad(const Octree* a, const Octree* b,
              const Octree* c, const Octree* d);

    /*
     *  Return new axes such that a, q, r is right-handed
     */
    static Octree::Axis Q(Octree::Axis a);
    static Octree::Axis R(Octree::Axis a);

    std::map<const Octree*, unsigned> verts;
    Mesh mesh;
};

////////////////////////////////////////////////////////////////////////////////

Octree::Axis Worker::Q(Octree::Axis a)
{
    return (a == Octree::AXIS_X) ? Octree::AXIS_Y :
           (a == Octree::AXIS_Y) ? Octree::AXIS_Z
                                 : Octree::AXIS_X;
}

Octree::Axis Worker::R(Octree::Axis a)
{
    return (a == Octree::AXIS_X) ? Octree::AXIS_Z :
           (a == Octree::AXIS_Y) ? Octree::AXIS_X
                                 : Octree::AXIS_Y;
}

void Worker::cell(const Octree* c)
{
    if (c->getType() == Octree::BRANCH)
    {
        // Recurse, calling the cell procedure for every child
        for (uint8_t i=0; i < 8; ++i)
        {
            cell(c->child(i));
        }

        // Then call the face procedure on every pair of cells
        face(c->child(0), c->child(Octree::AXIS_X), Octree::AXIS_X);
        face(c->child(Octree::AXIS_Y),
             c->child(Octree::AXIS_Y | Octree::AXIS_X),
             Octree::AXIS_X);
        face(c->child(Octree::AXIS_Z),
             c->child(Octree::AXIS_Z | Octree::AXIS_X),
             Octree::AXIS_X);
        face(c->child(Octree::AXIS_Y | Octree::AXIS_Z),
             c->child(Octree::AXIS_Y | Octree::AXIS_Z | Octree::AXIS_X),
             Octree::AXIS_X);

        face(c->child(0), c->child(Octree::AXIS_Y), Octree::AXIS_Y);
        face(c->child(Octree::AXIS_X),
             c->child(Octree::AXIS_X | Octree::AXIS_Y),
             Octree::AXIS_Y);
        face(c->child(Octree::AXIS_Z),
             c->child(Octree::AXIS_Z | Octree::AXIS_Y),
             Octree::AXIS_Y);
        face(c->child(Octree::AXIS_X | Octree::AXIS_Z),
             c->child(Octree::AXIS_X | Octree::AXIS_Z | Octree::AXIS_Y),
             Octree::AXIS_Y);

        face(c->child(0), c->child(Octree::AXIS_Z), Octree::AXIS_Z);
        face(c->child(Octree::AXIS_X),
             c->child(Octree::AXIS_X | Octree::AXIS_Z),
             Octree::AXIS_Z);
        face(c->child(Octree::AXIS_Y),
             c->child(Octree::AXIS_Y | Octree::AXIS_Z),
             Octree::AXIS_Z);
        face(c->child(Octree::AXIS_X | Octree::AXIS_Y),
             c->child(Octree::AXIS_X | Octree::AXIS_Y | Octree::AXIS_Z),
             Octree::AXIS_Z);

        // Finally, call the edge function 6 times
        edge(c->child(0),
             c->child(Octree::AXIS_X),
             c->child(Octree::AXIS_Y),
             c->child(Octree::AXIS_X | Octree::AXIS_Y),
             Octree::AXIS_Z);
        edge(c->child(Octree::AXIS_Z),
             c->child(Octree::AXIS_X | Octree::AXIS_Z),
             c->child(Octree::AXIS_Y | Octree::AXIS_Z),
             c->child(Octree::AXIS_X | Octree::AXIS_Y | Octree::AXIS_Z),
             Octree::AXIS_Z);

        edge(c->child(0),
             c->child(Octree::AXIS_Y),
             c->child(Octree::AXIS_Z),
             c->child(Octree::AXIS_Y | Octree::AXIS_Z),
             Octree::AXIS_X);
        edge(c->child(Octree::AXIS_X),
             c->child(Octree::AXIS_Y | Octree::AXIS_X),
             c->child(Octree::AXIS_Z | Octree::AXIS_X),
             c->child(Octree::AXIS_Y | Octree::AXIS_Z | Octree::AXIS_X),
             Octree::AXIS_X);

        edge(c->child(0),
             c->child(Octree::AXIS_Z),
             c->child(Octree::AXIS_X),
             c->child(Octree::AXIS_Z | Octree::AXIS_X),
             Octree::AXIS_Y);
        edge(c->child(Octree::AXIS_Y),
             c->child(Octree::AXIS_Z | Octree::AXIS_Y),
             c->child(Octree::AXIS_X | Octree::AXIS_Y),
             c->child(Octree::AXIS_Z | Octree::AXIS_X | Octree::AXIS_Y),
             Octree::AXIS_Y);
    }
}

void Worker::face(const Octree* a, const Octree* b, Octree::Axis axis)
{
    if (a->getType() == Octree::BRANCH || b->getType() == Octree::BRANCH)
    {
        Octree::Axis q = Q(axis);
        Octree::Axis r = R(axis);

        face(a->child(axis), b->child(0), axis);
        face(a->child(q|axis), b->child(q), axis);
        face(a->child(r|axis), b->child(r), axis);
        face(a->child(q|r|axis), b->child(q|r), axis);

        edge(a->child(axis), a->child(r|axis), b->child(0), b->child(r), q);
        edge(a->child(q|axis), a->child(q|r|axis), b->child(q), b->child(q|r), q);

        edge(a->child(axis), b->child(0), a->child(axis|q), b->child(q), r);
        edge(a->child(r|axis), b->child(r), a->child(r|axis|q), b->child(r|q), r);
    }
}

void Worker::edge(const Octree* a, const Octree* b,
                  const Octree* c, const Octree* d,
                  Octree::Axis axis)
{
    Octree::Axis q = Q(axis);
    Octree::Axis r = R(axis);

    if (a->getType() == Octree::LEAF && b->getType() == Octree::LEAF &&
        c->getType() == Octree::LEAF && d->getType() == Octree::LEAF)
    {
        /*  We need to check the values on the shared edge to see whether we need
         *  to add a face.  However, this is tricky when the edge spans multiple
         *  octree levels.
         *
         * In the following diagram, the target edge is marked with an x
         * (travelling into the screen):
         *      _________________
         *      | a |           |
         *      ----x   c, d    |
         *      | b |           |
         *      ----------------|
         *
         *  If we were to look at corners of c or d, we wouldn't be looking at the
         *  correct edge.  Instead, we need to look at corners for the smallest cell
         *  among the function arguments.
         */

        // For different octrees, these are the corners to check
        std::map<const Octree*, unsigned> corners =
            {{a, q|r}, {b, r}, {c, q}, {d, 0}};

        // Sort the octrees by level
        std::vector<const Octree*> ptrs = {a, b, c, d};
        std::sort(ptrs.begin(), ptrs.end(),
                [](const Octree* lhs, const Octree* rhs){
                    return lhs->getLevel() < rhs->getLevel(); });

        // Find the target cell that is smallest
        const Octree* smallest = ptrs.front();

        // If the relevant edge is mismatched, then add a quad (flipping
        // it around depending on sign to get normals correct)
        if (smallest->corner(corners[smallest]) !=
            smallest->corner(axis|corners[smallest]))
        {
            if (smallest->corner(corners[smallest]))
            {
                quad(a, b, c, d);
            }
            else
            {
                quad(a, c, b, d);
            }
        }
    }
    else if (a->getType() == Octree::BRANCH || b->getType() == Octree::BRANCH ||
             c->getType() == Octree::BRANCH || d->getType() == Octree::BRANCH)
    {
        edge(a->child(q|r), b->child(r), c->child(q), d->child(0), axis);
        edge(a->child(q|r|axis), b->child(r|axis),
             c->child(q|axis), d->child(axis), axis);
    }
}

void Worker::quad(const Octree* a, const Octree* b,
                  const Octree* c, const Octree* d)
{
    auto index = [&](const Octree* o)
    {
        auto i = verts.find(o);
        if (i == verts.end())
        {
            verts[o] = mesh.verts.size();
            mesh.verts.push_back(o->getVertex());
        }
        return verts[o];
    };

    unsigned ia = index(a);
    unsigned ib = index(b);
    unsigned ic = index(c);
    unsigned id = index(d);

    if (ia != ib && ia != ic && ib != ic)
    {
        mesh.tris.push_back({ia, ib, ic});
    }
    if (ib != ic && ic != id && ib != id)
    {
        mesh.tris.push_back({ic, ib, id});
    }
}
}

////////////////////////////////////////////////////////////////////////////////

Mesh Mesh::Render(Tree* t, const Region& r, uint32_t flags)
{
    std::unique_ptr<Octree> o(Octree::Render(t, r, flags));

    DC::Worker w;
    w.cell(o.get());

    return w.mesh;
}

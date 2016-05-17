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

#include <set>
#include <map>

#include <glm/gtx/common.hpp>

#include "ao/kernel/format/contours.hpp"
#include "ao/kernel/render/region.hpp"

struct Comparator
{
    bool operator()(const glm::vec2& a, const glm::vec2& b) const
    {
        if (a.x != b.x)
        {
            return a.x < b.x;
        }
        else
        {
            return a.y < b.y;
        }
    }
};

/*
 *  Helper struct that can be passed around when making contours
 */
struct Worker
{
    /*
     *  Mutually recursive functions to get a mesh from an Octree
     */
    void cell(const Quadtree* c);
    void edge(const Quadtree* a, const Quadtree* b, Quadtree::Axis axis);

    /*
     *  Write out the given segment into the segment list
     */
    void segment(const Quadtree* a, const Quadtree* b);

    std::list<std::pair<glm::vec2, glm::vec2>> segments;
};

void Worker::cell(const Quadtree* c)
{
    if (c->getType() == Quadtree::BRANCH)
    {
        // Recurse down every subface in the quadtree
        for (int i=0; i < 4; ++i)
        {
            cell(c->child(i));
        }

        //  Then, call edge on every pair of cells
        edge(c->child(0), c->child(Quadtree::AXIS_X), Quadtree::AXIS_X);
        edge(c->child(Quadtree::AXIS_Y),
             c->child(Quadtree::AXIS_Y | Quadtree::AXIS_X),
             Quadtree::AXIS_X);
        edge(c->child(0), c->child(Quadtree::AXIS_Y), Quadtree::AXIS_Y);
        edge(c->child(Quadtree::AXIS_X),
             c->child(Quadtree::AXIS_X | Quadtree::AXIS_Y),
             Quadtree::AXIS_Y);
    }
}

void Worker::edge(const Quadtree* a, const Quadtree* b, Quadtree::Axis axis)
{
    if (a->getType() == Quadtree::LEAF && b->getType() == Quadtree::LEAF)
    {
        bool crossed;

        //  See detailed comment in the 3D implementation
        if (a->getLevel() < b->getLevel())
        {
            crossed = a->corner(axis) != a->corner(3);
        }
        else
        {
            crossed = b->corner(0) != b->corner(3 ^ axis);
        }

        if (crossed)
        {
            segment(a, b);
        }
    }
    else if (a->getType() == Quadtree::BRANCH || b->getType() == Quadtree::BRANCH)
    {
        edge(a->child(axis), b->child(0), axis);
        edge(a->child(3), b->child(3 ^ axis), axis);
    }
}

void Worker::segment(const Quadtree* a, const Quadtree* b)
{
    auto va = a->getVertex();
    auto vb = b->getVertex();

    segments.push_back({{va.x, va.y}, {vb.x, vb.y}});
}

////////////////////////////////////////////////////////////////////////////////

Contours Contours::Render(Tree* t, const Region& r, uint32_t flags)
{
    auto q = Quadtree::Render(t, r, flags);

    Worker w;
    w.cell(q);

    std::map<glm::vec2, glm::vec2, Comparator> segs;
    for (auto s : w.segments)
    {
        segs[s.first] = segs[s.second];
    }

    Contours c;
    while (segs.size())
    {
        auto front = *segs.begin();
        std::set<glm::vec2, Comparator> found = {front.first};
        std::vector<glm::vec2> vec = {front.first};

        auto back = front.first;
        // Walk around the contour until we hit a vertex that we've already
        // seen or we run out of vertices to walk around
        while (!found.count(back))
        {
            auto b = segs.find(back);
            if (b == segs.end())
            {
                break;
            }
            else
            {
                back = b->second;
                segs.erase(b);
            }

            vec.push_back(back);
            found.insert(back);
        }
        c.contours.emplace_back(std::move(vec));
    }
    return c;
}

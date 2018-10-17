/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <fstream>
#include <boost/algorithm/string/predicate.hpp>

#include "libfive/render/brep/contours.hpp"
#include "libfive/render/brep/brep.hpp"
#include "libfive/render/brep/dc/xtree_pool.hpp"
#include "libfive/render/brep/dual.hpp"

namespace Kernel {

class Segments : public BRep<2>
{
public:
    template <Axis::Axis A>
    void load(const std::array<const XTree<2>*, 2>& ts)
    {
        /*  See comment in mesh.cpp about selecting a minimum edge */
        const auto index = std::min_element(ts.begin(), ts.end(),
                [](const XTree<2>* a, const XTree<2>* b)
                { return a->leaf->level < b->leaf->level; }) - ts.begin();

        constexpr uint8_t perp = (Axis::X | Axis::Y) ^ A;
        constexpr std::array<uint8_t, 2> corners = {{perp, 0}};

        // If there is a sign change across the relevant edge, then call the
        // watcher with the segment corners (with proper winding order)
        auto a = ts[index]->cornerState(corners[index]);
        auto b = ts[index]->cornerState(corners[index] | A);
        if (a != b)
        {
            // Use either forward or reversed segment building
            if ((a == Interval::FILLED && A == Axis::Y) ||
                (b == Interval::FILLED && A == Axis::X))
            {
                load<A, 0>(ts);
            }
            else
            {
                load<A, 1>(ts);
            }
        }
    }

    template <Axis::Axis A, bool D>
    void load(const std::array<const XTree<2>*, 2>& ts)
    {
        // From axis and contour direction, extract the relevant edge index
        // numbers for the two cells in ts
        int es[2];
        if (D ^ (A == Axis::X))
        {
            es[0] = XTree<2>::mt->e[3][3^A];
            es[1] = XTree<2>::mt->e[A][0];
        }
        else
        {
            es[0] = XTree<2>::mt->e[3^A][3];
            es[1] = XTree<2>::mt->e[0][A];
        }
        assert(es[0] != -1);
        assert(es[1] != -1);

        uint32_t vs[2];
        for (unsigned i=0; i < ts.size(); ++i)
        {
            assert(ts[i]->leaf != nullptr);

            auto vi = ts[i]->leaf->level > 0
                ? 0
                : XTree<2>::mt->p[ts[i]->leaf->corner_mask][es[i]];
            assert(vi != -1);

            // Sanity-checking manifoldness of collapsed cells
            assert(ts[i]->leaf->level == 0 || ts[i]->leaf->vertex_count == 1);

            if (ts[i]->leaf->index[vi] == 0)
            {
                ts[i]->leaf->index[vi] = verts.size();

                // Look up the appropriate vertex id
                verts.push_back(ts[i]->vert(vi).template cast<float>());
            }
            vs[i] = ts[i]->leaf->index[vi];
        }
        // Handle contour winding direction
        branes.push_back({vs[!D], vs[D]});
    }
};

std::unique_ptr<Contours> Contours::render(
        const Tree t, const Region<2>& r,
        double min_feature, double max_err,
        bool multithread)
{
    const unsigned workers = multithread ? 8 : 1;
    std::atomic_bool cancel(false);

    std::vector<XTreeEvaluator, Eigen::aligned_allocator<XTreeEvaluator>> es;
    es.reserve(workers);
    for (unsigned i=0; i < workers; ++i)
    {
        es.emplace_back(XTreeEvaluator(t));
    }

    // Create the quadtree on the scaffold
    auto xtree = XTreePool<2>::build(
        es.data(), r, min_feature, max_err,
        workers, cancel);

    // Perform marching squares
    Segments segs;
    Dual<2>::walk(xtree.get(), segs);

    auto c = std::unique_ptr<Contours>(new Contours(r));

    // Maps from index (in ms) to item in segments vector
    std::map<uint32_t, uint32_t> heads;
    std::map<uint32_t, uint32_t> tails;
    std::vector<std::list<uint32_t>> contours;

    for (auto& s : segs.branes)
    {
        {   // Check to see whether we can attach to the back of a tail
            auto t = tails.find(s[0]);
            if (t != tails.end())
            {
                contours[t->second].push_back(s[1]);
                tails.insert({s[1], t->second});
                tails.erase(t);
                continue;
            }
        }

        {   // Otherwise, see if we should prepend ourselves to a head
            auto h = heads.find(s[1]);
            if (h != heads.end())
            {
                contours[h->second].push_front(s[0]);
                heads.insert({s[0], h->second});
                heads.erase(h);
                continue;
            }
        }

        // Otherwise, start a new multi-segment line
        heads[s[0]] = contours.size();
        tails[s[1]] = contours.size();
        contours.push_back({s[0], s[1]});
    }

    std::vector<bool> processed(contours.size(), false);
    for (unsigned i=0; i < contours.size(); ++i)
    {
        if (processed[i])
        {
            continue;
        }
        c->contours.push_back(std::vector<Eigen::Vector2f>());

        // Weld multiple contours together here
        unsigned target = i;
        while (true)
        {
            for (const auto& pt : contours[target])
            {
                c->contours.back().push_back(segs.verts[pt]);
            }
            processed[target] = true;

            // Check for the next potentially-connected contour
            auto h = heads.find(contours[target].back());
            if (h != heads.end() && !processed[h->second])
            {
                // Remove the back point, beause it will be re-inserted
                // as the first point in the next contour
                c->contours.back().pop_back();
                target = h->second;
            }
            else
            {
                break;
            }
        }
    }

    return c;
}

bool Contours::saveSVG(const std::string& filename)
{
    if (!boost::algorithm::iends_with(filename, ".svg"))
    {
        std::cerr << "Contours::saveSVG: filename \"" << filename
                  << "\" does not end in .svg" << std::endl;
    }
    std::ofstream file;
    file.open(filename, std::ios::out);
    if (!file.is_open())
    {
        std::cout << "Contours::saveSVG: could not open " << filename
                  << std::endl;
        return false;
    }

    file <<
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
        "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"\n"
          " width=\"" << bbox.upper.x() - bbox.lower.x() <<
        "\" height=\"" << bbox.upper.y() - bbox.lower.y() <<
        "\" id=\"libfive\">\n";

    for (const auto& seg : contours)
    {
        file << "<path d=\"";

        const bool closed = seg.front() == seg.back();
        auto itr = seg.cbegin();
        auto end = seg.cend();
        if (closed)
        {
            end--;
        }

        // Initial move command
        file << "M " << itr->x() - bbox.lower.x()
             << ' '  << bbox.upper.y() - itr->y() << ' ';
        itr++;

        // Line to commands
        while (itr != end)
        {
            file << "L " << itr->x() - bbox.lower.x()
                 << ' '  << bbox.upper.y() - itr->y() << ' ';
            ++itr;
        }

        if (closed)
        {
            file << "Z";
        }
        file << "\"\nfill=\"none\" stroke=\"black\" stroke-width=\"0.01\"/>";
    }
    file << "\n</svg>";
    return true;
}


}   // namespace Kernel

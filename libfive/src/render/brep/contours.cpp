/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#include <fstream>
#include <boost/algorithm/string/predicate.hpp>

#include "libfive/eval/evaluator.hpp"

#include "libfive/render/brep/contours.hpp"
#include "libfive/render/brep/brep.hpp"
#include "libfive/render/brep/dc/dc_worker_pool.hpp"
#include "libfive/render/brep/dc/dc_contourer.hpp"
#include "libfive/render/brep/dual.hpp"

namespace libfive {

////////////////////////////////////////////////////////////////////////////////

std::unique_ptr<Contours> Contours::render(
        const Tree t, const Region<2>& r,
        const BRepSettings& settings)
{
    std::vector<Evaluator, Eigen::aligned_allocator<Evaluator>> es;
    es.reserve(settings.workers);
    for (unsigned i=0; i < settings.workers; ++i)
    {
        es.emplace_back(Evaluator(t));
    }

    // Create the quadtree on the scaffold
    auto xtree = DCWorkerPool<2>::build(es.data(), r, settings);

    // Abort early if the cancellation flag is set
    if (settings.cancel == true) {
        return nullptr;
    }

    // Perform marching squares, collecting into Contours
    auto cs = Dual<2>::walk<DCContourer>(xtree, settings);
    cs->bbox = r;
    return cs;
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

void Contours::collect(const std::vector<PerThreadBRep<2>>& children)
{
    // First, collect into a single b-rep, with unique indices
    BRep<2> segs;
    segs.collect(children);

    // Maps from index to item in segments vector
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
        this->contours.push_back(std::vector<Eigen::Vector2f>());

        // Weld multiple contours together here
        unsigned target = i;
        while (true)
        {
            for (const auto& pt : contours[target])
            {
                this->contours.back().push_back(segs.verts[pt]);
            }
            processed[target] = true;

            // Check for the next potentially-connected contour
            auto h = heads.find(contours[target].back());
            if (h != heads.end() && !processed[h->second])
            {
                // Remove the back point, beause it will be re-inserted
                // as the first point in the next contour
                this->contours.back().pop_back();
                target = h->second;
            }
            else
            {
                break;
            }
        }
    }
}

}   // namespace libfive

#include <fstream>

#include "ao/render/brep/contours.hpp"
#include "ao/render/brep/xtree.hpp"
#include "ao/render/brep/scaffold.hpp"
#include "ao/render/brep/ms.hpp"
#include "ao/render/brep/dual.hpp"

namespace Kernel {

std::unique_ptr<Contours> Contours::render(const Tree t, const Region<2>& r)
{
    std::unique_ptr<Evaluator> eval(new Evaluator(t));

    // Create a padded scaffolding for the XTree
    const auto scaffold = Scaffold<2>(eval.get(), r, 4, true);

    // Create the quadtree on the scaffold
    auto xtree = XTree<2>(eval.get(), scaffold);

    // Perform marching squares
    SquareMarcher ms(eval.get());
    Dual<2>::walk(xtree, ms);

    auto out = new Contours;
    out->bbox = r;

    // Maps from index (in ms) to item in segments vector
    std::map<uint32_t, uint32_t> heads;
    std::map<uint32_t, uint32_t> tails;
    std::vector<std::list<uint32_t>> contours;

    for (auto& s : ms.segments)
    {
        {   // Check to see whether we can attach to the back of a tail
            auto t = tails.find(s.first);
            if (t != tails.end())
            {
                contours[t->second].push_back(s.second);
                tails.insert({s.second, t->second});
                tails.erase(t);
                continue;
            }
        }

        {   // Otherwise, see if we should prepend ourselves to a head
            auto h = heads.find(s.second);
            if (h != heads.end())
            {
                contours[h->second].push_front(s.first);
                heads.insert({s.first, h->second});
                heads.erase(h);
                continue;
            }
        }

        // Otherwise, start a new multi-segment line
        heads[s.first] = contours.size();
        tails[s.second] = contours.size();
        contours.push_back({s.first, s.second});
    }

    std::vector<bool> processed(contours.size(), false);
    for (unsigned i=0; i < contours.size(); ++i)
    {
        if (processed[i])
        {
            continue;
        }
        out->contours.push_back(std::vector<Eigen::Vector2f>());

        // Weld multiple contours together here
        unsigned target = i;
        while (true)
        {
            for (const auto& pt : contours[target])
            {
                out->contours.back().push_back(ms.pts[pt]);
            }
            processed[target] = true;

            // Check for the next potentially-connected contour
            auto h = heads.find(contours[target].back());
            if (h != heads.end() && !processed[h->second])
            {
                // Remove the back point, beause it will be re-inserted
                // as the first point in the next contour
                out->contours.back().pop_back();
                target = h->second;
            }
            else
            {
                break;
            }
        }
    }

    return std::unique_ptr<Contours>(out);
}

bool Contours::saveSVG(std::string filename)
{
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
        "\" id=\"Ao\">\n";

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

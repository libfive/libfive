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

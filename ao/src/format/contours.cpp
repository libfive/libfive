#include <fstream>
#include <string>

#include "kernel/format/contours.hpp"

namespace Kernel {

void Contours::writeSVG(std::string filename, const Region& r)
{
    std::ofstream file;
    file.open(filename, std::ios::out);

    file <<
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
        "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"\n"
          " width=\"" << r.X.bounds.upper() - r.X.bounds.lower() <<
        "\" height=\"" << r.Y.bounds.upper() - r.Y.bounds.lower() <<
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
        file << "M " << itr->x - r.X.bounds.lower()
             << ' '  << r.Y.bounds.upper() - itr->y << ' ';
        itr++;

        // Line to commands
        while (itr != end)
        {
            file << "L " << itr->x - r.X.bounds.lower()
                 << ' '  << r.Y.bounds.upper() - itr->y << ' ';
            ++itr;
        }

        if (closed)
        {
            file << "Z";
        }
        file << "\"\nfill=\"none\" stroke=\"black\" stroke-width=\"0.01\"/>";
    }
    file << "\n</svg>";
}

}   // namespace Kernel

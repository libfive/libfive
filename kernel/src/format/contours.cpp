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
#include <fstream>
#include <string>

#include "ao/kernel/format/contours.hpp"

void Contours::writeSVG(std::string filename, const Region& r)
{
    std::ofstream file;
    file.open(filename, std::ios::out);

    file <<
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
        "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\"\n"
        "    viewBox=\"" <<
        r.X.bounds.lower() << ' ' << r.Y.bounds.lower() << ' ' <<
        r.X.bounds.upper() << ' ' << r.Y.bounds.upper() << ' ' <<
        "\" width=\"" << r.X.bounds.upper() - r.X.bounds.lower() <<
        "\" height=\"" << r.Y.bounds.upper() - r.Y.bounds.lower() << 
        "\" id=\"Ao\">\n";

    for (auto seg : contours)
    {
        file << "<polyline points=\"";

        auto itr = seg.begin();
        if (seg.front() == seg.back())
        {
            itr++;
        }
        while (itr != seg.end())
        {
            file << itr->x << "," << itr->y << " ";
            ++itr;
        }
        file << "\"\nstyle=\"fill:none,stroke:black;stroke-width:1\"/>";
    }
    file << "\n</svg>";
}

/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#pragma once

#include "libfive/tree/tree.hpp"
#include "libfive/render/brep/region.hpp"

namespace Kernel {

class Contours {
public:
    /*
     *  Basic render function
     */
    static std::unique_ptr<Contours> render(
            const Tree t, const Region<2>& r,
            double min_feature=0.1, double max_err=1e-8,
            bool multithread=true);

    /*
     *  Saves the contours to an SVG file
     */
    bool saveSVG(const std::string& filename);

    /*  Contours in 2D space  */
    std::vector<std::vector<Eigen::Vector2f>> contours;

    /*  Optional bounding box */
    Region<2> bbox;

protected:
    Contours(Region<2> bbox) : bbox(bbox) {}
};

}   // namespace Kernel

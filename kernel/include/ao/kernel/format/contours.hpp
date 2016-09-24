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
#pragma once

#include <vector>
#include <glm/vec2.hpp>

#include "ao/kernel/render/quadtree.hpp"
#include "ao/kernel/tree/tree.hpp"

class Region;

struct Contours
{
    static Contours Render(const Tree t, const Region& r);

    /*
     *  Saves the given contours as an SVG file
     */
    void writeSVG(std::string filename, const Region& r);

    /*  Contours in 2D space  */
    std::vector<std::vector<glm::vec2>> contours;
};

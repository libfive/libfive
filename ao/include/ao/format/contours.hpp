#pragma once

#include <vector>
#include <glm/vec2.hpp>

#include "ao/render/quadtree.hpp"
#include "ao/tree/tree.hpp"

namespace Kernel {

class Region;

struct Contours
{
    static Contours render(const Tree t, const Region& r);

    /*
     *  Saves the given contours as an SVG file
     */
    void writeSVG(std::string filename, const Region& r);

    /*  Contours in 2D space  */
    std::vector<std::vector<glm::vec2>> contours;
};

}   // namespace Kernel

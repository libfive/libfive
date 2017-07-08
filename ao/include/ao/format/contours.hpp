#pragma once

#include <vector>
#include <Eigen/Eigen>

#include "ao/render/quadtree.hpp"
#include "ao/tree/tree.hpp"

namespace Kernel {

class Region;

struct Contours
{
    static std::unique_ptr<Contours> render(const Tree t, const Region& r);

    /*
     *  Saves the given contours as an SVG file
     */
    void writeSVG(std::string filename, const Region& r);

    /*  Contours in 2D space  */
    std::vector<std::vector<Eigen::Vector2f>> contours;

    /*  Optional bounding box */
    Region bbox;
};

}   // namespace Kernel

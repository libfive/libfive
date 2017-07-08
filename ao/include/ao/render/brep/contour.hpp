#pragma once

#include "ao/tree/tree.hpp"
#include "ao/eval/evaluator.hpp"
#include "ao/render/brep/region.hpp"

namespace Kernel {

class Contour {
public:
    static std::unique_ptr<Contour> render(const Tree t, const Region<2>& r);

    /*
     *  Saves the depth component as a 16-bit single-channel PNG
     */
    bool saveSVG(std::string filename);

    /*  Contours in 2D space  */
    std::vector<std::vector<Eigen::Vector2f>> contours;

    /*  Optional bounding box */
    Region<2> bbox;
};

}   // namespace Kernel

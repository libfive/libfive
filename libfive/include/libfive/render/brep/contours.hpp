/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/tree/tree.hpp"
#include "libfive/render/brep/region.hpp"

#include <atomic>

namespace libfive {

// Forward declarations
template <unsigned N> class PerThreadBRep;
class Evaluator;
struct BRepSettings;

class Contours {
public:
    /*
     *  Generic render function
     */
    static std::unique_ptr<Contours> render(
        const Tree t, const Region<2>& r,
        const BRepSettings& settings);

    /*
     *  Saves the contours to an SVG file
     */
    bool saveSVG(const std::string& filename);

    /*
     *  Merge together a set of contours, welding continuous paths
     */
    void collect(const std::vector<PerThreadBRep<2>>& children);

    /*  Contours in 2D space  */
    std::vector<std::vector<Eigen::Vector2f>> contours;

    /*  Optional bounding box */
    Region<2> bbox;

    /*  Empty constructor, used prior to calling collect() */
    Contours() { /* Nothing to do here */ }

protected:
    Contours(Region<2> bbox) : bbox(bbox) {}
};

}   // namespace libfive

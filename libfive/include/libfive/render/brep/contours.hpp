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

namespace Kernel {

class Contours {
public:
    /*
     *  Basic render function
     */
    static std::unique_ptr<Contours> render(
        const Tree t, const Region<2>& r,
        double min_feature = 0.1, double max_err = 1e-8,
        bool multithread = true);

    static std::unique_ptr<Contours> render(
      const Tree t, const Region<2>& r,
      double min_feature, double max_err,
      int workers);

    static std::unique_ptr<Contours> render(
        const Tree t, const Region<2>& r,
        double min_feature, double max_err,
        std::atomic_bool& cancelled,
        int workers);

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

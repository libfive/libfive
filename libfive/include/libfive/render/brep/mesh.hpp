/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/tree/tree.hpp"

#include "libfive/render/brep/brep.hpp"

namespace libfive {

// Forward declaration
class Evaluator;
struct BRepSettings;

template <unsigned N> class Region;

class Mesh : public BRep<3> {
public:
    /*
     *  Core render function
     *
     *  Returns nullptr if min_feature is invalid or cancel is set to true
     *  partway through the computation.
     */
    static std::unique_ptr<Mesh> render(
            const Tree t, const Region<3>& r,
            const BRepSettings& settings);

    /*
     *  Render function that re-uses evaluators
     *  es must be a pointer to at least [settings.workers] Evaluators
     *
     *  Returns nullptr if min_feature is invalid or cancel is set to true
     *  partway through the computation.
     */
    static std::unique_ptr<Mesh> render(
            Evaluator* es, const Region<3>& r,
            const BRepSettings& settings);

    /*
     *  Writes the mesh to a file
     */
    bool saveSTL(const std::string& filename) const;

    /*
     *  Merge multiple bodies and write them to a single file
     */
    static bool saveSTL(const std::string& filename,
                        const std::list<const Mesh*>& meshes);

protected:

    /*
     *  Inserts a line into the mesh as a zero-size triangle
     *  (used for debugging)
     */
    void line(const Eigen::Vector3f& a, const Eigen::Vector3f& b);

};

}   // namespace libfive

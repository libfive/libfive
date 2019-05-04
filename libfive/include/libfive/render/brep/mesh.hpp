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
#include "libfive/render/brep/progress.hpp"
#include "libfive/render/brep/algorithm.hpp"

namespace Kernel {

// Forward declaration
class XTreeEvaluator;
class FreeThreadHandler;

template <unsigned N> class Region;

class Mesh : public BRep<3> {
public:
    /*
     *  Blocking, unstoppable render function
     *  Returns nullptr if min_feature is invalid (i.e. <= 0)
     */
    static std::unique_ptr<Mesh> render(
            const Tree t, const Region<3>& r,
            double min_feature=0.1, double max_err=1e-8,
            bool multithread=true, BRepAlgorithm alg=DUAL_CONTOURING);

    /*
     *  Fully-specified render function
     *  Returns nullptr if min_feature is invalid or cancel is set to true
     *  partway through the computation.
     */
    static std::unique_ptr<Mesh> render(
            const Tree t, const std::map<Tree::Id, float>& vars,
            const Region<3>& r, double min_feature, double max_err,
            unsigned workers, std::atomic_bool& cancel,
            BRepAlgorithm alg=DUAL_CONTOURING,
            ProgressCallback progress_callback=EMPTY_PROGRESS_CALLBACK,
            FreeThreadHandler* free_thread_handler=nullptr);

    /*
     *  Render function that re-uses evaluators
     *  es must be a pointer to at least workers Evaluators
     *  Returns nullptr if min_feature is invalid or cancel is set to true
     *  partway through the computation.
     */
    static std::unique_ptr<Mesh> render(
            XTreeEvaluator* es, const Region<3>& r,
            double min_feature, double max_err,
            unsigned workers, std::atomic_bool& cancel,
            BRepAlgorithm alg=DUAL_CONTOURING,
            ProgressCallback progress_callback=EMPTY_PROGRESS_CALLBACK,
            FreeThreadHandler* free_thread_handler=nullptr);

    /*
     *  Writes the mesh to a file
     */
    bool saveSTL(const std::string& filename);

    /*
     *  Merge multiple bodies and write them to a single file
     */
    static bool saveSTL(const std::string& filename,
                        const std::list<const Mesh*>& meshes);

    static const float MAX_PROGRESS;

protected:

    /*
     *  Inserts a line into the mesh as a zero-size triangle
     *  (used for debugging)
     */
    void line(const Eigen::Vector3f& a, const Eigen::Vector3f& b);

};

}   // namespace Kernel

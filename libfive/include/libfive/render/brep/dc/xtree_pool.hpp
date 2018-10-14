/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include <memory>

#include "libfive/tree/tree.hpp"
#include "libfive/render/brep/region.hpp"
#include "libfive/render/brep/root.hpp"
#include "libfive/render/brep/progress.hpp"
#include "libfive/render/brep/dc/xtree.hpp"

namespace Kernel {

template <unsigned N>
struct XTreePool
{
    /*
     *  Simplified construction with fewer arguments, used in unit testing
     */
    static Root<XTree<N>, typename XTree<N>::Leaf> build(
            const Tree t, Region<N> region,
            double min_feature=0.1, double max_err=1e-8, unsigned workers=1,
            ProgressCallback progress_callback=EMPTY_PROGRESS_CALLBACK);

    /*
     *  Full-featured construction
     *
     *  eval must be the first item in an array of at least `workers` items
     */
    static Root<XTree<N>, typename XTree<N>::Leaf> build(
            XTreeEvaluator* eval, Region<N> region,
            double min_feature, double max_err,
            unsigned workers, std::atomic_bool& cancel,
            ProgressCallback callback=EMPTY_PROGRESS_CALLBACK);
};

extern template struct XTreePool<2>;
extern template struct XTreePool<3>;

}   // namespace Kernel

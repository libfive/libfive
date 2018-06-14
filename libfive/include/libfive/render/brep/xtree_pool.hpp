/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018  Matt Keeter

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

#include <memory>

#include <boost/lockfree/queue.hpp>

#include "libfive/tree/tree.hpp"
#include "libfive/render/brep/region.hpp"
#include "libfive/render/brep/xtree.hpp"

namespace Kernel {

template <unsigned N>
struct Task
{
    XTree<N>* target;
    std::shared_ptr<Tape> tape;
};

template <unsigned N>
struct XTreePool
{
    /*
     *  Simplified construction with fewer arguments, used in unit testing
     */
    static std::unique_ptr<XTree<N>> build(
            const Tree t, Region<N> region,
            double min_feature=0.1, double max_err=1e-8,
            unsigned workers=1);

    /*
     *  Full-featured construction
     *
     *  eval must be the first item in an array of at least `workers` items
     */
    static std::unique_ptr<XTree<N>> build(
            XTreeEvaluator* eval, Region<N> region,
            double min_feature, double max_err,
            unsigned workers, std::atomic_bool& cancel);

protected:
    static void run(
            XTreeEvaluator* eval, boost::lockfree::queue<Task<N>*>& tasks,
            const float min_feature, const float max_err,
            std::atomic_int& slots, std::atomic_bool& done,
            std::atomic_bool& cancel);
};

extern template struct XTreePool<2>;
extern template struct XTreePool<3>;

}   // namespace Kernel

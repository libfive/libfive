/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2017  Matt Keeter

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
#include <array>

#include "libfive/render/axes.hpp"

namespace Kernel {

// Forward declarations
template <unsigned N> class DCTree;
template <unsigned N> class PerThreadBRep;

struct IntersectionAligner {
    using Input = DCTree<3>;
    struct Output {
        void collect(const std::vector<PerThreadBRep<3>>& children)
        {
            (void)children;
            /* This is a dummy function to meet the requirements for Dual::walk */
        }
    };

    IntersectionAligner(PerThreadBRep<3>& m) { (void)m; }

    /*
     *  Called by Dual::walk to ensure that all copies of the intersection
     *  common to ts use the same shared_ptr.  Should be called before calling
     *  Dual::walk on the Mesh class when doing fan meshing.
     */
    template <Axis::Axis A>
    static void load(const std::array<const DCTree<3>*, 4>& ts);

protected:
    template <Axis::Axis A, bool D>
    static void load(const std::array<const DCTree<3>*, 4>& ts, unsigned index);
};

}   // namespace Kernel

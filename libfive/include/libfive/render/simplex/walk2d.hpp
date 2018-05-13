/*
libfive: a CAD kernel for modeling with implicit functions
Copyright (C) 2018 Matt Keeter

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

#include "libfive/render/brep/brep.hpp"
#include "libfive/render/brep/region.hpp"
#include "libfive/render/simplex/simplextree.hpp"

namespace Kernel {
class XTreeEvaluator;

std::pair<BRep<2>, std::unique_ptr<SimplexTree<2>>> walk2d(
        XTreeEvaluator* eval, Region<2> region,
        unsigned min_depth, unsigned max_depth, double max_err);

}   // namespace Kernel

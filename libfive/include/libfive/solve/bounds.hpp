/*
libfive: a CAD kernel for modeling with implicit functions

Copyright (C) 2017  Matt Keeter

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this file,
You can obtain one at http://mozilla.org/MPL/2.0/.
*/
#pragma once

#include "libfive/render/brep/region.hpp"
#include "libfive/eval/eval_interval.hpp"

namespace Kernel {

#ifdef ENABLE_FIND_BOUNDS_EXPERIMENTAL
Region<3> findBounds(const Tree& t);
Region<3> findBounds(const Tree& t, const std::map<Tree::Id, float>& vars);
Region<3> findBounds(IntervalEvaluator* eval);
#else
#error \
The findBounds API is experimental and only works for the simplest of shapes. \
If you still want to use it, please #define ENABLE_FIND_BOUNDS_EXPERIMENTAL.
#endif

}   // namespace Kernel

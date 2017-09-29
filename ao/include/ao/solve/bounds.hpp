#pragma once

#include "ao/render/brep/region.hpp"
#include "ao/eval/evaluator.hpp"

namespace Kernel {

Region<3> findBounds(const Tree& t);
Region<3> findBounds(const Tree& t, const std::map<Tree::Id, float>& vars);
Region<3> findBounds(Evaluator* eval);

}   // namespace Kernel

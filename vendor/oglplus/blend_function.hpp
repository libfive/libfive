/**
 *  @file oglplus/blend_function.hpp
 *  @brief OpenGL blending function and equation enumeration
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_BLEND_FUNC_1107121519_HPP
#define OGLPLUS_BLEND_FUNC_1107121519_HPP

#include <oglplus/enums/blend_function.hpp>
#include <oglplus/enums/blend_equation.hpp>
#include <oglplus/enums/blend_equation_advanced.hpp>

namespace oglplus {

/// Alternative name for the BlendFunction enumeration
typedef BlendFunction BlendFn;

/// Alternative name for the BlendEquation enumeration
typedef BlendEquation BlendEq;

/// Alternative name for the BlendEquationAdvanced enumeration
typedef BlendEquationAdvanced BlendEqAdv;

} // namespace oglplus

#endif // include guard

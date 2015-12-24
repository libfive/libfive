/**
 *  @file oglplus/renderbuffer_target.hpp
 *  @brief Renderbuffer bind target enumerations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_RENDERBUFFER_TARGET_1201201052_HPP
#define OGLPLUS_RENDERBUFFER_TARGET_1201201052_HPP

#include <oglplus/fwd.hpp>
#include <oglplus/enums/renderbuffer_target.hpp>

namespace oglplus {

#if !OGLPLUS_NO_ENUM_VALUE_CLASSES
#include <oglplus/enums/renderbuffer_target_class.ipp>
#endif

template <>
struct ObjectTargetTag<RenderbufferTarget>
{
	typedef tag::Renderbuffer Type;
};

} // namespace oglplus

#endif // include guard

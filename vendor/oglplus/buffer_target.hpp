/**
 *  @file oglplus/buffer_target.hpp
 *  @brief Buffer bind target enumerations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_BUFFER_TARGET_1201201052_HPP
#define OGLPLUS_BUFFER_TARGET_1201201052_HPP

#include <oglplus/fwd.hpp>
#include <oglplus/buffer_binding.hpp>
#include <oglplus/enums/buffer_target.hpp>
#include <oglplus/enums/buffer_indexed_target.hpp>

namespace oglplus {

#if !OGLPLUS_NO_ENUM_VALUE_CLASSES
#include <oglplus/enums/buffer_target_class.ipp>
#endif

#if !OGLPLUS_NO_ENUM_VALUE_CLASSES
#include <oglplus/enums/buffer_indexed_target_class.ipp>
#endif

template <>
struct ObjectTargetTag<BufferTarget>
{
	typedef tag::Buffer Type;
};

template <>
struct ObjectTargetTag<BufferIndexedTarget>
{
	typedef tag::Buffer Type;
};

} // namespace oglplus

#endif // include guard

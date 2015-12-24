/**
 *  @file oglplus/client/buffer_masking.hpp
 *  @brief Client buffer masking ops and status stack.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CLIENT_BUFFER_MASKING_1412071213_HPP
#define OGLPLUS_CLIENT_BUFFER_MASKING_1412071213_HPP

#include <oglplus/client/setting.hpp>
#include <oglplus/context/buffer_masking.hpp>

namespace oglplus {
namespace client {
namespace aux {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0

class ColorMaskIndexed
 : public SettingStack<context::RGBAMask, DrawBufferIndex>
{
private:
	static
	context::RGBAMask _do_get(DrawBufferIndex index)
	{
		if(GLuint(index) == 0)
		{
			return context::BufferMaskingState::ColorWriteMask();
		}
		else
		{
			return context::BufferMaskingState::ColorWriteMask(
				index
			);
		}
	}

	static
	void _do_set(context::RGBAMask value, DrawBufferIndex index)
	{
		if(GLuint(index) == 0)
		{
			context::BufferMaskingState::ColorMask(value);
		}
		else
		{
			context::BufferMaskingState::ColorMask(index, value);
		}
	}
public:
	ColorMaskIndexed(DrawBufferIndex index = 0)
	 : SettingStack<context::RGBAMask, DrawBufferIndex>(
		&_do_get,
		&_do_set,
		index
	)
	{ }
};

class ColorMask
 : public SettingStackIndexed<
	ColorMaskIndexed,
	context::RGBAMask,
	DrawBufferIndex
>
{ };

#else

class ColorMask
 : public SettingStack<context::RGBAMask, Nothing>
{
private:
	static
	context::RGBAMask _do_get(Nothing)
	{
		return context::BufferMaskingState::ColorMask();
	}

	static
	void _do_set(context::RGBAMask value, Nothing)
	{
		context::BufferClearingState::ColorWriteMask();
	}
public:
	ColorMask(void)
	 : SettingStack<context::RGBAValue, Nothing>(
		&_do_get,
		&_do_set
	)
	{ }
};

#endif

class DepthMask
 : public SettingStack<Boolean, Nothing>
{
private:
	static
	Boolean _do_get(Nothing)
	{
		return context::BufferMaskingState::DepthWriteMask();
	}

	static
	void _do_set(Boolean value, Nothing)
	{
		context::BufferMaskingState::DepthMask(value);
	}
public:
	DepthMask(void)
	 : SettingStack<Boolean, Nothing>(
		&_do_get,
		&_do_set
	)
	{ }
};

template <SingleFace F>
class StencilMask
 : public SettingStack<Boolean, Nothing>
{
private:
	static
	Boolean _do_get(Nothing)
	{
		return context::BufferMaskingState::StencilWriteMaskSingle(F);
	}

	static
	void _do_set(Boolean value, Nothing)
	{
		context::BufferMaskingState::StencilMaskSeparateSingle(F, value);
	}
public:
	StencilMask(void)
	 : SettingStack<Boolean, Nothing>(
		&_do_get,
		&_do_set
	)
	{ }
};

} // namespace aux

class BufferMaskingState
{
public:
	aux::ColorMask ColorMask;
	aux::DepthMask DepthMask;

	oglplus::enums::EnumToClass<
		Nothing,
		SingleFace,
		aux::StencilMask
	> StencilMask;
};

} // namespace client
} // namespace oglplus

#endif // include guard

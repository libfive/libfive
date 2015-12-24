/**
 *  @file oglplus/client/blending.hpp
 *  @brief Client blending ops and status stack.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CLIENT_BLENDING_1412071213_HPP
#define OGLPLUS_CLIENT_BLENDING_1412071213_HPP

#include <oglplus/client/setting.hpp>
#include <oglplus/context/blending.hpp>

namespace oglplus {
namespace client {
namespace aux {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0

class BlendEquationIndexed
 : public SettingStack<context::BlendEquationSeparate, DrawBufferIndex>
{
private:
	static
	context::BlendEquationSeparate _do_get(DrawBufferIndex index)
	{
		if(GLuint(index) == 0)
		{
			return context::BlendingState::BlendEquationSeparate();
		}
		else
		{
			return context::BlendingState::BlendEquationSeparate(
				index
			);
		}
	}

	static
	void _do_set(context::BlendEquationSeparate value, DrawBufferIndex index)
	{
		if(GLuint(index) == 0)
		{
			if(value.Separate())
			{
				context::BlendingState::BlendEquationSeparate(
					value
				);
			}
			else
			{
				context::BlendingState::BlendEquation(
					value.Alpha()
				);
			}
		}
		else
		{
			if(value.Separate())
			{
				context::BlendingState::BlendEquationSeparate(
					index,
					value
				);
			}
			else
			{
				context::BlendingState::BlendEquation(
					index,
					value.Alpha()
				);
			}
		}
	}
public:
	BlendEquationIndexed(DrawBufferIndex index = 0)
	 : SettingStack<context::BlendEquationSeparate, DrawBufferIndex>(
		&_do_get,
		&_do_set,
		index
	)
	{ }
};

class BlendEquation
 : public SettingStackIndexed<
	BlendEquationIndexed,
	context::BlendEquationSeparate,
	DrawBufferIndex
>
{ };

class BlendFunctionIndexed
 : public SettingStack<context::BlendFunctionSeparate, DrawBufferIndex>
{
private:
	static
	context::BlendFunctionSeparate _do_get(DrawBufferIndex index)
	{
		if(GLuint(index) == 0)
		{
			return context::BlendingState::BlendFuncSeparate();
		}
		else
		{
			return context::BlendingState::BlendFuncSeparate(
				index
			);
		}
	}

	static
	void _do_set(context::BlendFunctionSeparate value, DrawBufferIndex index)
	{
		if(GLuint(index) == 0)
		{
			if(value.Separate())
			{
				context::BlendingState::BlendFuncSeparate(
					value
				);
			}
			else
			{
				context::BlendingState::BlendFunc(
					value.SrcAlpha(),
					value.DstAlpha()
				);
			}
		}
		else
		{
			if(value.Separate())
			{
				context::BlendingState::BlendFuncSeparate(
					index,
					value
				);
			}
			else
			{
				context::BlendingState::BlendFunc(
					index,
					value.SrcAlpha(),
					value.DstAlpha()
				);
			}
		}
	}
public:
	BlendFunctionIndexed(DrawBufferIndex index = 0)
	 : SettingStack<context::BlendFunctionSeparate, DrawBufferIndex>(
		&_do_get,
		&_do_set,
		index
	)
	{ }
};

class BlendFunction
 : public SettingStackIndexed<
	BlendFunctionIndexed,
	context::BlendFunctionSeparate,
	DrawBufferIndex
>
{ };

#else

class BlendEquation
 : public SettingStack<context::BlendEquationSeparate, Nothing>
{
private:
	static
	context::BlendEquationSeparate _do_get(Nothing)
	{
		return context::BlendingState::BlendEquationSeparate();
	}

	static
	void _do_set(context::BlendEquationSeparate value, Nothing)
	{
		if(value.Separate())
		{
			context::BlendingState::BlendEquationSeparate(value);
		}
		else
		{
			context::BlendingState::BlendEquation(value.Alpha());
		}
	}
public:
	BlendEquation(void)
	 : SettingStack<context::BlendEquationSeparate, Nothing>(
		&_do_get,
		&_do_set
	)
	{ }
};

class BlendFunction
 : public SettingStack<context::BlendFunctionSeparate, Nothing>
{
private:
	static
	context::BlendFunctionSeparate _do_get(Nothing)
	{
		return context::BlendingState::BlendFuncSeparate();
	}

	static
	void _do_set(context::BlendFunctionSeparate value, Nothing)
	{
		if(value.Separate())
		{
			context::BlendingState::BlendFuncSeparate(value);
		}
		else
		{
			context::BlendingState::BlendFunc(
				value.SrcAlpha(),
				value.DstAlpha()
			);
		}
	}
public:
	BlendFunction(void)
	 : SettingStack<context::BlendFunctionSeparate, Nothing>(
		&_do_get,
		&_do_set
	)
	{ }
};
#endif

class BlendColor
 : public SettingStack<context::RGBAValue, Nothing>
{
private:
	static
	context::RGBAValue _do_get(Nothing)
	{
		return context::BlendingState::BlendColor();
	}

	static
	void _do_set(context::RGBAValue value, Nothing)
	{
		context::BlendingState::BlendColor(value);
	}
public:
	BlendColor(void)
	 : SettingStack<context::RGBAValue, Nothing>(
		&_do_get,
		&_do_set
	)
	{ }
};

} // namespace aux

class BlendingState
{
public:
	aux::BlendEquation BlendEquation;
	aux::BlendFunction BlendFunction;
	aux::BlendColor BlendColor;
};

using context::BlendingOps;

} // namespace client
} // namespace oglplus

#endif // include guard

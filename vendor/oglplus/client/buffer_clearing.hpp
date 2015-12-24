/**
 *  @file oglplus/client/buffer_clearing.hpp
 *  @brief Client buffer clearing ops and status stack.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CLIENT_BUFFER_CLEARING_1412071213_HPP
#define OGLPLUS_CLIENT_BUFFER_CLEARING_1412071213_HPP

#include <oglplus/client/setting.hpp>
#include <oglplus/context/buffer_clearing.hpp>

namespace oglplus {
namespace client {
namespace aux {

class ClearColor
 : public SettingStack<context::RGBAValue, Nothing>
{
private:
	static
	context::RGBAValue _do_get(Nothing)
	{
		return context::BufferClearingState::ColorClearValue();
	}

	static
	void _do_set(context::RGBAValue value, Nothing)
	{
		context::BufferClearingState::ClearColor(value);
	}
public:
	ClearColor(void)
	 : SettingStack<context::RGBAValue, Nothing>(
		&_do_get,
		&_do_set
	)
	{ }
};

class ClearDepth
 : public SettingStack<GLfloat, Nothing>
{
private:
	static
	GLfloat _do_get(Nothing)
	{
		return context::BufferClearingState::DepthClearValue();
	}

	static
	void _do_set(GLfloat value, Nothing)
	{
		context::BufferClearingState::ClearDepth(value);
	}
public:
	ClearDepth(void)
	 : SettingStack<GLfloat, Nothing>(
		&_do_get,
		&_do_set
	)
	{ }
};

class ClearStencil
 : public SettingStack<GLint, Nothing>
{
private:
	static
	GLint _do_get(Nothing)
	{
		return context::BufferClearingState::StencilClearValue();
	}

	static
	void _do_set(GLint value, Nothing)
	{
		context::BufferClearingState::ClearStencil(value);
	}
public:
	ClearStencil(void)
	 : SettingStack<GLint, Nothing>(
		&_do_get,
		&_do_set
	)
	{ }
};

} // namespace aux

class BufferClearingState
{
public:
	aux::ClearColor ClearColor;
	aux::ClearDepth ClearDepth;
	aux::ClearStencil ClearStencil;
};

using context::BufferClearingOps;

} // namespace client
} // namespace oglplus

#endif // include guard

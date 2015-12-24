/**
 *  @file oglplus/client/drawing.hpp
 *  @brief Drawing operations and settings stack
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CLIENT_DRAWING_1412071213_HPP
#define OGLPLUS_CLIENT_DRAWING_1412071213_HPP

#include <oglplus/client/setting.hpp>
#include <oglplus/context/drawing.hpp>

namespace oglplus {
namespace client {
namespace aux {

#if GL_VERSION_3_1
class PrimitiveRestartIndex
 : public SettingStack<GLint, Nothing>
{
private:
	static
	GLint _do_get(Nothing)
	{
		return context::DrawingState::PrimitiveRestartIndex();
	}

	static
	void _do_set(GLint val, Nothing)
	{
		context::DrawingState::PrimitiveRestartIndex(val);
	}
public:
	PrimitiveRestartIndex(void)
	 : SettingStack<GLint, Nothing>(&_do_get, &_do_set)
	{ }
};
#endif

} // namespace aux

class DrawingState
{
public:
	aux::PrimitiveRestartIndex PrimitiveRestartIndex;
};

using oglplus::context::DrawingOps;

} // namespace client
} // namespace oglplus

#endif // include guard

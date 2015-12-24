/**
 *  @file oglplus/client/clip_control.hpp
 *  @brief Client clip control status stack.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CLIENT_CLIP_CONTROL_1412071213_HPP
#define OGLPLUS_CLIENT_CLIP_CONTROL_1412071213_HPP

#include <oglplus/client/setting.hpp>
#include <oglplus/context/clip_control.hpp>

namespace oglplus {
namespace client {
namespace aux {

#if GL_VERSION_4_5

class ClipControl
 : public SettingStack<context::ClipControlParams, Nothing>
{
private:
	static
	context::ClipControlParams _do_get(Nothing)
	{
		return context::ClipControlState::ClipControl();
	}

	static
	void _do_set(context::ClipControlParams value, Nothing)
	{
		context::ClipControlState::ClipControl(value);
	}
public:
	ClipControl(void)
	 : SettingStack<context::ClipControlParams, Nothing>(
		&_do_get,
		&_do_set
	)
	{ }
};

#endif

} // namespace aux

class ClipControlState
{
public:
#if GL_VERSION_4_5
	aux::ClipControl ClipControl;
#endif
};

} // namespace client
} // namespace oglplus

#endif // include guard

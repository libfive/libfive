/**
 *  @file oglplus/client/scissor_test.hpp
 *  @brief Client current scissor test setting stack
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CLIENT_SCISSOR_TEST_1412071213_HPP
#define OGLPLUS_CLIENT_SCISSOR_TEST_1412071213_HPP

#include <oglplus/client/setting.hpp>
#include <oglplus/context/scissor_test.hpp>

namespace oglplus {
namespace client {
namespace aux {

#if GL_VERSION_4_1 || GL_ARB_viewport_array

class ScissorIndexed
 : public SettingStack<context::ScissorRectangle, ViewportIndex>
{
private:
	static
	context::ScissorRectangle _do_get(ViewportIndex index)
	{
		return context::ScissorTest::ScissorBox(index);
	}

	static
	void _do_set(context::ScissorRectangle vp, ViewportIndex index)
	{
		context::ScissorTest::Scissor(index, vp);
	}
public:
	ScissorIndexed(ViewportIndex index)
	 : SettingStack<context::ScissorRectangle, ViewportIndex>(
		&_do_get,
		&_do_set,
		index
	)
	{ }
};

class Scissor
 : public SettingStackIndexed<
	ScissorIndexed,
	context::ScissorRectangle,
	ViewportIndex
>
{ };

#else

class Scissor
 : public SettingStack<context::ScissorRectangle, Nothing>
{
private:
	static
	context::ScissorRectangle _do_get(Nothing)
	{
		return context::ScissorTest::ScissorBox();
	}

	static
	void _do_set(context::ScissorRectangle vp, Nothing)
	{
		context::ScissorTest::Scissor(vp);
	}
public:
	Scissor(void)
	 : SettingStack<context::ScissorRectangle, Nothing>(
		&_do_get,
		&_do_set
	)
	{ }
};

#endif

} // namespace aux

class ScissorTestState
{
public:
	aux::Scissor Scissor;
};

} // namespace client
} // namespace oglplus

#endif // include guard

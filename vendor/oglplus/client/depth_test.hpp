/**
 *  @file oglplus/client/depth_test.hpp
 *  @brief Client current depth test setting stack
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CLIENT_DEPTH_TEST_1412071213_HPP
#define OGLPLUS_CLIENT_DEPTH_TEST_1412071213_HPP

#include <oglplus/client/setting.hpp>
#include <oglplus/context/depth_test.hpp>

namespace oglplus {
namespace client {
namespace aux {

class DepthFunc
 : public SettingStack<CompareFunction, Nothing>
{
private:
	static
	CompareFunction _do_get(Nothing)
	{
		return context::DepthTest::DepthFunc();
	}

	static
	void _do_set(CompareFunction func, Nothing)
	{
		context::DepthTest::DepthFunc(func);
	}
public:
	DepthFunc(void)
	 : SettingStack<CompareFunction, Nothing>(&_do_get, &_do_set)
	{ }
};

} // namespace aux

class DepthTestState
{
public:
	aux::DepthFunc DepthFunc;
};

} // namespace client
} // namespace oglplus

#endif // include guard

/**
 *  @file oglplus/client/stencil_test.hpp
 *  @brief Client current stencil test setting stack
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CLIENT_STENCIL_TEST_1412071213_HPP
#define OGLPLUS_CLIENT_STENCIL_TEST_1412071213_HPP

#include <oglplus/client/setting.hpp>
#include <oglplus/context/stencil_test.hpp>

namespace oglplus {
namespace client {
namespace aux {

template <SingleFace F>
class StencilFunc
 : public SettingStack<context::StencilFuncArgs, Nothing>
{
private:
	static
	context::StencilFuncArgs _do_get(Nothing)
	{
		return context::StencilTest::StencilFuncArgsSingle(F);
	}

	static
	void _do_set(context::StencilFuncArgs val, Nothing)
	{
		context::StencilTest::StencilFuncSeparateSingle(F, val);
	}
public:
	StencilFunc(void)
	 : SettingStack<context::StencilFuncArgs, Nothing>(
		&_do_get,
		&_do_set
	)
	{ }
};

template <SingleFace F>
class StencilOp
 : public SettingStack<context::StencilOperations, Nothing>
{
private:
	static
	context::StencilOperations _do_get(Nothing)
	{
		return context::StencilTest::StencilOpsSingle(F);
	}

	static
	void _do_set(context::StencilOperations val, Nothing)
	{
		context::StencilTest::StencilOpSeparateSingle(F, val);
	}
public:
	StencilOp(void)
	 : SettingStack<context::StencilOperations, Nothing>(
		&_do_get,
		&_do_set
	)
	{ }
};

} // namespace aux

class StencilTestState
{
public:
	oglplus::enums::EnumToClass<
		Nothing,
		SingleFace,
		aux::StencilFunc
	> StencilFunc;

	oglplus::enums::EnumToClass<
		Nothing,
		SingleFace,
		aux::StencilOp
	> StencilOp;
};

} // namespace client
} // namespace oglplus

#endif // include guard

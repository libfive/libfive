/**
 *  @file oglplus/client/logic_ops.hpp
 *  @brief Client current logical operations setting stack
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CLIENT_LOGIC_OPS_1412071213_HPP
#define OGLPLUS_CLIENT_LOGIC_OPS_1412071213_HPP

#include <oglplus/client/setting.hpp>
#include <oglplus/context/logic_ops.hpp>

namespace oglplus {
namespace client {
namespace aux {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
class LogicOp
 : public SettingStack<ColorLogicOperation, Nothing>
{
private:
	static
	ColorLogicOperation _do_get(Nothing)
	{
		return context::LogicOpState::LogicOpMode();
	}

	static
	void _do_set(ColorLogicOperation op, Nothing)
	{
		context::LogicOpState::LogicOp(op);
	}
public:
	LogicOp(void)
	 : SettingStack<ColorLogicOperation, Nothing>(&_do_get, &_do_set)
	{ }
};
#endif

} // namespace aux

class LogicOpState
{
public:
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	aux::LogicOp LogicOp;
#endif
};

} // namespace client
} // namespace oglplus

#endif // include guard

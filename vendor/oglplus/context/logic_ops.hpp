/**
 *  @file oglplus/context/logic_ops.hpp
 *  @brief Wrappers for color logical operations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_LOGIC_OPS_1201040722_HPP
#define OGLPLUS_CONTEXT_LOGIC_OPS_1201040722_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/color_logic_operation.hpp>

namespace oglplus {
namespace context {

/// Wrapper for the color buffer logical operations
/**
 *  @ingroup ogl_context
 */
class LogicOpState
{
public:
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Sets the color logical operation
	/**
	 *  @glsymbols
	 *  @glfunref{LogicOp}
	 */
	static void LogicOp(ColorLogicOperation op)
	{
		OGLPLUS_GLFUNC(LogicOp)(GLenum(op));
		OGLPLUS_VERIFY(
			LogicOp,
			Error,
			EnumParam(op)
		);
	}

	/// Returns the color logical operation
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{LOGIC_OP_MODE}
	 */
	static ColorLogicOperation LogicOpMode(void)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_LOGIC_OP_MODE, &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return ColorLogicOperation(result);
	}
#endif // GL_VERSION_3_0
};

} // namespace context
} // namespace oglplus

#endif // include guard

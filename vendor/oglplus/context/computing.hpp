/**
 *  @file oglplus/context/computing.hpp
 *  @brief Wrappers for general purpose computing operations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_COMPUTING_1201040722_HPP
#define OGLPLUS_CONTEXT_COMPUTING_1201040722_HPP

#include <oglplus/glfunc.hpp>

namespace oglplus {
namespace context {

/// Wrapper for the general-purpose-computing-related operations
/**
 *  @ingroup ogl_context
 */
class ComputingOps
{
public:
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3
	/// Launches several work groups
	/**
	 *  @glsymbols
	 *  @glfunref{DispatchCompute}
	 */
	static void DispatchCompute(
		GLuint num_groups_x,
		GLuint num_groups_y,
		GLuint num_groups_z
	)
	{
		OGLPLUS_GLFUNC(DispatchCompute)(
			num_groups_x,
			num_groups_y,
			num_groups_z
		);
		OGLPLUS_CHECK_SIMPLE(DispatchCompute);
	}

	/// Launches indirectly several work groups
	/**
	 *  @glsymbols
	 *  @glfunref{DispatchComputeIndirect}
	 */
	static void DispatchComputeIndirect(GLintptr indirect)
	{
		OGLPLUS_GLFUNC(DispatchComputeIndirect)(indirect);
		OGLPLUS_CHECK_SIMPLE(DispatchComputeIndirect);
	}
#endif
};

} // namespace context
} // namespace oglplus

#endif // include guard

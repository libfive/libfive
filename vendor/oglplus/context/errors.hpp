/**
 *  @file oglplus/context/errors.hpp
 *  @brief Wrappers for error reporting operations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_ERRORS_1201040722_HPP
#define OGLPLUS_CONTEXT_ERRORS_1201040722_HPP

#include <oglplus/enumerations.hpp>
#include <oglplus/glfunc.hpp>
#include <oglplus/error/code.hpp>

namespace oglplus {
namespace context {

/// Wrapper for the error-reporting-related operations
/**
 *  @ingroup ogl_context
 */
class Errors
{
public:
	/// returns the error code
	/**
	 *  @glsymbols
	 *  @glfunref{GetError}
	 */
	static ErrorCode GetError(void)
	{
		return ErrorCode(OGLPLUS_GLFUNC(GetError)());
	}
};

} // namespace context
} // namespace oglplus

#endif // include guard

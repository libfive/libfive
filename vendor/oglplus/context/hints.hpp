/**
 *  @file oglplus/context/hints.hpp
 *  @brief Wrappers for hints
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_HINTS_1201040722_HPP
#define OGLPLUS_CONTEXT_HINTS_1201040722_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/hint.hpp>

namespace oglplus {
namespace context {

/// Wrapper for the hint-related operations
/**
 *  @ingroup ogl_context
 */
class Hints
{
public:
	/// Selects a hint @p option for a @p target
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{Hint}
	 */
	static void Hint(HintTarget target, HintOption option)
	{
		OGLPLUS_GLFUNC(Hint)(GLenum(target), GLenum(option));
		OGLPLUS_VERIFY_SIMPLE(Hint);
	}

	/// Queries the current hint for a @p target
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{Hint}
	 */
	static HintOption Hint(HintTarget target)
	{
		GLint result = 0;
		OGLPLUS_GLFUNC(GetIntegerv)(GLenum(target), &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return HintOption(GLenum(result));
	}
};

} // namespace context
} // namespace oglplus

#endif // include guard

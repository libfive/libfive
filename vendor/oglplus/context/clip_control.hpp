/**
 *  @file oglplus/context/clip_control.hpp
 *  @brief Wrappers clip control operations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_CLIP_CONTROL_1201040722_HPP
#define OGLPLUS_CONTEXT_CLIP_CONTROL_1201040722_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/clip_control.hpp>

namespace oglplus {
namespace context {

struct ClipControlParams
{
	GLenum _origin;
	GLenum _depth;

	ClipControlParams(void)
	OGLPLUS_NOEXCEPT(true)
	{ }

	ClipControlParams(ClipOrigin origin, ClipDepthMode depth)
	OGLPLUS_NOEXCEPT(true)
	 : _origin(GLenum(origin))
	 , _depth(GLenum(depth))
	{ }

	ClipOrigin Origin(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return ClipOrigin(_origin);
	}

	ClipDepthMode DepthMode(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return ClipDepthMode(_depth);
	}
};

/// Wrapper for the clip control-related operations
/**
 *  @ingroup ogl_context
 */
class ClipControlState
{
public:
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_5 || GL_ARB_clip_control
	/// Sets the clipping mode
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{ClipControl}
	 */
	static void ClipControl(ClipOrigin origin, ClipDepthMode depth)
	{
		OGLPLUS_GLFUNC(ClipControl)(GLenum(origin), GLenum(depth));
		OGLPLUS_VERIFY_SIMPLE(ClipControl);
	}

	static void ClipControl(const ClipControlParams& params)
	{
		OGLPLUS_GLFUNC(ClipControl)(params._origin, params._depth);
		OGLPLUS_VERIFY_SIMPLE(ClipControl);
	}

	/// Queries the current clip origin setting
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{Get}
	 */
	static oglplus::ClipOrigin ClipOrigin(void)
	{
		GLint result = 0;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_CLIP_ORIGIN, &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return oglplus::ClipOrigin(GLenum(result));
	}

	/// Queries the current clip depth mode setting
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{Get}
	 */
	static oglplus::ClipDepthMode ClipDepthMode(void)
	{
		GLint result = 0;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_CLIP_DEPTH_MODE, &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return oglplus::ClipDepthMode(GLenum(result));
	}

	static ClipControlParams ClipControl(void)
	{
		return ClipControlParams(
			ClipOrigin(),
			ClipDepthMode()
		);
	}
#endif
};

} // namespace context
} // namespace oglplus

#endif // include guard

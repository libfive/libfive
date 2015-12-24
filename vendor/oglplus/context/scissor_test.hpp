/**
 *  @file oglplus/context/scissor_test.hpp
 *  @brief Wrappers for scissor tests and operations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_SCISSOR_TEST_1201040722_HPP
#define OGLPLUS_CONTEXT_SCISSOR_TEST_1201040722_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/context/viewport.hpp>

namespace oglplus {
namespace context {

/// Helper structure storing the extents of a 2D scissor rectangle
typedef ViewportExtents ScissorRectangle;

/// Wrapper for the scissor-buffer-related operations
/**
 *  @ingroup ogl_context
 */
class ScissorTest
{
public:
	/// Defines the scissor rectangle for the first viewport
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{Scissor}
	 */
	static void Scissor(
		GLint left,
		GLint bottom,
		SizeType width,
		SizeType height
	)
	{
		OGLPLUS_GLFUNC(Scissor)(left, bottom, width, height);
		OGLPLUS_VERIFY_SIMPLE(Scissor);
	}

	static void Scissor(const ScissorRectangle& rect)
	{
		OGLPLUS_GLFUNC(Scissor)(
			rect.Left(),
			rect.Bottom(),
			rect.Width(),
			rect.Height()
		);
		OGLPLUS_VERIFY_SIMPLE(Scissor);
	}

	/// Returns the extents of the scissor box
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{SCISSOR_BOX}
	 */
	static ScissorRectangle ScissorBox(void)
	{
		ScissorRectangle result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			GL_SCISSOR_BOX,
			result._v
		);
		OGLPLUS_CHECK_SIMPLE(GetIntegerv);
		return result;
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_1 || GL_ARB_viewport_array
	/// Defines the scissor rectangle for the specified @p viewport
	/**
	 *  @throws Error
	 *
	 *  @glvoereq{4,1,ARB,viewport_array}
	 *  @glsymbols
	 *  @glfunref{ScissorIndexed}
	 */
	static void Scissor(
		ViewportIndex viewport,
		GLint left,
		GLint bottom,
		SizeType width,
		SizeType height
	)
	{
		OGLPLUS_GLFUNC(ScissorIndexed)(
			GLuint(viewport),
			left,
			bottom,
			width,
			height
		);
		OGLPLUS_CHECK(
			ScissorIndexed,
			Error,
			Index(GLuint(viewport))
		);
	}

	static void Scissor(
		ViewportIndex viewport,
		const ScissorRectangle& rect
	)
	{
		OGLPLUS_GLFUNC(ScissorIndexed)(
			GLuint(viewport),
			rect.Left(),
			rect.Bottom(),
			rect.Width(),
			rect.Height()
		);
		OGLPLUS_CHECK(
			ScissorIndexed,
			Error,
			Index(GLuint(viewport))
		);
	}

	/// Defines the scissor rectangle for the specified @p viewport
	/**
	 *  @glvoereq{4,1,ARB,viewport_array}
	 *  @glsymbols
	 *  @glfunref{ScissorIndexedv}
	 */
	static void Scissor(ViewportIndex viewport, GLint* v)
	{
		OGLPLUS_GLFUNC(ScissorIndexedv)(GLuint(viewport), v);
		OGLPLUS_CHECK(
			ScissorIndexedv,
			Error,
			Index(GLuint(viewport))
		);
	}

	/// Defines scissor boxes for viewports specified by @p first @p count
	/**
	 *  @throws Error
	 *
	 *  @glvoereq{4,1,ARB,viewport_array}
	 *  @glsymbols
	 *  @glfunref{ScissorArrayv}
	 */
	static void ScissorArray(GLuint first, SizeType count, GLint* v)
	{
		OGLPLUS_GLFUNC(ScissorArrayv)(first, count, v);
		OGLPLUS_CHECK_SIMPLE(ScissorArrayv);
	}

	/// Returns the extents of scissor box of the specified @p viewport
	/**
	 *  @throws Error
	 *
	 *  @glvoereq{4,1,ARB,viewport_array}
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{SCISSOR_BOX}
	 */
	static ScissorRectangle ScissorBox(ViewportIndex viewport)
	{
		ScissorRectangle result;
		OGLPLUS_GLFUNC(GetIntegeri_v)(
			GL_SCISSOR_BOX,
			GLuint(viewport),
			result._v
		);
		OGLPLUS_CHECK(
			GetIntegeri_v,
			Error,
			Index(GLuint(viewport))
		);
		return result;
	}
#endif
};

} // namespace context
} // namespace oglplus

#endif // include guard

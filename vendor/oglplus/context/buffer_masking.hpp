/**
 *  @file oglplus/context/buffer_masking.hpp
 *  @brief Wrappers for operations for fine control of buffer updates
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_BUFFER_MASKING_1201040722_HPP
#define OGLPLUS_CONTEXT_BUFFER_MASKING_1201040722_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/boolean.hpp>
#include <oglplus/face_mode.hpp>
#include <oglplus/context/color.hpp>
#include <oglplus/draw_buffer_index.hpp>

namespace oglplus {
namespace context {

/// Wrappers for operations for fine control of buffer updates
/**
 *  @ingroup ogl_context
 */
class BufferMaskingState
{
public:
	/// Sets the color mask
	/**
	 *  @glsymbols
	 *  @glfunref{ColorMask}
	 */
	static void ColorMask(Boolean r, Boolean g, Boolean b, Boolean a)
	{
		OGLPLUS_GLFUNC(ColorMask)(
			r._get(),
			g._get(),
			b._get(),
			a._get()
		);
		OGLPLUS_VERIFY_SIMPLE(ColorMask);
	}

	static void ColorMask(const RGBAMask& m)
	{
		OGLPLUS_GLFUNC(ColorMask)(
			GLboolean(m._v[0]),
			GLboolean(m._v[1]),
			GLboolean(m._v[2]),
			GLboolean(m._v[3])
		);
		OGLPLUS_VERIFY_SIMPLE(ColorMask);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Sets the color mask for a particular @p buffer
	/**
	 *  @glsymbols
	 *  @glfunref{ColorMaski}
	 */
	static void ColorMask(
		DrawBufferIndex buffer,
		Boolean r,
		Boolean g,
		Boolean b,
		Boolean a
	)
	{
		OGLPLUS_GLFUNC(ColorMaski)(
			GLuint(buffer),
			r._get(),
			g._get(),
			b._get(),
			a._get()
		);
		OGLPLUS_VERIFY(
			ColorMaski,
			Error,
			Index(GLuint(buffer))
		);
	}

	static void ColorMask(DrawBufferIndex buffer, const RGBAMask& m)
	{
		OGLPLUS_GLFUNC(ColorMaski)(
			GLuint(buffer),
			GLboolean(m._v[0]),
			GLboolean(m._v[1]),
			GLboolean(m._v[2]),
			GLboolean(m._v[3])
		);
		OGLPLUS_VERIFY(
			ColorMaski,
			Error,
			Index(GLuint(buffer))
		);
	}
#endif

	/// Sets the depth @p mask
	/**
	 *  @glsymbols
	 *  @glfunref{DepthMask}
	 */
	static void DepthMask(Boolean mask)
	{
		OGLPLUS_GLFUNC(DepthMask)(mask._get());
		OGLPLUS_VERIFY_SIMPLE(DepthMask);
	}

	/// Sets the stencil @p mask
	/**
	 *  @glsymbols
	 *  @glfunref{StencilMask}
	 */
	static void StencilMask(GLuint mask)
	{
		OGLPLUS_GLFUNC(StencilMask)(mask);
		OGLPLUS_VERIFY_SIMPLE(StencilMask);
	}

	/// Sets the stencil mask separately for front and back faces
	/**
	 *  @glsymbols
	 *  @glfunref{StencilMaskSeparate}
	 */
	static void StencilMaskSeparate(Face face, GLuint mask)
	{
		OGLPLUS_GLFUNC(StencilMaskSeparate)(GLenum(face), mask);
		OGLPLUS_VERIFY_SIMPLE(StencilMaskSeparate);
	}

	static void StencilMaskSeparateSingle(SingleFace face, GLuint mask)
	{
		OGLPLUS_GLFUNC(StencilMaskSeparate)(GLenum(face), mask);
		OGLPLUS_VERIFY_SIMPLE(StencilMaskSeparate);
	}

	static oglplus::context::RGBAMask ColorWriteMask(void)
	{
		oglplus::context::RGBAMask result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			GL_COLOR_WRITEMASK,
			result._v
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return result;
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Returns the value of color buffer write mask
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{COLOR_WRITEMASK}
	 */
	static oglplus::context::RGBAMask ColorWriteMask(DrawBufferIndex buffer)
	{
		oglplus::context::RGBAMask result;
		OGLPLUS_GLFUNC(GetIntegeri_v)(
			GL_COLOR_WRITEMASK,
			GLuint(buffer),
			result._v
		);
		OGLPLUS_CHECK(
			GetIntegeri_v,
			Error,
			Index(GLuint(buffer))
		);
		return result;
	}
#endif

	/// Returns the value of depth buffer write mask
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{DEPTH_WRITEMASK}
	 */
	static Boolean DepthWriteMask(void)
	{
		Boolean result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			GL_DEPTH_WRITEMASK,
			result._ptr()
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return result;
	}

	/// Returns the value of stencil write mask
	/**
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{STENCIL_WRITEMASK}
	 *  @gldefref{STENCIL_BACK_WRITEMASK}
	 */
	static GLuint StencilWriteMask(bool backface = false)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			backface?
			GL_STENCIL_BACK_WRITEMASK:
			GL_STENCIL_WRITEMASK,
			&result
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return GLuint(result);
	}

	static GLuint StencilWriteMask(Face face)
	{
		return StencilWriteMask(face == Face::Back);
	}

	static GLuint StencilWriteMaskSingle(SingleFace face)
	{
		return StencilWriteMask(face == SingleFace::Back);
	}
};

} // namespace context
} // namespace oglplus

#endif // include guard

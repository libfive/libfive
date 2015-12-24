/**
 *  @file oglplus/context/blending.hpp
 *  @brief Wrappers for blending operations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_BLENDING_1201040722_HPP
#define OGLPLUS_CONTEXT_BLENDING_1201040722_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/blend_function.hpp>
#include <oglplus/draw_buffer_index.hpp>
#include <oglplus/context/color.hpp>

#ifdef RGB
#undef RGB
#endif

namespace oglplus {
namespace context {

struct BlendEquationSeparate
{
	GLint _v[2];

	BlendEquationSeparate(void)
	OGLPLUS_NOEXCEPT(true)
	{ }

	BlendEquationSeparate(BlendEquation rgb, BlendEquation alpha)
	OGLPLUS_NOEXCEPT(true)
	{
		_v[0] = GLint(GLenum(rgb));
		_v[1] = GLint(GLenum(alpha));
	}

	BlendEquation RGB(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return BlendEquation(GLenum(_v[0]));
	}

	BlendEquation Alpha(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return BlendEquation(GLenum(_v[1]));
	}

	bool Separate(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v[0] != _v[1];
	}

	friend
	bool operator == (
		const BlendEquationSeparate& a,
		const BlendEquationSeparate& b
	) OGLPLUS_NOEXCEPT(true)
	{
		return (a._v[0] == b._v[0]) && (a._v[1] == b._v[1]);
	}

	friend
	bool operator != (
		const BlendEquationSeparate& a,
		const BlendEquationSeparate& b
	) OGLPLUS_NOEXCEPT(true)
	{
		return (a._v[0] != b._v[0]) || (a._v[1] != b._v[1]);
	}
};

struct BlendFunctionSeparate
{
	GLint _v[4];

	BlendFunctionSeparate(void)
	OGLPLUS_NOEXCEPT(true)
	{ }

	BlendFunctionSeparate(
		BlendFunction src_rgb,
		BlendFunction src_alpha,
		BlendFunction dst_rgb,
		BlendFunction dst_alpha
	)
	OGLPLUS_NOEXCEPT(true)
	{
		_v[0] = GLint(GLenum(src_rgb));
		_v[1] = GLint(GLenum(src_alpha));
		_v[2] = GLint(GLenum(dst_rgb));
		_v[3] = GLint(GLenum(dst_alpha));
	}

	BlendFunction SrcRGB(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return BlendFunction(GLenum(_v[0]));
	}

	BlendFunction SrcAlpha(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return BlendFunction(GLenum(_v[1]));
	}

	BlendFunction DstRGB(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return BlendFunction(GLenum(_v[2]));
	}

	BlendFunction DstAlpha(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return BlendFunction(GLenum(_v[3]));
	}

	bool Separate(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return (_v[0] != _v[1]) || (_v[2] == _v[3]);
	}

	friend
	bool operator == (
		const BlendFunctionSeparate& a,
		const BlendFunctionSeparate& b
	) OGLPLUS_NOEXCEPT(true)
	{
		for(unsigned i=0; i<4; ++i)
		{
			if(a._v[i] != b._v[i])
			{
				return false;
			}
		}
		return true;
	}

	friend
	bool operator != (
		const BlendFunctionSeparate& a,
		const BlendFunctionSeparate& b
	) OGLPLUS_NOEXCEPT(true)
	{
		return !(a == b);
	}
};

/// Wrapper for blending operations
/**
 *  @ingroup ogl_context
 */
class BlendingOps
{
public:

#if OGLPLUS_DOCUMENTATION_ONLY || GL_KHR_blend_equation_advanced
	/// Specified boundaries between blending passes
	/**
	 *  @glsymbols
	 *  @glfunref{BlendBarrierKHR}
	 */
	static void BlendBarrier(void)
	{
		OGLPLUS_GLFUNC(BlendBarrierKHR)();
		OGLPLUS_VERIFY_SIMPLE(BlendBarrierKHR);
	}
#endif
};

/// Wrapper for blending operations
/**
 *  @ingroup ogl_context
 */
class BlendingState
{
public:
	/// Sets the blend equation
	/**
	 *  @glsymbols
	 *  @glfunref{BlendEquation}
	 */
	static void BlendEquation(
		OneOf<
			GLenum,
			std::tuple<
				oglplus::BlendEquation,
				oglplus::BlendEquationAdvanced
			>
		> eq
	)
	{
		OGLPLUS_GLFUNC(BlendEquation)(GLenum(eq));
		OGLPLUS_VERIFY_SIMPLE(BlendEquation);
	}

	/// Sets the blend equation separate for RGB and alpha
	/**
	 *  @glsymbols
	 *  @glfunref{BlendEquationSeparate}
	 */
	static void BlendEquationSeparate(
		oglplus::BlendEquation eq_rgb,
		oglplus::BlendEquation eq_alpha
	)
	{
		OGLPLUS_GLFUNC(BlendEquationSeparate)(
			GLenum(eq_rgb),
			GLenum(eq_alpha)
		);
		OGLPLUS_VERIFY_SIMPLE(BlendEquationSeparate);
	}

	static void BlendEquationSeparate(
		const oglplus::context::BlendEquationSeparate& eq
	)
	{
		OGLPLUS_GLFUNC(BlendEquationSeparate)(
			GLenum(eq._v[0]),
			GLenum(eq._v[1])
		);
		OGLPLUS_VERIFY_SIMPLE(BlendEquationSeparate);
	}

	static oglplus::context::BlendEquationSeparate BlendEquationSeparate(void)
	{
		oglplus::context::BlendEquationSeparate result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			GL_BLEND_EQUATION_RGB,
			&result._v[0]
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		OGLPLUS_GLFUNC(GetIntegerv)(
			GL_BLEND_EQUATION_ALPHA,
			&result._v[1]
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return result;
	}

	static oglplus::BlendEquation BlendEquationRGB(void)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_BLEND_EQUATION_RGB, &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return oglplus::BlendEquation(GLenum(result));
	}

	static oglplus::BlendEquation BlendEquationAlpha(void)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_BLEND_EQUATION_ALPHA, &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return oglplus::BlendEquation(GLenum(result));
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0
	/// Sets the blend equation for a particular draw @p buffer
	/**
	 *  @glverreq{4,0}
	 *  @glsymbols
	 *  @glfunref{BlendEquationi}
	 */
	static void BlendEquation(
		DrawBufferIndex buffer,
		OneOf<
			GLenum,
			std::tuple<
				oglplus::BlendEquation,
				oglplus::BlendEquationAdvanced
			>
		> eq
	)
	{
		OGLPLUS_GLFUNC(BlendEquationi)(GLuint(buffer), GLenum(eq));
		OGLPLUS_VERIFY(
			BlendEquationi,
			Error,
			Index(GLuint(buffer))
		);
	}

	/// Sets the blend equation separate for RGB and alpha for a @p buffer
	/**
	 *  @glverreq{4,0}
	 *  @glsymbols
	 *  @glfunref{BlendEquationSeparatei}
	 */
	static void BlendEquationSeparate(
		DrawBufferIndex buffer,
		oglplus::BlendEquation eq_rgb,
		oglplus::BlendEquation eq_alpha
	)
	{
		OGLPLUS_GLFUNC(BlendEquationSeparatei)(
			GLuint(buffer),
			GLenum(eq_rgb),
			GLenum(eq_alpha)
		);
		OGLPLUS_VERIFY(
			BlendEquationSeparatei,
			Error,
			Index(GLuint(buffer))
		);
	}

	static void BlendEquationSeparate(
		DrawBufferIndex buffer,
		const oglplus::context::BlendEquationSeparate& eq
	)
	{
		OGLPLUS_GLFUNC(BlendEquationSeparatei)(
			GLuint(buffer),
			GLenum(eq._v[0]),
			GLenum(eq._v[1])
		);
		OGLPLUS_VERIFY(
			BlendEquationSeparatei,
			Error,
			Index(GLuint(buffer))
		);
	}

	static oglplus::context::BlendEquationSeparate
	BlendEquationSeparate(DrawBufferIndex buffer)
	{
		oglplus::context::BlendEquationSeparate result;
		OGLPLUS_GLFUNC(GetIntegeri_v)(
			GL_BLEND_EQUATION_RGB,
			GLuint(buffer),
			&result._v[0]
		);
		OGLPLUS_CHECK(
			GetIntegeri_v,
			Error,
			Index(GLuint(buffer))
		);
		OGLPLUS_GLFUNC(GetIntegeri_v)(
			GL_BLEND_EQUATION_ALPHA,
			GLuint(buffer),
			&result._v[1]
		);
		OGLPLUS_CHECK(
			GetIntegeri_v,
			Error,
			Index(GLuint(buffer))
		);
		return result;
	}

	static oglplus::BlendEquation BlendEquationRGB(DrawBufferIndex buffer)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegeri_v)(
			GL_BLEND_EQUATION_RGB,
			GLuint(buffer),
			&result
		);
		OGLPLUS_CHECK(
			GetIntegeri_v,
			Error,
			Index(GLuint(buffer))
		);
		return oglplus::BlendEquation(GLenum(result));
	}

	static oglplus::BlendEquation BlendEquationAlpha(DrawBufferIndex buffer)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegeri_v)(
			GL_BLEND_EQUATION_ALPHA,
			GLuint(buffer),
			&result
		);
		OGLPLUS_CHECK(
			GetIntegeri_v,
			Error,
			Index(GLuint(buffer))
		);
		return oglplus::BlendEquation(GLenum(result));
	}
#endif

	/// Sets the blend function
	/**
	 *  @glsymbols
	 *  @glfunref{BlendFunc}
	 */
	static void BlendFunc(BlendFunction src, BlendFunction dst)
	{
		OGLPLUS_GLFUNC(BlendFunc)(GLenum(src), GLenum(dst));
		OGLPLUS_VERIFY_SIMPLE(BlendFunc);
	}

	/// Sets the blend function separate for RGB and alpha
	/**
	 *  @glsymbols
	 *  @glfunref{BlendFuncSeparate}
	 */
	static void BlendFuncSeparate(
		BlendFunction src_rgb,
		BlendFunction dst_rgb,
		BlendFunction src_alpha,
		BlendFunction dst_alpha
	)
	{
		OGLPLUS_GLFUNC(BlendFuncSeparate)(
			GLenum(src_rgb),
			GLenum(dst_rgb),
			GLenum(src_alpha),
			GLenum(dst_alpha)
		);
		OGLPLUS_VERIFY_SIMPLE(BlendFuncSeparate);
	}

	static void BlendFuncSeparate(
		const oglplus::context::BlendFunctionSeparate& fn
	)
	{
		OGLPLUS_GLFUNC(BlendFuncSeparate)(
			GLenum(fn._v[0]),
			GLenum(fn._v[1]),
			GLenum(fn._v[2]),
			GLenum(fn._v[3])
		);
		OGLPLUS_VERIFY_SIMPLE(BlendFuncSeparate);
	}

	static oglplus::context::BlendFunctionSeparate BlendFuncSeparate(void)
	{
		oglplus::context::BlendFunctionSeparate result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			GL_BLEND_SRC_RGB,
			&result._v[0]
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		OGLPLUS_GLFUNC(GetIntegerv)(
			GL_BLEND_SRC_ALPHA,
			&result._v[1]
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		OGLPLUS_GLFUNC(GetIntegerv)(
			GL_BLEND_DST_RGB,
			&result._v[2]
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		OGLPLUS_GLFUNC(GetIntegerv)(
			GL_BLEND_DST_ALPHA,
			&result._v[3]
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return result;
	}

	static oglplus::BlendFunction BlendFuncSrcRGB(void)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_BLEND_SRC_RGB, &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return oglplus::BlendFunction(GLenum(result));
	}

	static oglplus::BlendFunction BlendFuncSrcAlpha(void)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_BLEND_SRC_ALPHA, &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return oglplus::BlendFunction(GLenum(result));
	}

	static oglplus::BlendFunction BlendFuncDstRGB(void)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_BLEND_DST_RGB, &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return oglplus::BlendFunction(GLenum(result));
	}

	static oglplus::BlendFunction BlendFuncDstAlpha(void)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_BLEND_DST_ALPHA, &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return oglplus::BlendFunction(GLenum(result));
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0
	/// Sets the blend function for a particular @p buffer
	/**
	 *  @glverreq{4,0}
	 *  @glsymbols
	 *  @glfunref{BlendFunci}
	 */
	static void BlendFunc(
		DrawBufferIndex buffer,
		BlendFunction src,
		BlendFunction dst
	)
	{
		OGLPLUS_GLFUNC(BlendFunci)(
			GLuint(buffer),
			GLenum(src),
			GLenum(dst)
		);
		OGLPLUS_VERIFY(
			BlendFunci,
			Error,
			Index(GLuint(buffer))
		);
	}

	/// Sets the blend function separate for RGB and alpha for a @p buffer
	/**
	 *  @glverreq{4,0}
	 *  @glsymbols
	 *  @glfunref{BlendFuncSeparatei}
	 */
	static void BlendFuncSeparate(
		DrawBufferIndex buffer,
		BlendFunction src_rgb,
		BlendFunction dst_rgb,
		BlendFunction src_alpha,
		BlendFunction dst_alpha
	)
	{
		OGLPLUS_GLFUNC(BlendFuncSeparatei)(
			GLuint(buffer),
			GLenum(src_rgb),
			GLenum(dst_rgb),
			GLenum(src_alpha),
			GLenum(dst_alpha)
		);
		OGLPLUS_VERIFY(
			BlendFuncSeparatei,
			Error,
			Index(GLuint(buffer))
		);
	}

	static void BlendFuncSeparate(
		DrawBufferIndex buffer,
		const oglplus::context::BlendFunctionSeparate& eq
	)
	{
		OGLPLUS_GLFUNC(BlendFuncSeparatei)(
			GLuint(buffer),
			GLenum(eq._v[0]),
			GLenum(eq._v[1]),
			GLenum(eq._v[2]),
			GLenum(eq._v[3])
		);
		OGLPLUS_VERIFY(
			BlendFuncSeparatei,
			Error,
			Index(GLuint(buffer))
		);
	}

	static oglplus::context::BlendFunctionSeparate
	BlendFuncSeparate(DrawBufferIndex buffer)
	{
		oglplus::context::BlendFunctionSeparate result;
		OGLPLUS_GLFUNC(GetIntegeri_v)(
			GL_BLEND_SRC_RGB,
			GLuint(buffer),
			&result._v[0]
		);
		OGLPLUS_CHECK(
			GetIntegeri_v,
			Error,
			Index(GLuint(buffer))
		);
		OGLPLUS_GLFUNC(GetIntegeri_v)(
			GL_BLEND_SRC_ALPHA,
			GLuint(buffer),
			&result._v[1]
		);
		OGLPLUS_CHECK(
			GetIntegeri_v,
			Error,
			Index(GLuint(buffer))
		);
		OGLPLUS_GLFUNC(GetIntegeri_v)(
			GL_BLEND_DST_RGB,
			GLuint(buffer),
			&result._v[2]
		);
		OGLPLUS_CHECK(
			GetIntegeri_v,
			Error,
			Index(GLuint(buffer))
		);
		OGLPLUS_GLFUNC(GetIntegeri_v)(
			GL_BLEND_DST_ALPHA,
			GLuint(buffer),
			&result._v[3]
		);
		OGLPLUS_CHECK(
			GetIntegeri_v,
			Error,
			Index(GLuint(buffer))
		);
		return result;
	}

	static oglplus::BlendFunction BlendFuncSrcRGB(DrawBufferIndex buffer)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegeri_v)(
			GL_BLEND_SRC_RGB,
			GLuint(buffer),
			&result
		);
		OGLPLUS_CHECK(
			GetIntegeri_v,
			Error,
			Index(GLuint(buffer))
		);
		return oglplus::BlendFunction(GLenum(result));
	}

	static oglplus::BlendFunction BlendFuncSrcAlpha(DrawBufferIndex buffer)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegeri_v)(
			GL_BLEND_SRC_ALPHA,
			GLuint(buffer),
			&result
		);
		OGLPLUS_CHECK(
			GetIntegeri_v,
			Error,
			Index(GLuint(buffer))
		);
		return oglplus::BlendFunction(GLenum(result));
	}

	static oglplus::BlendFunction BlendFuncDstRGB(DrawBufferIndex buffer)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegeri_v)(
			GL_BLEND_DST_RGB,
			GLuint(buffer),
			&result
		);
		OGLPLUS_CHECK(
			GetIntegeri_v,
			Error,
			Index(GLuint(buffer))
		);
		return oglplus::BlendFunction(GLenum(result));
	}

	static oglplus::BlendFunction BlendFuncDstAlpha(DrawBufferIndex buffer)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegeri_v)(
			GL_BLEND_DST_ALPHA,
			GLuint(buffer),
			&result
		);
		OGLPLUS_CHECK(
			GetIntegeri_v,
			Error,
			Index(GLuint(buffer))
		);
		return oglplus::BlendFunction(GLenum(result));
	}
#endif

	/// Sets the blend color
	/**
	 *  @glsymbols
	 *  @glfunref{BlendColor}
	 */
	static void BlendColor(GLclampf r, GLclampf g, GLclampf b, GLclampf a)
	{
		OGLPLUS_GLFUNC(BlendColor)(r, g, b, a);
		OGLPLUS_VERIFY_SIMPLE(BlendColor);
	}

	static void BlendColor(const oglplus::context::RGBAValue& rgba)
	{
		OGLPLUS_GLFUNC(BlendColor)(
			rgba._v[0],
			rgba._v[1],
			rgba._v[2],
			rgba._v[3]
		);
		OGLPLUS_VERIFY_SIMPLE(BlendColor);
	}

	static oglplus::context::RGBAValue BlendColor(void)
	{
		oglplus::context::RGBAValue result;
		OGLPLUS_GLFUNC(GetFloatv)(
			GL_BLEND_COLOR,
			result._v
		);
		OGLPLUS_VERIFY_SIMPLE(GetFloatv);
		return result;
	}
};

} // namespace context
} // namespace oglplus

#endif // include guard

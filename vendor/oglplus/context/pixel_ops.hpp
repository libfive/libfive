/**
 *  @file oglplus/context/pixel_ops.hpp
 *  @brief Wrappers for pixel storage and transfer operations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_PIXEL_OPS_1201040722_HPP
#define OGLPLUS_CONTEXT_PIXEL_OPS_1201040722_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/pixel_parameter.hpp>
#include <oglplus/pixel_data.hpp>
#include <oglplus/blit_filter.hpp>
#include <oglplus/buffer_select_bit.hpp>
#include <oglplus/boolean.hpp>
#include <oglplus/size_type.hpp>
#include <oglplus/bitfield.hpp>

namespace oglplus {
namespace context {

/// Wrapper for the pixel storage and transfer operations
/**
 *  @ingroup ogl_context
 */
class PixelState
{
public:
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	static void PixelStore(
		PixelParameter parameter,
		GLfloat value,
		TypeTag<float>
	)
	{
		OGLPLUS_GLFUNC(PixelStoref)(GLenum(parameter), value);
		OGLPLUS_CHECK(
			PixelStoref,
			Error,
			EnumParam(parameter)
		);
	}

	static void PixelStore(
		PixelParameter parameter,
		GLint value,
		TypeTag<int>
	)
	{
		OGLPLUS_GLFUNC(PixelStorei)(GLenum(parameter), value);
		OGLPLUS_CHECK(
			PixelStorei,
			Error,
			EnumParam(parameter)
		);
	}

	static void PixelStore(
		PixelParameter parameter,
		Boolean value,
		TypeTag<bool>
	)
	{
		PixelStore(parameter, value._get(), TypeTag<int>());
	}

	static GLfloat PixelStoreValue(
		PixelParameter parameter,
		TypeTag<float>
	)
	{
		GLfloat result;
		OGLPLUS_GLFUNC(GetFloatv)(GLenum(parameter), &result);
		OGLPLUS_VERIFY_SIMPLE(GetFloatv);
		return result;
	}

	static GLint PixelStoreValue(
		PixelParameter parameter,
		TypeTag<int>
	)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(GLenum(parameter), &result);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return result;
	}

	static Boolean PixelStoreValue(
		PixelParameter parameter,
		TypeTag<bool>
	)
	{
		return Boolean(
			PixelStoreValue(parameter, TypeTag<int>()),
			std::nothrow
		);
	}

	/// Sets the @p value of a pixel storage @p parameter
	/**
	 *  @glsymbols
	 *  @glfunref{PixelStore}
	 */
	static void PixelStore(PixelParameter parameter, GLfloat value)
	{
		PixelStore(parameter, value, TypeTag<float>());
	}

	/// Sets the @p value of a pixel storage @p parameter
	/**
	 *  @glsymbols
	 *  @glfunref{PixelStore}
	 */
	static void PixelStore(PixelParameter parameter, GLint value)
	{
		PixelStore(parameter, value, TypeTag<int>());
	}

	template <PixelParameter Parameter>
	static void PixelStore(
		typename enums::EnumAssocType<
			PixelParameter,
			Parameter
		>::Type value
	)
	{
		PixelStore(
			Parameter,
			value,
			TypeTag<
				typename enums::EnumAssocType<
					PixelParameter,
					Parameter
				>::Type
			>()
		);
	}

	template <PixelParameter Parameter>
	static typename enums::EnumAssocType<
		PixelParameter,
		Parameter
	>::Type PixelStoreValue(void)
	{
		return PixelStoreValue(
			Parameter,
			TypeTag<
				typename enums::EnumAssocType<
					PixelParameter,
					Parameter
				>::Type
			>()
		);
	}
#endif // GL_VERSION_3_0
};

/// Wrapper for the pixel storage and transfer operations
/**
 *  @ingroup ogl_context
 */
class PixelOps
{
public:
	/// Reads the pixels from the current framebuffer into a host memory buffer
	/**
	 *  @glsymbols
	 *  @glfunref{ReadPixels}
	 */
	static void ReadPixels(
		GLint x,
		GLint y,
		SizeType width,
		SizeType height,
		PixelDataFormat format,
		PixelDataType type,
		void* data
	)
	{
		OGLPLUS_GLFUNC(ReadPixels)(
			x, y,
			width, height,
			GLenum(format),
			GLenum(type),
			data
		);
		OGLPLUS_CHECK(
			ReadPixels,
			Error,
			EnumParam(format)
		);
	}

	/// Transfers a rectangle of pixels from the read buffer the draw buffer
	/**
	 *  @glsymbols
	 *  @glfunref{BlitFramebuffer}
	 */
	static void BlitFramebuffer(
		GLint srcX0,
		GLint srcY0,
		GLint srcX1,
		GLint srcY1,
		GLint dstX0,
		GLint dstY0,
		GLint dstX1,
		GLint dstY1,
		Bitfield<oglplus::BufferSelectBit> mask,
		BlitFilter filter
	)
	{
		OGLPLUS_GLFUNC(BlitFramebuffer)(
			srcX0,
			srcY0,
			srcX1,
			srcY1,
			dstX0,
			dstY0,
			dstX1,
			dstY1,
			GLbitfield(mask),
			GLenum(filter)
		);
		OGLPLUS_CHECK_SIMPLE(BlitFramebuffer);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_5 || GL_ARB_direct_state_access
	/// Transfers a rectangle of pixels from the read buffer the draw buffer
	/**
	 *  @glsymbols
	 *  @glfunref{BlitNamedFramebuffer}
	 */
	static void BlitFramebuffer(
		FramebufferName read,
		FramebufferName draw,
		GLint srcX0,
		GLint srcY0,
		GLint srcX1,
		GLint srcY1,
		GLint dstX0,
		GLint dstY0,
		GLint dstX1,
		GLint dstY1,
		Bitfield<oglplus::BufferSelectBit> mask,
		BlitFilter filter
	)
	{
		OGLPLUS_GLFUNC(BlitNamedFramebuffer)(
			GetGLName(read),
			GetGLName(draw),
			srcX0,
			srcY0,
			srcX1,
			srcY1,
			dstX0,
			dstY0,
			dstX1,
			dstY1,
			GLbitfield(mask),
			GLenum(filter)
		);
		OGLPLUS_CHECK(
			BlitNamedFramebuffer,
			ObjectPairError,
			Subject(read).
			Object(draw).
			EnumParam(filter)
		);
	}
#endif
};

} // namespace context
} // namespace oglplus

#endif // include guard

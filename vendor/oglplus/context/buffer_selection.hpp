/** *  @file oglplus/context/buffer_selection.hpp
 *  @brief Wrappers for functions selecting the buffers for read/write operations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_BUFFER_SELECTION_1201040722_HPP
#define OGLPLUS_CONTEXT_BUFFER_SELECTION_1201040722_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/color_buffer.hpp>
#include <oglplus/framebuffer_attachment.hpp>
#include <oglplus/size_type.hpp>
#include <oglplus/one_of.hpp>

namespace oglplus {
namespace context {

/// Wrappers for functions selecting the buffers for read/write operations
/**
 *  @ingroup ogl_context
 */
class BufferSelection
{
public:
	/// Color buffer specification type
	typedef OneOf<
		GLenum,
		std::tuple<
			oglplus::ColorBuffer,
			oglplus::FramebufferColorAttachment
		>
	> ColorBuffer;

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Sets the destination color buffer for draw operations
	/**
	 *  @glsymbols
	 *  @glfunref{DrawBuffer}
	 */
	static void DrawBuffer(ColorBuffer buffer)
	{
		OGLPLUS_GLFUNC(DrawBuffer)(GLenum(buffer));
		OGLPLUS_VERIFY_SIMPLE(DrawBuffer);
	}

	/// Sets the destination color buffers for draw operations
	/**
	 *  @glsymbols
	 *  @glfunref{DrawBuffers}
	 */
	static void DrawBuffers(const EnumArray<ColorBuffer>& buffers)
	{
		OGLPLUS_GLFUNC(DrawBuffers)(
			GLsizei(buffers.Count()),
			buffers.Values()
		);
		OGLPLUS_VERIFY_SIMPLE(DrawBuffers);
	}

	static void DrawBuffers(SizeType count, const ColorBuffer* buffers)
	{
		DrawBuffers(oglplus::EnumArray<ColorBuffer>(count, buffers));
	}
#endif // GL_VERSION_3_0

	/// Sets the source color buffer for read operations
	/**
	 *  @glsymbols
	 *  @glfunref{ReadBuffer}
	 */
	static void ReadBuffer(ColorBuffer buffer)
	{
		OGLPLUS_GLFUNC(ReadBuffer)(GLenum(buffer));
		OGLPLUS_VERIFY_SIMPLE(ReadBuffer);
	}
};

} // namespace context
} // namespace oglplus

#endif // include guard

/**
 *  @file oglplus/context/drawing.hpp
 *  @brief Wrappers for drawing operations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CONTEXT_DRAWING_1201040722_HPP
#define OGLPLUS_CONTEXT_DRAWING_1201040722_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/error/object.hpp>
#include <oglplus/primitive_type.hpp>
#include <oglplus/patch_parameter.hpp>
#include <oglplus/data_type.hpp>
#include <oglplus/size_type.hpp>
#include <oglplus/object/name.hpp>

namespace oglplus {
namespace context {

/// Wrapper for primitive drawing operations
/**
 *  @ingroup ogl_context
 */
class DrawingOps
{
public:
	/// Draws @a count of primitives from the bound array buffers
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{DrawArrays}
	 */
	static void DrawArrays(
		PrimitiveType primitive,
		GLint first,
		SizeType count
	)
	{
		OGLPLUS_GLFUNC(DrawArrays)(GLenum(primitive), first, count);
		OGLPLUS_VERIFY(
			DrawArrays,
			Error,
			EnumParam(primitive)
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_2
	/// Draws @a count of primitives from the bound array buffers
	/**
	 *  @throws Error
	 *
	 *  @glverreq{4,2}
	 *  @glsymbols
	 *  @glfunref{DrawArraysInstancedBaseInstance}
	 */
	static void DrawArraysInstancedBaseInstance(
		PrimitiveType primitive,
		GLint first,
		SizeType count,
		SizeType inst_count,
		SizeType base_instance
	)
	{
		OGLPLUS_GLFUNC(DrawArraysInstancedBaseInstance)(
			GLenum(primitive),
			first,
			count,
			inst_count,
			base_instance
		);
		OGLPLUS_VERIFY(
			DrawArraysInstancedBaseInstance,
			Error,
			EnumParam(primitive)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_1
	/// Draws @a count of primitives from the bound array buffers
	/**
	 *  @throws Error
	 *
	 *  @glverreq{3,1}
	 *  @glsymbols
	 *  @glfunref{DrawArraysInstanced}
	 */
	static void DrawArraysInstanced(
		PrimitiveType primitive,
		GLint first,
		SizeType count,
		SizeType inst_count
	)
	{
		OGLPLUS_GLFUNC(DrawArraysInstanced)(
			GLenum(primitive),
			first,
			count,
			inst_count
		);
		OGLPLUS_VERIFY(
			DrawArraysInstanced,
			Error,
			EnumParam(primitive)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0 || GL_ARB_draw_indirect
	/// Draws primitives from an indirect buffer
	/**
	 *  @throws Error
	 *
	 *  @glvoereq{4,0,ARB,draw_indirect}
	 *  @glsymbols
	 *  @glfunref{DrawArraysIndirect}
	 */
	static void DrawArraysIndirect(
		PrimitiveType primitive,
		const void* indirect = nullptr
	)
	{
		OGLPLUS_GLFUNC(DrawArraysIndirect)(
			GLenum(primitive),
			indirect
		);
		OGLPLUS_VERIFY(
			DrawArraysIndirect,
			Error,
			EnumParam(primitive)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Draws @a primcount ranges of primitives from the bound array buffers
	/**
	 *  @throws Error
	 *
	 *  @see DrawArrays
	 *
	 *  @glsymbols
	 *  @glfunref{MultiDrawArrays}
	 */
	static void MultiDrawArrays(
		PrimitiveType primitive,
		const GLint* first,
		const GLsizei* count,
		SizeType primcount
	)
	{
		OGLPLUS_GLFUNC(MultiDrawArrays)(
			GLenum(primitive),
			const_cast<GLint*>(first), //TODO: cast because of GLEW
			const_cast<GLsizei*>(count), // remove when GLEW fixed
			primcount
		);
		OGLPLUS_VERIFY(
			MultiDrawArrays,
			Error,
			EnumParam(primitive)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3 || GL_ARB_multi_draw_indirect
	/// Draws multiple sets of primitives from an indirect buffer
	/**
	 *  @throws Error
	 *
	 *  @glvoereq{4,3,ARB,multi_draw_indirect}
	 *  @glsymbols
	 *  @glfunref{MultiDrawArraysIndirect}
	 */
	static void MultiDrawArraysIndirect(
		PrimitiveType primitive,
		SizeType draw_count,
		SizeType stride = 0,
		const void* indirect = nullptr
	)
	{
		OGLPLUS_GLFUNC(MultiDrawArraysIndirect)(
			GLenum(primitive),
			indirect,
			draw_count,
			stride
		);
		OGLPLUS_VERIFY(
			MultiDrawArraysIndirect,
			Error,
			EnumParam(primitive)
		);
	}
#endif

	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{DrawElements}
	 */
	static void DrawElements(
		PrimitiveType primitive,
		SizeType count,
		DataType data_type
	)
	{
		OGLPLUS_GLFUNC(DrawElements)(
			GLenum(primitive),
			count,
			GLenum(data_type),
			nullptr
		);
		OGLPLUS_VERIFY(
			DrawElements,
			Error,
			EnumParam(primitive)
		);
	}

	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{DrawElements}
	 */
	template <typename T>
	static typename std::enable_if<IsGLDataType<T>::value, void>::type
	DrawElements(
		PrimitiveType primitive,
		SizeType count,
		const T* indices
	)
	{
		OGLPLUS_GLFUNC(DrawElements)(
			GLenum(primitive),
			count,
			GLenum(GetDataType<T>()),
			indices
		);
		OGLPLUS_VERIFY(
			DrawElements,
			Error,
			EnumParam(primitive)
		);
	}

	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{DrawElementsInstanced}
	 */
	static void DrawElementsInstanced(
		PrimitiveType primitive,
		SizeType count,
		DataType data_type,
		SizeType instance_count
	)
	{
		OGLPLUS_GLFUNC(DrawElementsInstanced)(
			GLenum(primitive),
			count,
			GLenum(data_type),
			nullptr,
			instance_count
		);
		OGLPLUS_VERIFY(
			DrawElementsInstanced,
			Error,
			EnumParam(primitive)
		);
	}

	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{DrawElementsInstanced}
	 */
	template <typename T>
	static typename std::enable_if<IsGLDataType<T>::value, void>::type
	DrawElementsInstanced(
		PrimitiveType primitive,
		SizeType count,
		const T* indices,
		SizeType instance_count
	)
	{
		OGLPLUS_GLFUNC(DrawElementsInstanced)(
			GLenum(primitive),
			count,
			GLenum(GetDataType<T>()),
			indices,
			instance_count
		);
		OGLPLUS_VERIFY(
			DrawElementsInstanced,
			Error,
			EnumParam(primitive)
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_2
	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glverreq{4,2}
	 *  @glsymbols
	 *  @glfunref{DrawElementsInstancedBaseInstance}
	 */
	static void DrawElementsInstancedBaseInstance(
		PrimitiveType primitive,
		SizeType count,
		DataType data_type,
		SizeType inst_count,
		GLuint base_instance
	)
	{
		OGLPLUS_GLFUNC(DrawElementsInstancedBaseInstance)(
			GLenum(primitive),
			count,
			GLenum(data_type),
			nullptr,
			inst_count,
			base_instance
		);
		OGLPLUS_VERIFY(
			DrawElementsInstancedBaseInstance,
			Error,
			EnumParam(primitive)
		);
	}

	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glverreq{4,2}
	 *  @glsymbols
	 *  @glfunref{DrawElementsInstancedBaseInstance}
	 */
	template <typename T>
	static typename std::enable_if<IsGLDataType<T>::value, void>::type
	DrawElementsInstancedBaseInstance(
		PrimitiveType primitive,
		SizeType count,
		const T* indices,
		SizeType inst_count,
		GLuint base_instance
	)
	{
		OGLPLUS_GLFUNC(DrawElementsInstancedBaseInstance)(
			GLenum(primitive),
			count,
			GLenum(GetDataType<T>()),
			indices,
			inst_count,
			base_instance
		);
		OGLPLUS_VERIFY(
			DrawElementsInstancedBaseInstance,
			Error,
			EnumParam(primitive)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{MultiDrawElements}
	 */
	static void MultiDrawElements(
		PrimitiveType primitive,
		const GLsizei* count,
		DataType data_type,
		SizeType draw_count
	)
	{
		OGLPLUS_GLFUNC(MultiDrawElements)(
			GLenum(primitive),
			const_cast<GLsizei*>(count), //TODO: cast because of GLEW
			GLenum(data_type),
			nullptr,
			draw_count
		);
		OGLPLUS_VERIFY(
			MultiDrawElements,
			Error,
			EnumParam(primitive)
		);
	}

	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{MultiDrawElements}
	 */
	template <typename T>
	static typename std::enable_if<IsGLDataType<T>::value, void>::type
	MultiDrawElements(
		PrimitiveType primitive,
		const GLsizei* count,
		T* const * indices,
		SizeType draw_count
	)
	{
		OGLPLUS_GLFUNC(MultiDrawElements)(
			GLenum(primitive),
			const_cast<GLsizei*>(count), //TODO: cast because of GLEW
			GLenum(GetDataType<T>()),
			indices,
			draw_count
		);
		OGLPLUS_VERIFY(
			MultiDrawElements,
			Error,
			EnumParam(primitive)
		);
	}
#endif

	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{DrawRangeElements}
	 */
	static void DrawRangeElements(
		PrimitiveType primitive,
		GLuint start,
		GLuint end,
		SizeType count,
		DataType data_type
	)
	{
		OGLPLUS_GLFUNC(DrawRangeElements)(
			GLenum(primitive),
			start,
			end,
			count,
			GLenum(data_type),
			nullptr
		);
		OGLPLUS_VERIFY(
			DrawRangeElements,
			Error,
			EnumParam(primitive)
		);
	}

	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{DrawRangeElements}
	 */
	template <typename T>
	static typename std::enable_if<IsGLDataType<T>::value, void>::type
	DrawRangeElements(
		PrimitiveType primitive,
		GLuint start,
		GLuint end,
		SizeType count,
		const T* indices
	)
	{
		OGLPLUS_GLFUNC(DrawRangeElements)(
			GLenum(primitive),
			start,
			end,
			count,
			GLenum(GetDataType<T>()),
			indices
		);
		OGLPLUS_VERIFY(
			DrawRangeElements,
			Error,
			EnumParam(primitive)
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0 || GL_ARB_draw_indirect
	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glvoereq{4,0,ARB,GL_ARB_draw_indirect}
	 *  @glsymbols
	 *  @glfunref{DrawElementsIndirect}
	 */
	static void DrawElementsIndirect(
		PrimitiveType primitive,
		DataType data_type,
		const void* indirect = nullptr
	)
	{
		OGLPLUS_GLFUNC(DrawElementsIndirect)(
			GLenum(primitive),
			GLenum(data_type),
			indirect
		);
		OGLPLUS_VERIFY(
			DrawElementsIndirect,
			Error,
			EnumParam(primitive)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3
	/// Draws sequences of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glverreq{4,3}
	 *  @glsymbols
	 *  @glfunref{MultiDrawElementsIndirect}
	 */
	static void MultiDrawElementsIndirect(
		PrimitiveType primitive,
		DataType data_type,
		SizeType draw_count,
		SizeType stride = 0,
		const void* indirect = nullptr
	)
	{
		OGLPLUS_GLFUNC(MultiDrawElementsIndirect)(
			GLenum(primitive),
			GLenum(data_type),
			indirect,
			draw_count,
			stride
		);
		OGLPLUS_VERIFY(
			MultiDrawElementsIndirect,
			Error,
			EnumParam(primitive)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_2 || GL_ARB_draw_elements_base_vertex
	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glvoereq{3,2,ARB,draw_elements_base_vertex}
	 *  @glsymbols
	 *  @glfunref{DrawElementsBaseVertex}
	 */
	static void DrawElementsBaseVertex(
		PrimitiveType primitive,
		SizeType count,
		DataType data_type,
		GLint base_vertex
	)
	{
		OGLPLUS_GLFUNC(DrawElementsBaseVertex)(
			GLenum(primitive),
			count,
			GLenum(data_type),
			nullptr,
			base_vertex
		);
		OGLPLUS_VERIFY(
			DrawElementsBaseVertex,
			Error,
			EnumParam(primitive)
		);
	}
	/// Draws a sequence of primitives from the bound element array buffers

	/**
	 *  @throws Error
	 *
	 *  @glvoereq{3,2,ARB,draw_elements_base_vertex}
	 *  @glsymbols
	 *  @glfunref{DrawElementsBaseVertex}
	 */
	template <typename T>
	static typename std::enable_if<IsGLDataType<T>::value, void>::type
	DrawElementsBaseVertex(
		PrimitiveType primitive,
		SizeType count,
		const T* indices,
		GLint base_vertex
	)
	{
		OGLPLUS_GLFUNC(DrawElementsBaseVertex)(
			GLenum(primitive),
			count,
			GLenum(GetDataType<T>()),
			indices,
			base_vertex
		);
		OGLPLUS_VERIFY(
			DrawElementsBaseVertex,
			Error,
			EnumParam(primitive)
		);
	}

	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glvoereq{3,2,ARB,draw_elements_base_vertex}
	 *  @glsymbols
	 *  @glfunref{DrawRangeElementsBaseVertex}
	 */
	static void DrawRangeElementsBaseVertex(
		PrimitiveType primitive,
		GLuint start,
		GLuint end,
		SizeType count,
		DataType data_type,
		GLint base_vertex
	)
	{
		OGLPLUS_GLFUNC(DrawRangeElementsBaseVertex)(
			GLenum(primitive),
			start,
			end,
			count,
			GLenum(data_type),
			nullptr,
			base_vertex
		);
		OGLPLUS_VERIFY(
			DrawRangeElementsBaseVertex,
			Error,
			EnumParam(primitive)
		);
	}

	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glvoereq{3,2,ARB,draw_elements_base_vertex}
	 *  @glsymbols
	 *  @glfunref{DrawRangeElementsBaseVertex}
	 */
	template <typename T>
	static typename std::enable_if<IsGLDataType<T>::value, void>::type
	DrawRangeElementsBaseVertex(
		PrimitiveType primitive,
		GLuint start,
		GLuint end,
		SizeType count,
		const T* indices,
		GLint base_vertex
	)
	{
		OGLPLUS_GLFUNC(DrawRangeElementsBaseVertex)(
			GLenum(primitive),
			start,
			end,
			count,
			GLenum(GetDataType<T>()),
			indices,
			base_vertex
		);
		OGLPLUS_VERIFY(
			DrawRangeElementsBaseVertex,
			Error,
			EnumParam(primitive)
		);
	}

	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glvoereq{3,2,ARB,draw_elements_base_vertex}
	 *  @glsymbols
	 *  @glfunref{DrawElementsInstancedBaseVertex}
	 */
	static void DrawElementsInstancedBaseVertex(
		PrimitiveType primitive,
		SizeType count,
		DataType data_type,
		SizeType inst_count,
		GLint base_vertex
	)
	{
		OGLPLUS_GLFUNC(DrawElementsInstancedBaseVertex)(
			GLenum(primitive),
			count,
			GLenum(data_type),
			nullptr,
			inst_count,
			base_vertex
		);
		OGLPLUS_VERIFY(
			DrawElementsInstancedBaseVertex,
			Error,
			EnumParam(primitive)
		);
	}

	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glvoereq{3,2,ARB,draw_elements_base_vertex}
	 *  @glsymbols
	 *  @glfunref{DrawElementsInstancedBaseVertex}
	 */
	template <typename T>
	static typename std::enable_if<IsGLDataType<T>::value, void>::type
	DrawElementsInstancedBaseVertex(
		PrimitiveType primitive,
		SizeType count,
		const T* indices,
		SizeType inst_count,
		GLint base_vertex
	)
	{
		OGLPLUS_GLFUNC(DrawElementsInstancedBaseVertex)(
			GLenum(primitive),
			count,
			GLenum(GetDataType<T>()),
			indices,
			inst_count,
			base_vertex
		);
		OGLPLUS_VERIFY(
			DrawElementsInstancedBaseVertex,
			Error,
			EnumParam(primitive)
		);
	}

	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glvoereq{3,2,ARB,draw_elements_base_vertex}
	 *  @glsymbols
	 *  @glfunref{MultiDrawElementsBaseVertex}
	 */
	static void MultiDrawElementsBaseVertex(
		PrimitiveType primitive,
		const GLsizei* count,
		DataType data_type,
		SizeType draw_count,
		const GLint* base_vertex
	)
	{
		OGLPLUS_GLFUNC(MultiDrawElementsBaseVertex)(
			GLenum(primitive),
			const_cast<GLsizei*>(count), //TODO remove const_cast
			GLenum(data_type),
			nullptr,
			draw_count,
			const_cast<GLint*>(base_vertex) //TODO remove const_cast
		);
		OGLPLUS_VERIFY(
			MultiDrawElementsBaseVertex,
			Error,
			EnumParam(primitive)
		);
	}

	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glvoereq{3,2,ARB,draw_elements_base_vertex}
	 *  @glsymbols
	 *  @glfunref{MultiDrawElementsBaseVertex}
	 */
	template <typename T>
	static typename std::enable_if<IsGLDataType<T>::value, void>::type
	MultiDrawElementsBaseVertex(
		PrimitiveType primitive,
		const GLsizei* count,
		T* const * indices,
		SizeType draw_count,
		const GLint* base_vertex
	)
	{
		OGLPLUS_GLFUNC(MultiDrawElementsBaseVertex)(
			GLenum(primitive),
			const_cast<GLsizei*>(count), //TODO remove const_cast
			GLenum(GetDataType<T>()),
			indices,
			draw_count,
			const_cast<GLint*>(base_vertex) //TODO remove const_cast
		);
		OGLPLUS_VERIFY(
			MultiDrawElementsBaseVertex,
			Error,
			EnumParam(primitive)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_2
	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glverreq{4,2}
	 *  @glsymbols
	 *  @glfunref{DrawElementsInstancedBaseVertexBaseInstance}
	 */
	static void DrawElementsInstancedBaseVertexBaseInstance(
		PrimitiveType primitive,
		SizeType count,
		DataType data_type,
		SizeType inst_count,
		GLint base_vertex,
		GLuint base_instance
	)
	{
		OGLPLUS_GLFUNC(DrawElementsInstancedBaseVertexBaseInstance)(
			GLenum(primitive),
			count,
			GLenum(data_type),
			nullptr,
			inst_count,
			base_vertex,
			base_instance
		);
		OGLPLUS_VERIFY(
			DrawElementsInstancedBaseVertexBaseInstance,
			Error,
			EnumParam(primitive)
		);
	}

	/// Draws a sequence of primitives from the bound element array buffers
	/**
	 *  @throws Error
	 *
	 *  @glverreq{4,2}
	 *  @glsymbols
	 *  @glfunref{DrawElementsInstancedBaseVertexBaseInstance}
	 */
	template <typename T>
	static typename std::enable_if<IsGLDataType<T>::value, void>::type
	DrawElementsInstancedBaseVertexBaseInstance(
		PrimitiveType primitive,
		SizeType count,
		const T* indices,
		SizeType inst_count,
		GLint base_vertex,
		GLuint base_instance
	)
	{
		OGLPLUS_GLFUNC(DrawElementsInstancedBaseVertexBaseInstance)(
			GLenum(primitive),
			count,
			GLenum(GetDataType<T>()),
			indices,
			inst_count,
			base_vertex,
			base_instance
		);
		OGLPLUS_VERIFY(
			DrawElementsInstancedBaseVertexBaseInstance,
			Error,
			EnumParam(primitive)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0 || GL_ARB_transform_feedback2
	static void DrawTransformFeedback(
		PrimitiveType primitive,
		TransformFeedbackName xfb
	)
	{
		OGLPLUS_GLFUNC(DrawTransformFeedback)(
			GLenum(primitive),
			GetGLName(xfb)
		);
		OGLPLUS_VERIFY(
			DrawTransformFeedback,
			ObjectError,
			Object(xfb).
			EnumParam(primitive)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_NV_draw_texture
	static void DrawTexture(
		TextureName texture,
		SamplerName sampler,
		GLfloat x0,
		GLfloat y0,
		GLfloat x1,
		GLfloat y1,
		GLfloat z,
		GLfloat s0,
		GLfloat t0,
		GLfloat s1,
		GLfloat t1
	)
	{
		OGLPLUS_GLFUNC(DrawTextureNV)(
			GetGLName(texture),
			GetGLName(sampler),
			x0, y0,
			x1, y1,
			z,
			s0, t0,
			s1, t1
		);
		OGLPLUS_CHECK(
			DrawTextureNV,
			ObjectPairError,
			Subject(sampler).
			Object(texture)
		);
	}

	static void DrawTexture(
		TextureName texture,
		GLfloat x0,
		GLfloat y0,
		GLfloat x1,
		GLfloat y1,
		GLfloat z
	)
	{
		DrawTexture(
			texture,
			SamplerName(),
			x0, y0,
			x1, y1,
			z,
			0, 0,
			1, 1
		);
	}
#endif
};

/// Wrapper for primitive drawing settings
/**
 *  @ingroup ogl_context
 */
class DrawingState
{
public:
#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_1
	/// Sets the primitive restart index
	/**
	 *  @throws Error
	 *
	 *  @glverreq{3,1}
	 *  @glsymbols
	 *  @glfunref{PrimitiveRestartIndex}
	 */
	static void PrimitiveRestartIndex(GLuint index)
	{
		OGLPLUS_GLFUNC(PrimitiveRestartIndex)(index);
		OGLPLUS_CHECK(
			PrimitiveRestartIndex,
			Error,
			Index(index)
		);
	}

	static GLuint PrimitiveRestartIndex(void)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			GL_PRIMITIVE_RESTART_INDEX,
			&result
		);
		OGLPLUS_VERIFY_SIMPLE(GetIntegerv);
		return GLuint(result);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_ARB_tessellation_shader || GL_VERSION_4_0
	static void PatchParameter(
		oglplus::PatchParameter parameter,
		GLint value
	)
	{
		OGLPLUS_GLFUNC(PatchParameteri)(GLenum(parameter), value);
		OGLPLUS_CHECK(
			PatchParameteri,
			Error,
			EnumParam(parameter)
		);
	}
#endif
};

} // namespace context
} // namespace oglplus

#endif // include guard

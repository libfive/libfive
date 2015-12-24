/**
 *  @file oglplus/dsa/vertex_attrib.hpp
 *  @brief VertexArrayAttrib wrappers with direct state access
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_DSA_VERTEX_ATTRIB_1107121519_HPP
#define OGLPLUS_DSA_VERTEX_ATTRIB_1107121519_HPP

#include <oglplus/vertex_attrib.hpp>
#include <oglplus/error/object.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_5 || GL_ARB_direct_state_access

class DSAVertexArrayAttrib
 : public ProgVarCommonOps<tag::VertexAttrib>
{
private:
	GLuint _vao;
public:
	/// References the vertex attribute array at @p location
	/**
	 *  @glsymbols
	 *  @glfunref{GetAttribLocation}
	 */
	DSAVertexArrayAttrib(
		VertexArrayName vao,
		VertexAttribSlot location
	): ProgVarCommonOps<tag::VertexAttrib>(VertexAttribLoc(GLint(location)))
	 , _vao(GetGLName(vao))
	{ }

	/// References the vertex attrib array @p identifier of the @p program
	/**
	 *  @glsymbols
	 *  @glfunref{GetAttribLocation}
	 */
	DSAVertexArrayAttrib(
		VertexArrayName vao,
		ProgramName program,
		StrCRef identifier
	): ProgVarCommonOps<tag::VertexAttrib>(VertexAttribLoc(program, identifier))
	 , _vao(GetGLName(vao))
	{ }

	/// Enable this vertex array attribute
	/**
	 *  @glsymbols
	 *  @glfunref{EnableVertexArrayAttrib}
	 */
	DSAVertexArrayAttrib& Enable(void)
	{
		OGLPLUS_GLFUNC(EnableVertexArrayAttrib)(
			_vao,
			GLuint(_location)
		);
		OGLPLUS_CHECK_SIMPLE(EnableVertexArrayAttrib);
		return *this;
	}

	/// Disable this specified vertex array attribute
	/**
	 *  @glsymbols
	 *  @glfunref{DisableVertexArrayAttrib}
	 */
	DSAVertexArrayAttrib& Disable(void)
	{
		OGLPLUS_GLFUNC(DisableVertexArrayAttrib)(
			_vao,
			GLuint(_location)
		);
		OGLPLUS_CHECK_SIMPLE(DisableVertexArrayAttrib);
		return *this;
	}

	/// Set the vertex buffer for this vertex array attribute
	DSAVertexArrayAttrib& VertexBuffer(
		BufferName buffer,
		GLintptr offset,
		GLsizei stride
	)
	{
		OGLPLUS_GLFUNC(VertexArrayVertexBuffer)(
			_vao,
			GLuint(_location),
			GetGLName(buffer),
			offset,
			stride
		);
		OGLPLUS_CHECK(
			VertexArrayVertexBuffer,
			ObjectPairError,
			Subject(buffer).
			Object(VertexArrayName(_vao))
		);
		return *this;
	}

	/// Setup the properties of this vertex array attribute
	/**
	 *  @glsymbols
	 *  @glfunref{VertexArrayAttribFormat}
	 */
	DSAVertexArrayAttrib& Format(
		GLint values_per_vertex,
		DataType data_type,
		Boolean normalized,
		GLuint relative_offset
	)
	{
		OGLPLUS_GLFUNC(VertexArrayAttribFormat)(
			_vao,
			GLuint(_location),
			values_per_vertex,
			GLenum(data_type),
			normalized._get(),
			relative_offset
		);
		OGLPLUS_CHECK(
			VertexArrayAttribFormat,
			ObjectError,
			Object(VertexArrayName(_vao)).
			EnumParam(data_type)
		);
		return *this;
	}

	/// Setup the properties of this vertex array attribute
	/**
	 *  @glsymbols
	 *  @glfunref{VertexArrayAttribIFormat}
	 */
	DSAVertexArrayAttrib& IFormat(
		GLint values_per_vertex,
		DataType data_type,
		GLuint relative_offset
	)
	{
		OGLPLUS_GLFUNC(VertexArrayAttribIFormat)(
			_vao,
			GLuint(_location),
			values_per_vertex,
			GLenum(data_type),
			relative_offset
		);
		OGLPLUS_CHECK(
			VertexArrayAttribIFormat,
			ObjectError,
			Object(VertexArrayName(_vao)).
			EnumParam(data_type)
		);
		return *this;
	}

	/// Setup the properties of this vertex array attribute
	/**
	 *  @glsymbols
	 *  @glfunref{VertexArrayAttribLFormat}
	 */
	DSAVertexArrayAttrib& LFormat(
		GLint values_per_vertex,
		DataType data_type,
		GLuint relative_offset
	)
	{
		OGLPLUS_GLFUNC(VertexArrayAttribLFormat)(
			_vao,
			GLuint(_location),
			values_per_vertex,
			GLenum(data_type),
			relative_offset
		);
		OGLPLUS_CHECK(
			VertexArrayAttribLFormat,
			ObjectError,
			Object(VertexArrayName(_vao)).
			EnumParam(data_type)
		);
		return *this;
	}
};

#endif // GL_ARB_direct_state_access

} // namespace oglplus

#endif // include guard

/**
 *  @file oglplus/shader_storage_block.hpp
 *  @brief Named uniform block wrappers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_SHADER_STORAGE_BLOCK_1506141200_HPP
#define OGLPLUS_SHADER_STORAGE_BLOCK_1506141200_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/string/ref.hpp>
#include <oglplus/error/prog_var.hpp>
#include <oglplus/prog_var/location.hpp>
#include <oglplus/prog_var/varpara_fns.hpp>
#include <oglplus/prog_var/set_ops.hpp>
#include <oglplus/prog_var/wrapper.hpp>
#include <oglplus/boolean.hpp>
#include <oglplus/shader_type.hpp>
#include <oglplus/buffer_binding.hpp>

#include <cassert>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3

template <>
class ProgVarLocOps<tag::ShaderStorageBlock>
{
private:
	static const char* MsgGettingInactive(void);
protected:
	static const char* MsgUsingInactive(void);
public:
	/// Finds the shader-storage block location, throws on failure if active_only
	/** Finds the location / index of the shader-storage block specified
	 *  by @p identifier in a @p program. If active_only is true then
	 *  throws if no such shader-storage block exists or if it is not active.
	 *
	 *  @glsymbols
	 *  @glfunref{GetProgramResourceIndex}
	 */
	static GLint GetLocation(
		ProgramName program,
		StrCRef identifier,
		bool active_only
	)
	{
		GLuint result = OGLPLUS_GLFUNC(GetProgramResourceIndex)(
			GetGLName(program),
			GL_SHADER_STORAGE_BLOCK,
			identifier.c_str()
		);
		OGLPLUS_CHECK(
			GetProgramResourceIndex,
			ProgVarError,
			Program(program).
			Identifier(identifier)
		);
		OGLPLUS_HANDLE_ERROR_IF(
			active_only && (result == GL_INVALID_INDEX),
			GL_INVALID_OPERATION,
			MsgGettingInactive(),
			ProgVarError,
			Program(program).
			Identifier(identifier)
		);

		if(result == GL_INVALID_INDEX)
		{
			return -1;
		}
		return GLint(result);
	}
};

template <>
class ProgVarCommonOps<tag::ShaderStorageBlock>
 : public ProgVarLoc<tag::ShaderStorageBlock>
{
private:
	static GLenum _translate_ref(ShaderType shader_type);
	static GLenum _translate_max(ShaderType shader_type);
protected:
	ProgVarCommonOps(ShaderStorageBlockLoc ubloc)
	 : ProgVarLoc<tag::ShaderStorageBlock>(ubloc)
	{ }

	GLuint _block_index(void) const
	{
		if(this->_location < 0)
		{
			return GL_INVALID_INDEX;
		}
		return GLuint(this->_location);
	}
public:
	/// Return the maximum number of shader-storage blocks for a @p shader_type
	static GLuint MaxIn(ShaderType shader_type)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(
			_translate_max(shader_type),
			&result
		);
		OGLPLUS_VERIFY(
			GetIntegerv,
			Error,
			EnumParam(_translate_max(shader_type))
		);
		assert(result >= 0);
		return GLuint(result);
	}

	/// Returns true if this shader-storage block is referenced by @p shader_type
	/**
	 *  @glsymbols
	 *  @glfunref{GetActiveUniformBlock}
	 */
	Boolean ReferencedBy(ShaderType shader_type) const
	{
		Boolean result;

		// TODO: check this
		OGLPLUS_GLFUNC(GetActiveUniformBlockiv)(
			this->_program,
			_block_index(),
			_translate_ref(shader_type),
			result._ptr()
		);
		OGLPLUS_VERIFY(
			GetActiveUniformBlockiv,
			Error,
			EnumParam(_translate_ref(shader_type))
		);
		return result;
	}

	/// Returns the size of the shader-storage block
	/**
	 *  @glsymbols
	 *  @glfunref{GetActiveUniformBlock}
	 */
	GLuint64 DataSize(void) const
	{
		GLint64 result;
		OGLPLUS_GLFUNC(GetInteger64i_v)(
			_block_index(),
			GL_SHADER_STORAGE_BUFFER_SIZE,
			&result
		);
		OGLPLUS_VERIFY(
			GetInteger64i_v,
			Error,
			EnumParam(GLenum(GL_SHADER_STORAGE_BUFFER_SIZE))
		);
		assert(result >= 0);
		return GLuint64(result);
	}

	/// Sets up the shader-storage block binding
	/**
	 *  @glsymbols
	 *  @glfunref{ShaderStorageBlockBinding}
	 */
	void Binding(ShaderStorageBufferBindingPoint binding)
	{
		OGLPLUS_GLFUNC(ShaderStorageBlockBinding)(
			this->_program,
			_block_index(),
			GLuint(binding)
		);
		OGLPLUS_VERIFY(
			ShaderStorageBlockBinding,
			ProgVarError,
			Program(ProgramName(this->_program)).
			Index(GLuint(binding))
		);
	}
};

/// Encapsulates shader-storage block operations
/**
 *  @see Uniform
 *
 *  @ingroup shader_variables
 *
 *  @glvoereq{4,3,ARB,shader_storage_buffer_object}
 */
typedef ProgVar<
	tag::ImplicitSel,
	tag::ShaderStorageBlock,
	tag::NoTypecheck,
	void
> ShaderStorageBlock;

#endif // shader-storage buffer object

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/shader_storage_block.ipp>
#endif

#endif // include guard

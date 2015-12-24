/**
 *  @file oglplus/uniform_block.hpp
 *  @brief Named uniform block wrappers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_UNIFORM_BLOCK_1107121519_HPP
#define OGLPLUS_UNIFORM_BLOCK_1107121519_HPP

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

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_1 || GL_ARB_uniform_buffer_object

template <>
class ProgVarLocOps<tag::UniformBlock>
{
private:
	static const char* MsgGettingInactive(void);
protected:
	static const char* MsgUsingInactive(void);
public:
	/// Finds the uniform block location, throws on failure if active_only
	/** Finds the location / index of the uniform block specified
	 *  by @p identifier in a @p program. If active_only is true then
	 *  throws if no such uniform block exists or if it is not active.
	 *
	 *  @glsymbols
	 *  @glfunref{GetUniformBlockIndex}
	 */
	static GLint GetLocation(
		ProgramName program,
		StrCRef identifier,
		bool active_only
	)
	{
		GLuint result = OGLPLUS_GLFUNC(GetUniformBlockIndex)(
			GetGLName(program),
			identifier.c_str()
		);
		OGLPLUS_CHECK(
			GetUniformBlockIndex,
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
class ProgVarCommonOps<tag::UniformBlock>
 : public ProgVarLoc<tag::UniformBlock>
{
private:
	static GLenum _translate_ref(ShaderType shader_type);
	static GLenum _translate_max(ShaderType shader_type);
protected:
	ProgVarCommonOps(UniformBlockLoc ubloc)
	 : ProgVarLoc<tag::UniformBlock>(ubloc)
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
	/// Return the maximum number of uniform blocks for a @p shader_type
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

	/// Returns true if this uniform block is referenced by @p shader_type
	/**
	 *  @glsymbols
	 *  @glfunref{GetActiveUniformBlock}
	 */
	Boolean ReferencedBy(ShaderType shader_type) const
	{
		Boolean result;
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

	/// Returns the size of the uniform block
	/**
	 *  @glsymbols
	 *  @glfunref{GetActiveUniformBlock}
	 */
	GLuint DataSize(void) const
	{
		GLint result;
		OGLPLUS_GLFUNC(GetActiveUniformBlockiv)(
			this->_program,
			_block_index(),
			GL_UNIFORM_BLOCK_DATA_SIZE,
			&result
		);
		OGLPLUS_VERIFY(
			GetActiveUniformBlockiv,
			Error,
			EnumParam(GLenum(GL_UNIFORM_BLOCK_DATA_SIZE))
		);
		assert(result >= 0);
		return GLuint(result);
	}

	/// Sets up the uniform block binding
	/**
	 *  @glsymbols
	 *  @glfunref{UniformBlockBinding}
	 */
	void Binding(UniformBufferBindingPoint binding)
	{
		OGLPLUS_GLFUNC(UniformBlockBinding)(
			this->_program,
			_block_index(),
			GLuint(binding)
		);
		OGLPLUS_VERIFY(
			UniformBlockBinding,
			ProgVarError,
			Program(ProgramName(this->_program)).
			Index(GLuint(binding))
		);
	}
};

/// Encapsulates uniform block operations
/**
 *  @see Uniform
 *
 *  @ingroup shader_variables
 *
 *  @glvoereq{3,1,ARB,uniform_buffer_object}
 */
typedef ProgVar<
	tag::ImplicitSel,
	tag::UniformBlock,
	tag::NoTypecheck,
	void
> UniformBlock;

#endif // uniform buffer object

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/uniform_block.ipp>
#endif

#endif // include guard

/**
 *  @file oglplus/uniform.ipp
 *  @brief Implementation of Uniform functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/error/prog_var.hpp>
#include <oglplus/object/tags.hpp>
#include <oglplus/object/desc.hpp>
#include <oglplus/lib/incl_end.ipp>
#include <cstring>

namespace oglplus {

OGLPLUS_LIB_FUNC
const char* ProgVarLocOps<tag::Uniform>::
MsgGettingInactive(void)
{
	return "Getting the location of inactive program uniform variable";
}

OGLPLUS_LIB_FUNC
const char* ProgVarLocOps<tag::Uniform>::
MsgUsingInactive(void)
{
	return "Using inactive program uniform variable";
}

OGLPLUS_LIB_FUNC
GLenum ProgVarTypeOps<tag::Uniform>::
GetType(ProgramName program, GLint /*location*/, StrCRef identifier)
{

	GLenum type, result = GL_NONE;
	GLint size;
	GLsizei length = 0;
	// The +2 is intentional
	// to distinguish between the searched identifier
	// and the identifiers having the searched identifier
	// as prefix
	std::vector<GLchar> buffer(identifier.size()+2);

	GLint active_uniforms = 0;
	OGLPLUS_GLFUNC(GetProgramiv)(
		GetGLName(program),
		GL_ACTIVE_UNIFORMS,
		&active_uniforms
	);
	OGLPLUS_VERIFY(
		GetProgramiv,
		ProgVarError,
		Program(program).
		Identifier(identifier)
	);

	for(GLint index=0; index<active_uniforms; ++index)
	{
		OGLPLUS_GLFUNC(GetActiveUniform)(
			GetGLName(program),
			GLuint(index),
			GLsizei(buffer.size()),
			&length,
			&size,
			&type,
			buffer.data()
		);
		OGLPLUS_VERIFY(
			GetActiveUniform,
			ProgVarError,
			Program(program).
			Identifier(identifier)
		);

		assert(!(length < 0));

		if(GLsizei(identifier.size()) == length)
		{
			if(std::strncmp(
				identifier.c_str(),
				buffer.data(),
				std::size_t(length)
			) == 0)
			{
				result = type;
				break;
			}
		}
	}
	return result;
}

} // namespace oglplus


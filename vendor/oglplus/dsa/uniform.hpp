/**
 *  @file oglplus/dsa/uniform.hpp
 *  @brief Uniform direct state access wrappers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_DSA_UNIFORM_1107121519_HPP
#define OGLPLUS_DSA_UNIFORM_1107121519_HPP

#include <oglplus/uniform.hpp>

namespace oglplus {

// collection of direct state access ProgramUniform
// setter functions for basic types
template <>
class ProgVarSetters<tag::DirectState, tag::Uniform, tag::NativeTypes>
{
protected:
	OGLPLUS_ERROR_CONTEXT(ProgramUniform, ProgramUniform)

#if GL_VERSION_4_1 || GL_ARB_separate_shader_objects
	OGLPLUS_AUX_VARPARA_FNS(ProgramUniform, ui, t, GLuint)
	OGLPLUS_AUX_VARPARA_FNS(ProgramUniform, i, t, GLint)
#if GL_ARB_bindless_texture
	OGLPLUS_AUX_VARPARA_FNC(ProgramUniformHandle, ui64ARB, t, GLuint64, 1)
#elif GL_NV_shader_buffer_load
	OGLPLUS_AUX_VARPARA_FNC(ProgramUniform, ui64NV, t, GLuint64EXT, 1)
#endif
	OGLPLUS_AUX_VARPARA_FNS(ProgramUniform, f, t, GLfloat)
	OGLPLUS_AUX_VARPARA_FNS(ProgramUniform, d, t, GLdouble)

	OGLPLUS_AUX_VARPARA_FNS(ProgramUniform, iv, v, GLint)
#if GL_ARB_bindless_texture
	OGLPLUS_AUX_VARPARA_FNC(ProgramUniformHandle, ui64vARB, v, GLuint64, 1)
#endif
	OGLPLUS_AUX_VARPARA_FNS(ProgramUniform, fv, v, GLfloat)
	OGLPLUS_AUX_VARPARA_FNS(ProgramUniform, dv, v, GLdouble)
#elif GL_EXT_direct_state_access
	OGLPLUS_AUX_VARPARA_FNS_EXT(ProgramUniform, ui, EXT, t, GLuint)
	OGLPLUS_AUX_VARPARA_FNS_EXT(ProgramUniform, i, EXT, t, GLint)
	OGLPLUS_AUX_VARPARA_FNS_EXT(ProgramUniform, f, EXT, t, GLfloat)

	OGLPLUS_AUX_VARPARA_FNS_EXT(ProgramUniform, iv, EXT, v, GLint)
	OGLPLUS_AUX_VARPARA_FNS_EXT(ProgramUniform, fv, EXT, v, GLfloat)
#endif
};

// collection of direct state access ProgramUniform
// setter functions for matrices
template <>
class ProgVarSetters<tag::DirectState, tag::Uniform, tag::MatrixTypes>
{
protected:
	OGLPLUS_ERROR_CONTEXT(ProgramUniformMatrix, ProgramUniform)

#if GL_VERSION_4_1 || GL_ARB_separate_shader_objects
	OGLPLUS_AUX_VARPARA_MAT_FNS(ProgramUniformMatrix, fv, v, GLfloat)
	OGLPLUS_AUX_VARPARA_MAT_FNS(ProgramUniformMatrix, dv, v, GLdouble)
#elif GL_EXT_direct_state_access
	OGLPLUS_AUX_VARPARA_MAT_FNS_EXT(ProgramUniformMatrix,fv,EXT, v, GLfloat)
#endif
};

OGLPLUS_DECLARE_PROG_VAR(
	ProgramUniform,
	tag::DirectState,
	tag::Uniform,
	tag::NoTypecheck
)

/// ProgramUniform sampler
typedef ProgramUniform<GLint> ProgramUniformSampler;

} // namespace oglplus

#endif // include guard

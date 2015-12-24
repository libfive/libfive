/**
 *  @file oglplus/opt/shader_literals.hpp
 *  @brief User-defined string literals resulting in shader objects
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_OPT_SHADER_LITERALS_HPP
#define OGLPLUS_OPT_SHADER_LITERALS_HPP

#include <oglplus/config/compiler.hpp>
#include <oglplus/shader.hpp>

#if !OGLPLUS_NO_USER_DEFINED_LITERALS

namespace oglplus {
namespace aux {

inline Shader ShaderFromLiteral(
	ShaderType shader_type,
	const char* lit,
	size_t size
)
{
	Shader shader(shader_type);
	shader.Source(StrCRef(lit, size));
	shader.Compile();
	return shader;
}

} // namespace aux

#ifdef GL_VERTEX_SHADER
inline Shader operator "" _glsl_vs(const char* str, size_t size)
{
	return aux::ShaderFromLiteral(ShaderType::Vertex, str, size);
}
#endif

#ifdef GL_TESS_CONTROL_SHADER
inline Shader operator "" _glsl_tcs(const char* str, size_t size)
{
	return aux::ShaderFromLiteral(ShaderType::TessControl, str, size);
}
#endif

#ifdef GL_TESS_EVALUATION_SHADER
inline Shader operator "" _glsl_tes(const char* str, size_t size)
{
	return aux::ShaderFromLiteral(ShaderType::TessEvaluation, str, size);
}
#endif

#ifdef GL_GEOMETRY_SHADER
inline Shader operator "" _glsl_gs(const char* str, size_t size)
{
	return aux::ShaderFromLiteral(ShaderType::Geometry, str, size);
}
#endif

#ifdef GL_FRAGMENT_SHADER
inline Shader operator "" _glsl_fs(const char* str, size_t size)
{
	return aux::ShaderFromLiteral(ShaderType::Fragment, str, size);
}
#endif

#ifdef GL_COMPUTE_SHADER
inline Shader operator "" _glsl_cs(const char* str, size_t size)
{
	return aux::ShaderFromLiteral(ShaderType::Compute, str, size);
}
#endif

} // namespace oglplus

#else
#error "User-defined literals are required!"
#endif // OGLPLUS_NO_USER_DEFINED_LITERALS

#endif // include guard

/**
 *  @file oglplus/shader.ipp
 *  @brief Implementation of Shader
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/detail/info_log.hpp>
#include <oglplus/lib/incl_end.ipp>

namespace oglplus {

OGLPLUS_LIB_FUNC
String ObjectOps<tag::DirectState, tag::Shader>::
GetInfoLog(void) const
{
	return aux::GetInfoLog(
		_obj_name(), OGLPLUS_GLFUNC(GetShaderiv),
		OGLPLUS_GLFUNC(GetShaderInfoLog),
		"GetShaderiv",
		"GetShaderInfoLog"
	);
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Shader>&
ObjectOps<tag::DirectState, tag::Shader>::
Compile(void)
{
	OGLPLUS_GLFUNC(CompileShader)(_obj_name());
	OGLPLUS_CHECK(
		CompileShader,
		ObjectError,
		Object(*this).
		EnumParam(Type())
	);
	OGLPLUS_HANDLE_ERROR_IF(
		!IsCompiled(),
		GL_INVALID_OPERATION,
		CompileError::Message(),
		CompileError,
		Log(GetInfoLog()).
		Object(*this).
		EnumParam(Type())
	);
	return *this;
}

OGLPLUS_LIB_FUNC
Outcome<ObjectOps<tag::DirectState, tag::Shader>&>
ObjectOps<tag::DirectState, tag::Shader>::
Compile(std::nothrow_t)
{
	OGLPLUS_GLFUNC(CompileShader)(_obj_name());
	OGLPLUS_DEFERRED_CHECK(
		CompileShader,
		ObjectError,
		Object(*this).
		EnumParam(Type())
	);
	OGLPLUS_RETURN_HANDLER_IF(
		!IsCompiled(),
		GL_INVALID_OPERATION,
		CompileError::Message(),
		CompileError,
		Log(GetInfoLog()).
		Object(*this).
		EnumParam(Type())
	);
	return *this;
}

#if GL_ARB_shading_language_include
OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Shader>&
ObjectOps<tag::DirectState, tag::Shader>::
CompileInclude(
	const SizeType count,
	const GLchar* const* paths,
	const GLint* lengths
)
{
	OGLPLUS_GLFUNC(CompileShaderIncludeARB)(
		_obj_name(),
		count,
		const_cast<const GLchar**>(paths),
		lengths
	);
	OGLPLUS_CHECK(
		CompileShaderIncludeARB,
		ObjectError,
		Object(*this).
		EnumParam(Type())
	);
	return *this;
}

OGLPLUS_LIB_FUNC
Outcome<ObjectOps<tag::DirectState, tag::Shader>&>
ObjectOps<tag::DirectState, tag::Shader>::
CompileInclude(
	const SizeType count,
	const GLchar* const* paths,
	const GLint* lengths,
	std::nothrow_t
)
{
	OGLPLUS_GLFUNC(CompileShaderIncludeARB)(
		_obj_name(),
		count,
		const_cast<const GLchar**>(paths),
		lengths
	);
	OGLPLUS_DEFERRED_CHECK(
		CompileShaderIncludeARB,
		ObjectError,
		Object(*this).
		EnumParam(Type())
	);
	return *this;
}
#endif

} // namespace oglplus


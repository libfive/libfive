/**
 *  @file oglplus/program.ipp
 *  @brief Implementation of Program
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/detail/info_log.hpp>
#include <oglplus/object/reference.hpp>
#include <oglplus/shader.hpp>
#include <oglplus/lib/incl_end.ipp>

namespace oglplus {

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>&
ObjectOps<tag::DirectState, tag::Program>::
AttachShader(ShaderName shader)
{
	OGLPLUS_GLFUNC(AttachShader)(
		_obj_name(),
		GetGLName(shader)
	);
	OGLPLUS_CHECK(
		AttachShader,
		ObjectPairError,
		Subject(shader).
		Object(*this)
	);
	return *this;
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>&
ObjectOps<tag::DirectState, tag::Program>::
AttachShaders(const Sequence<ShaderName>& shaders)
{
	for(std::size_t i=0, n=shaders.size(); i!=n; ++i)
	{
		this->AttachShader(shaders[i]);
	}
	return *this;
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>&
ObjectOps<tag::DirectState, tag::Program>::
DetachShader(ShaderName shader)
{
	OGLPLUS_GLFUNC(DetachShader)(
		_obj_name(),
		GetGLName(shader)
	);
	OGLPLUS_CHECK(
		DetachShader,
		ObjectPairError,
		Subject(shader).
		Object(*this)
	);
	return *this;
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>&
ObjectOps<tag::DirectState, tag::Program>::
Link(void)
{
	OGLPLUS_GLFUNC(LinkProgram)(_obj_name());
	OGLPLUS_CHECK(
		LinkProgram,
		ObjectError,
		Object(*this)
	);
	OGLPLUS_HANDLE_ERROR_IF(
		!IsLinked(),
		GL_INVALID_OPERATION,
		LinkError::Message(),
		LinkError,
		Log(GetInfoLog()).
		Object(*this)
	);
	return *this;
}

OGLPLUS_LIB_FUNC
Outcome<ObjectOps<tag::DirectState, tag::Program>&>
ObjectOps<tag::DirectState, tag::Program>::
Link(std::nothrow_t)
{
	OGLPLUS_GLFUNC(LinkProgram)(_obj_name());
	OGLPLUS_DEFERRED_CHECK(
		LinkProgram,
		ObjectError,
		Object(*this)
	);
	OGLPLUS_RETURN_HANDLER_IF(
		!IsLinked(),
		GL_INVALID_OPERATION,
		LinkError::Message(),
		LinkError,
		Log(GetInfoLog()).
		Object(*this)
	);
	return *this;
}

OGLPLUS_LIB_FUNC
Outcome<ObjectOps<tag::DirectState, tag::Program>&>
ObjectOps<tag::DirectState, tag::Program>::
Build(void)
{
	typedef Outcome<ObjectOps<tag::DirectState, tag::Program>&> Res;

	ShaderRange shaders = AttachedShaders();
	while(!shaders.Empty())
	{
		Reference<ShaderOps> shader = shaders.Front();
		if(!shader.IsCompiled())
		{
			if(auto outcome = Failed(shader.Compile(std::nothrow)))
			{
				return Res(outcome.ReleaseHandler(), *this);
			}
		}
		shaders.Next();
	}
	return Link(std::nothrow);
}

#if GL_ARB_shading_language_include
OGLPLUS_LIB_FUNC
Outcome<ObjectOps<tag::DirectState, tag::Program>&>
ObjectOps<tag::DirectState, tag::Program>::
BuildInclude(
	SizeType count,
	const GLchar* const* paths,
	const GLint* lengths
)
{
	typedef Outcome<ObjectOps<tag::DirectState, tag::Program>&> Res;

	ShaderRange shaders = AttachedShaders();
	while(!shaders.Empty())
	{
		Reference<ShaderOps> shader = shaders.Front();
		if(!shader.IsCompiled())
		{
			if(auto outcome = Failed(shader.CompileInclude(
				count,
				paths,
				lengths,
				std::nothrow
			)))
			{
				return Res(outcome.ReleaseHandler(), *this);
			}
		}
		shaders.Next();
	}
	return Link(std::nothrow);
}
#endif

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>&
ObjectOps<tag::DirectState, tag::Program>::
Validate(void)
{
	OGLPLUS_GLFUNC(ValidateProgram)(_obj_name());
	OGLPLUS_VERIFY(
		ValidateProgram,
		ObjectError,
		Object(*this)
	);
	OGLPLUS_HANDLE_ERROR_IF(
		!IsValid(),
		GL_INVALID_OPERATION,
		ValidationError::Message(),
		ValidationError,
		Log(GetInfoLog()).
		Object(*this)
	);
	return *this;
}

OGLPLUS_LIB_FUNC
Outcome<ObjectOps<tag::DirectState, tag::Program>&>
ObjectOps<tag::DirectState, tag::Program>::
Validate(std::nothrow_t)
{
	OGLPLUS_GLFUNC(ValidateProgram)(_obj_name());
	OGLPLUS_DEFERRED_CHECK(
		ValidateProgram,
		ObjectError,
		Object(*this)
	);
	OGLPLUS_RETURN_HANDLER_IF(
		!IsValid(),
		GL_INVALID_OPERATION,
		ValidationError::Message(),
		ValidationError,
		Log(GetInfoLog()).
		Object(*this)
	);
	return *this;
}

OGLPLUS_LIB_FUNC
String ObjectOps<tag::DirectState, tag::Program>::
GetInfoLog(void) const
{
	return aux::GetInfoLog(
		_obj_name(),
		OGLPLUS_GLFUNC(GetProgramiv),
		OGLPLUS_GLFUNC(GetProgramInfoLog),
		"GetProgramiv",
		"GetProgramInfoLog"
	);
}

OGLPLUS_LIB_FUNC
void ObjectOps<tag::DirectState, tag::Program>::
TransformFeedbackVaryings(
	SizeType count,
	const GLchar** varyings,
	TransformFeedbackMode mode
)
{
	OGLPLUS_GLFUNC(TransformFeedbackVaryings)(
		_obj_name(),
		count,
		varyings,
		GLenum(mode)
	);
	OGLPLUS_CHECK(
		TransformFeedbackVaryings,
		ObjectError,
		Object(*this)
	);
}

OGLPLUS_LIB_FUNC
void ObjectOps<tag::DirectState, tag::Program>::
TransformFeedbackVaryings(
	const std::vector<String>& varyings,
	TransformFeedbackMode mode
) const
{
	std::vector<const GLchar*> tmp(varyings.size());
	auto i = varyings.begin(), e = varyings.end();
	auto t = tmp.begin();
	while(i != e)
	{
		assert(t != tmp.end());
		*t = i->c_str();
		++i;
		++t;
	}
	OGLPLUS_GLFUNC(TransformFeedbackVaryings)(
		_obj_name(),
		GLsizei(tmp.size()),
		tmp.data(),
		GLenum(mode)
	);
	OGLPLUS_CHECK(
		TransformFeedbackVaryings,
		ObjectError,
		Object(*this)
	);
}

#if GL_VERSION_4_1 || GL_ARB_separate_shader_objects

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>&
ObjectOps<tag::DirectState, tag::Program>::
MakeSeparable(Boolean para)
{
	OGLPLUS_GLFUNC(ProgramParameteri)(
		_obj_name(),
		GL_PROGRAM_SEPARABLE,
		para._get()
	);
	OGLPLUS_CHECK(
		ProgramParameteri,
		ObjectError,
		Object(*this)
	);
	return *this;
}
#endif

#if GL_VERSION_4_1 || GL_ARB_get_program_binary

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>&
ObjectOps<tag::DirectState, tag::Program>::
MakeRetrievable(Boolean para)
{
	OGLPLUS_GLFUNC(ProgramParameteri)(
		_obj_name(),
		GL_PROGRAM_BINARY_RETRIEVABLE_HINT,
		para._get()
	);
	OGLPLUS_CHECK(
		ProgramParameteri,
		ObjectError,
		Object(*this)
	);
	return *this;
}

OGLPLUS_LIB_FUNC
void ObjectOps<tag::DirectState, tag::Program>::
GetBinary(std::vector<GLubyte>& binary, GLenum& format) const
{
	GLint size = GetIntParam(GL_PROGRAM_BINARY_LENGTH);
	if(size > 0)
	{
		GLsizei len = 0;
		binary.resize(std::size_t(size));
		OGLPLUS_GLFUNC(GetProgramBinary)(
			_obj_name(),
			size,
			&len,
			&format,
			binary.data()
		);
		OGLPLUS_CHECK(
			GetProgramBinary,
			ObjectError,
			Object(*this)
		);
	}
}

OGLPLUS_LIB_FUNC
void ObjectOps<tag::DirectState, tag::Program>::
Binary(const std::vector<GLubyte>& binary, GLenum format)
{
	OGLPLUS_GLFUNC(ProgramBinary)(
		_obj_name(),
		format,
		binary.data(),
		GLsizei(binary.size())
	);
	OGLPLUS_CHECK(
		ProgramBinary,
		ObjectError,
		Object(*this)
	);
}
#endif

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>::
ShaderIterationContext::ShaderIterationContext(
	GLuint name,
	GLuint count
): _shader_names(count)
{
	OGLPLUS_GLFUNC(GetAttachedShaders)(
		name,
		GLsizei(_shader_names.size()),
		nullptr,
		_shader_names.data()
	);
	OGLPLUS_CHECK(
		GetAttachedShaders,
		ObjectError,
		Object(ProgramName(name))
	);
}

#if GL_VERSION_4_3
OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>::InterfaceContext
ObjectOps<tag::DirectState, tag::Program>::
ActiveResourceContext(ProgramInterface intf) const
{
	// get the maximum string length of the longest identifier
	GLint length = 0;
	OGLPLUS_GLFUNC(GetProgramInterfaceiv)(
		_obj_name(),
		GLenum(intf),
		GL_MAX_NAME_LENGTH,
		&length
	);
	// for some interfaces the call above is not applicable
	// so GetError may return INVALID_OPERATION and we
	// silently ignore it here
	OGLPLUS_GLFUNC(GetError)();

	assert(!(length < 0));

	return InterfaceContext(_obj_name(), GLuint(length), GLenum(intf));
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>::ActiveResourceRange
ObjectOps<tag::DirectState, tag::Program>::
ActiveResources(ProgramInterface intf) const
{
	// get the count of active attributes
	GLint count = 0;
	OGLPLUS_GLFUNC(GetProgramInterfaceiv)(
		_obj_name(),
		GLenum(intf),
		GL_ACTIVE_RESOURCES,
		&count
	);
	OGLPLUS_VERIFY(
		GetProgramInterfaceiv,
		ObjectError,
		Object(*this).
		EnumParam(intf)
	);

	assert(!(count < 0));

	return ActiveResourceRange(ActiveResourceContext(intf), unsigned(count));
}
#endif

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>::InterfaceContext
ObjectOps<tag::DirectState, tag::Program>::
ActiveAttribContext(void) const
{
	return InterfaceContext(
		_obj_name(),
		GetUIntParam(GL_ACTIVE_ATTRIBUTE_MAX_LENGTH)
	);
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>::ActiveAttribRange
ObjectOps<tag::DirectState, tag::Program>::
ActiveAttribs(void) const
{
	return ActiveAttribRange(
		ActiveAttribContext(),
		GetUIntParam(GL_ACTIVE_ATTRIBUTES)
	);
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>::InterfaceContext
ObjectOps<tag::DirectState, tag::Program>::
ActiveUniformContext(void) const
{
	return InterfaceContext(
		_obj_name(),
		GetUIntParam(GL_ACTIVE_UNIFORM_MAX_LENGTH)
	);
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>::ActiveUniformRange
ObjectOps<tag::DirectState, tag::Program>::
ActiveUniforms(void) const
{
	return ActiveUniformRange(
		ActiveUniformContext(),
		GetUIntParam(GL_ACTIVE_UNIFORMS)
	);
}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0 || GL_ARB_shader_subroutine
OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>::InterfaceContext
ObjectOps<tag::DirectState, tag::Program>::
ActiveSubroutineContext(ShaderType stage) const
{
	return InterfaceContext(
		_obj_name(),
		GetStageUIntParam(
			GLenum(stage),
			GL_ACTIVE_SUBROUTINE_MAX_LENGTH
		),
		GLenum(stage)
	);
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>::ActiveSubroutineRange
ObjectOps<tag::DirectState, tag::Program>::
ActiveSubroutines(ShaderType stage) const
{
	return ActiveSubroutineRange(
		ActiveSubroutineContext(stage),
		GetStageUIntParam(
			GLenum(stage),
			GL_ACTIVE_SUBROUTINES
		)
	);
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>::InterfaceContext
ObjectOps<tag::DirectState, tag::Program>::
ActiveSubroutineUniformContext(ShaderType stage) const
{
	return InterfaceContext(
		_obj_name(),
		GetStageUIntParam(
			GLenum(stage),
			GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH
		),
		GLenum(stage)
	);
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>::ActiveSubroutineUniformRange
ObjectOps<tag::DirectState, tag::Program>::
ActiveSubroutineUniforms(ShaderType stage) const
{
	return ActiveSubroutineUniformRange(
		ActiveSubroutineUniformContext(stage),
		GetStageUIntParam(
			GLenum(stage),
			GL_ACTIVE_SUBROUTINE_UNIFORMS
		)
	);
}
#endif

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>::InterfaceContext
ObjectOps<tag::DirectState, tag::Program>::
TransformFeedbackVaryingContext(void) const
{
	return InterfaceContext(
		_obj_name(),
		GetUIntParam(GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH)
	);
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>::TransformFeedbackVaryingRange
ObjectOps<tag::DirectState, tag::Program>::
TransformFeedbackVaryings(void) const
{
	return TransformFeedbackVaryingRange(
		TransformFeedbackVaryingContext(),
		GetUIntParam(GL_TRANSFORM_FEEDBACK_VARYINGS)
	);
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>::ShaderRange
ObjectOps<tag::DirectState, tag::Program>::
AttachedShaders(void) const
{
	GLuint count = GetUIntParam(GL_ATTACHED_SHADERS);
	return ShaderRange(
		ShaderIterationContext(_obj_name(), count),
		0, count
	);
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Program>::ActiveUniformBlockRange
ObjectOps<tag::DirectState, tag::Program>::
ActiveUniformBlocks(void) const
{
	// get the count of active uniform blocks
	GLuint count = GetUIntParam(GL_ACTIVE_UNIFORM_BLOCKS);
	GLuint length = 0;
	if(count > 0)
	{
		// get the string length of the first identifier
		length = GetUIntParam(GL_UNIFORM_BLOCK_NAME_LENGTH);
	}
	return ActiveUniformBlockRange(
		aux::ProgramInterfaceContext(_obj_name(), length),
		0, count
	);
}

#if GL_VERSION_4_1 || GL_ARB_separate_shader_objects

OGLPLUS_LIB_FUNC
ProgramName ShaderProgram::_make(
	ShaderType shader_type,
	GLsizei count,
	const GLchar* const* strings
)
{
	GLuint program = OGLPLUS_GLFUNC(CreateShaderProgramv)(
		GLenum(shader_type),
		count,
		const_cast<const GLchar**>(strings)
	);
	OGLPLUS_CHECK(
		CreateShaderProgramv,
		Error,
		EnumParam(shader_type)
	);
	return ProgramName(program);
}

OGLPLUS_LIB_FUNC
void ShaderProgram::_check(void)
{
	OGLPLUS_HANDLE_ERROR_IF(
		!IsValid(),
		GL_INVALID_OPERATION,
		ValidationError::Message(),
		ValidationError,
		Log(GetInfoLog()).
		Object(*this)
	);
}
#endif

} // namespace oglplus


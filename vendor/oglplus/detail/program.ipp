/**
 *  .file oglplus/detail/program.ipp
 *  .brief Implementation of program helpers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/error/object.hpp>
#include <oglplus/lib/incl_end.ipp>

namespace oglplus {
namespace aux {

OGLPLUS_LIB_FUNC
std::vector<GLchar>& ProgramInterfaceContext::Buffer(void)
{
	if(_size && _buffer.empty())
		_buffer.resize(_size);
	return _buffer;
}

OGLPLUS_LIB_FUNC
ActiveVariableInfo::ActiveVariableInfo(
	ProgramInterfaceContext& context,
	GLuint index,
	void (GLAPIENTRY *GetActiveVariable)(
		GLuint /*program*/,
		GLuint /*index*/,
		GLsizei /*bufsize*/,
		GLsizei* /*length*/,
		GLint* /*size*/,
		GLenum* /*type*/,
		GLchar* /*name*/
	)
): _index(index)
 , _size(0)
{
	GLsizei strlen = 0;
	GetActiveVariable(
		GetGLName(context.Program()),
		index,
		GLsizei(context.Buffer().size()),
		&strlen,
		&_size,
		&_type,
		context.Buffer().data()
	);

	assert(!(strlen < 0));

	_var_name = String(context.Buffer().data(), std::size_t(strlen));
}

OGLPLUS_LIB_FUNC
ActiveAttribInfo::ActiveAttribInfo(
	ProgramInterfaceContext& context,
	GLuint index
): ActiveVariableInfo(
	context,
	index,
	OGLPLUS_GLFUNC(GetActiveAttrib)
)
{
	OGLPLUS_CHECK(
		GetActiveAttrib,
		ObjectError,
		Object(context.Program()).
		Index(index)
	);
}

OGLPLUS_LIB_FUNC
ActiveUniformInfo::ActiveUniformInfo(
	ProgramInterfaceContext& context,
	GLuint index
): ActiveVariableInfo(
	context,
	index,
	OGLPLUS_GLFUNC(GetActiveUniform)
)
{
	OGLPLUS_CHECK(
		GetActiveUniform,
		ObjectError,
		Object(context.Program()).
		Index(index)
	);
}

#if GL_VERSION_4_0 || GL_ARB_shader_subroutine

OGLPLUS_LIB_FUNC
ActiveSubroutineInfo::ActiveSubroutineInfo(
	ProgramInterfaceContext& context,
	GLuint index
): _index(index)
{
	GLsizei strlen = 0;
	OGLPLUS_GLFUNC(GetActiveSubroutineName)(
		GetGLName(context.Program()),
		context.Stage(),
		index,
		GLsizei(context.Buffer().size()),
		&strlen,
		context.Buffer().data()
	);
	OGLPLUS_CHECK(
		GetActiveSubroutineName,
		ObjectError,
		Object(context.Program()).
		EnumParam(context.Stage()).
		Index(index)
	);

	assert(!(strlen < 0));

	_var_name = String(context.Buffer().data(), std::size_t(strlen));
}

OGLPLUS_LIB_FUNC
SLDataType ActiveSubroutineInfo::Type(void) const
{
#ifdef None
#pragma push_macro("None")
#undef None
	return SLDataType::None;
#pragma pop_macro("None")
#else
	return SLDataType::None;
#endif // None
}

OGLPLUS_LIB_FUNC
ActiveSubroutineUniformInfo::ActiveSubroutineUniformInfo(
	ProgramInterfaceContext& context,
	GLuint index
): _index(index)
 , _size(0)
{
	OGLPLUS_GLFUNC(GetActiveSubroutineUniformiv)(
		GetGLName(context.Program()),
		context.Stage(),
		index,
		GL_UNIFORM_SIZE,
		&_size
	);
	OGLPLUS_CHECK(
		GetActiveSubroutineUniformiv,
		ObjectError,
		Object(context.Program()).
		EnumParam(context.Stage()).
		Index(index)
	);

	GLsizei strlen = 0;
	OGLPLUS_GLFUNC(GetActiveSubroutineUniformName)(
		GetGLName(context.Program()),
		context.Stage(),
		index,
		GLsizei(context.Buffer().size()),
		&strlen,
		context.Buffer().data()
	);
	OGLPLUS_CHECK(
		GetActiveSubroutineUniformName,
		ObjectError,
		Object(context.Program()).
		EnumParam(context.Stage()).
		Index(index)
	);

	assert(!(strlen < 0));

	_var_name = String(context.Buffer().data(), std::size_t(strlen));
}

OGLPLUS_LIB_FUNC
SLDataType ActiveSubroutineUniformInfo::Type(void) const
{
#ifdef None
#pragma push_macro("None")
#undef None
	return SLDataType::None;
#pragma pop_macro("None")
#else
	return SLDataType::None;
#endif // None
}

#endif // GL_VERSION_4_0 || GL_ARB_shader_subroutine

OGLPLUS_LIB_FUNC
TransformFeedbackVaryingInfo::TransformFeedbackVaryingInfo(
	ProgramInterfaceContext& context,
	GLuint index
): ActiveVariableInfo(
	context,
	index,
	OGLPLUS_GLFUNC(GetTransformFeedbackVarying)
)
{
	OGLPLUS_CHECK(
		GetTransformFeedbackVarying,
		ObjectError,
		Object(context.Program()).
		Index(index)
	);
}

OGLPLUS_LIB_FUNC
ActiveUniformBlockInfo::ActiveUniformBlockInfo(
	ProgramInterfaceContext& context,
	GLuint index
): _index(0) // TODO := index ?
{
	GLint length = 0;
	OGLPLUS_GLFUNC(GetProgramiv)(
		GetGLName(context.Program()),
		GL_UNIFORM_BLOCK_NAME_LENGTH,
		&length
	);
	OGLPLUS_VERIFY(
		GetProgramiv,
		ObjectError,
		Object(context.Program()).
		EnumParam(GLenum(GL_UNIFORM_BLOCK_NAME_LENGTH))
	);

	assert(!(length < 0));

	if(context.Buffer().size() < std::size_t(length))
	{
		context.Buffer().resize(std::size_t(length));
	}

	GLsizei strlen = 0;
	OGLPLUS_GLFUNC(GetActiveUniformBlockName)(
		GetGLName(context.Program()),
		index,
		GLsizei(context.Buffer().size()),
		&strlen,
		context.Buffer().data()
	);
	OGLPLUS_CHECK(
		GetActiveUniformBlockName,
		ObjectError,
		Object(context.Program()).
		Index(index)
	);

	assert(!(strlen < 0));

	_var_name = String(context.Buffer().data(), std::size_t(strlen));
}

OGLPLUS_LIB_FUNC
SLDataType ActiveUniformBlockInfo::Type(void) const
{
#ifdef None
#pragma push_macro("None")
#undef None
	return SLDataType::None;
#pragma pop_macro("None")
#else
	return SLDataType::None;
#endif // None
}

} // namespace aux
} // namespace oglplus


/**
 *  @file oglplus/error/basic.ipp
 *  @brief Implementation of Error / exceptions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {

OGLPLUS_LIB_FUNC
const char* Error::Message(GLenum code)
{
	switch(code)
	{
		case GL_OUT_OF_MEMORY:
			return "OpenGL out of memory";
		case GL_INVALID_ENUM:
			return "Invalid OpenGL enum argument";
		case GL_INVALID_VALUE:
			return "OpenGL numeric argument out of range";
		case GL_INVALID_OPERATION:
			return "Invalid OpenGL operation";
		case GL_INVALID_FRAMEBUFFER_OPERATION:
			return "Invalid OpenGL framebuffer operation";
#ifdef GL_STACK_OVERFLOW
		case GL_STACK_OVERFLOW:
			return "Stack overflow";
#endif
#ifdef GL_STACK_UNDERFLOW
		case GL_STACK_UNDERFLOW:
			return "Stack underflow";
#endif
#ifdef GL_TABLE_TOO_LARGE
		case GL_TABLE_TOO_LARGE:
			return "Table too large";
#endif
		default:;
	}
	return "Unknown error";
}

OGLPLUS_LIB_FUNC
Error::Error(const char* message)
 : std::runtime_error(message)
 , _code(0)
#if !OGLPLUS_ERROR_NO_FILE
 , _file(nullptr)
#endif
#if !OGLPLUS_ERROR_NO_LINE
 , _line(0)
#endif
#if !OGLPLUS_ERROR_NO_GL_LIB
 , _gllib_name("gl")
#endif
#if !OGLPLUS_ERROR_NO_GL_FUNC
 , _glfunc_name(nullptr)
#endif
#if !OGLPLUS_ERROR_NO_GL_SYMBOL
 , _enumpar_name(nullptr)
 , _enumpar(0)
 , _index(-1)
#endif
{ }

OGLPLUS_LIB_FUNC
const char* Error::SourceFile(void) const
{
#if !OGLPLUS_ERROR_NO_FILE
	return _file;
#else
	return nullptr;
#endif
}

OGLPLUS_LIB_FUNC
const char* Error::SourceFunc(void) const
{
#if !OGLPLUS_ERROR_NO_FUNC
	return _func;
#else
	return nullptr;
#endif
}

OGLPLUS_LIB_FUNC
unsigned Error::SourceLine(void) const
{
#if !OGLPLUS_ERROR_NO_LINE
	return _line;
#else
	return 0u;
#endif
}

OGLPLUS_LIB_FUNC
const char* Error::GLLib(void) const
{
#if !OGLPLUS_ERROR_NO_GL_LIB
	return _gllib_name;
#else
	return nullptr;
#endif
}

OGLPLUS_LIB_FUNC
const char* Error::GLFunc(void) const
{
#if !OGLPLUS_ERROR_NO_GL_FUNC
	return _glfunc_name;
#else
	return nullptr;
#endif
}

OGLPLUS_LIB_FUNC
GLenum Error::EnumParam(void) const
{
#if !OGLPLUS_ERROR_NO_GL_SYMBOL
	return _enumpar;
#else
	return 0;
#endif
}

OGLPLUS_LIB_FUNC
const char* Error::EnumParamName(void) const
{
#if !OGLPLUS_ERROR_NO_GL_SYMBOL
	return _enumpar_name;
#else
	return nullptr;
#endif
}

OGLPLUS_LIB_FUNC
GLint Error::Index(void) const
{
#if !OGLPLUS_ERROR_NO_GL_SYMBOL
	return _index;
#else
	return -1;
#endif
}

} // namespace oglplus


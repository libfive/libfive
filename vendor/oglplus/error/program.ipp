/**
 *  @file oglplus/error/program.ipp
 *  @brief Implementation of program exceptions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {

OGLPLUS_LIB_FUNC
const char* CompileError::Message(void)
{
	return "OpenGL shading language compilation error";
}

OGLPLUS_LIB_FUNC
const char* LinkError::Message(void)
{
	return "OpenGL shading language program link error";
}

OGLPLUS_LIB_FUNC
const char* ValidationError::Message(void)
{
	return "OpenGL shading language program validation error";
}

} // namespace oglplus


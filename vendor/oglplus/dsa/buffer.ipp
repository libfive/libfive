/**
 *  @file oglplus/dsa/buffer.ipp
 *  @brief Implementation of DSA Buffer functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {

#if GL_VERSION_4_5 || GL_ARB_direct_state_access
OGLPLUS_LIB_FUNC
GLint ObjectOps<tag::DirectState, tag::Buffer>::
GetIntParam(GLenum query) const
{
	GLint value = 0;
	OGLPLUS_GLFUNC(GetNamedBufferParameteriv)(
		_name,
		query,
		&value
	);
	OGLPLUS_VERIFY(
		GetNamedBufferParameteriv,
		ObjectError,
		Object(BufferName(_name)).
		EnumParam(query)
	);
	return value;
}

#endif // GL_ARB_direct_state_access

} // namespace oglplus


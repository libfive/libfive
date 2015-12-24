/**
 *  @file oglplus/dsa/transform_feedback.ipp
 *  @brief Implementation of DSA TransformFeedback functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {

#if GL_VERSION_4_5 || GL_ARB_direct_state_access
OGLPLUS_LIB_FUNC
GLint ObjectOps<tag::DirectState, tag::TransformFeedback>::
GetIntParam(GLenum query) const
{
	GLint result = 0;
	OGLPLUS_GLFUNC(GetTransformFeedbackiv)(
		_obj_name(),
		query,
		&result
	);
	OGLPLUS_VERIFY(
		GetTransformFeedbackiv,
		ObjectError,
		Object(*this).
		EnumParam(query)
	);
	return result;
}

OGLPLUS_LIB_FUNC
GLint ObjectOps<tag::DirectState, tag::TransformFeedback>::
GetIntParam(GLenum query, GLuint index) const
{
	GLint result = 0;
	OGLPLUS_GLFUNC(GetTransformFeedbacki_v)(
		_obj_name(),
		query,
		index,
		&result
	);
	OGLPLUS_VERIFY(
		GetTransformFeedbacki_v,
		ObjectError,
		Object(*this).
		EnumParam(query).
		Index(index)
	);
	return result;
}

OGLPLUS_LIB_FUNC
GLint64 ObjectOps<tag::DirectState, tag::TransformFeedback>::
GetInt64Param(GLenum query, GLuint index) const
{
	GLint64 result = 0;
	OGLPLUS_GLFUNC(GetTransformFeedbacki64_v)(
		_obj_name(),
		query,
		index,
		&result
	);
	OGLPLUS_VERIFY(
		GetTransformFeedbacki64_v,
		ObjectError,
		Object(*this).
		EnumParam(query).
		Index(index)
	);
	return result;
}

#endif // GL_ARB_direct_state_access

} // namespace oglplus


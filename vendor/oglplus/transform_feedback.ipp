/**
 *  @file oglplus/transform_feedback.ipp
 *  @brief Implementation of TransformFeedback functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_0 || GL_ARB_transform_feedback2

OGLPLUS_LIB_FUNC
GLenum ObjBindingOps<tag::TransformFeedback>::
_binding_query(TransformFeedbackTarget target)
{
	switch(GLenum(target))
	{
#include <oglplus/enums/transform_feedback_target_bq.ipp>
		default:;
	}
	return 0;
}

OGLPLUS_LIB_FUNC
GLuint ObjBindingOps<tag::TransformFeedback>::
_binding(TransformFeedbackTarget target)
{
	GLint name = 0;
	OGLPLUS_GLFUNC(GetIntegerv)(_binding_query(target), &name);
	OGLPLUS_VERIFY(
		GetIntegerv,
		Error,
		EnumParam(_binding_query(target))
	);

	assert(!(name < 0));
	return GLuint(name);
}

#endif // GL_VERSION_4_0

} // namespace oglplus


/**
 *  @file oglplus/framebuffer.ipp
 *  @brief Implementation of Framebuffer functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {

OGLPLUS_LIB_FUNC
GLenum ObjBindingOps<tag::Framebuffer>::
_binding_query(Target target)
{
	switch(GLenum(target))
	{
#include <oglplus/enums/framebuffer_target_bq.ipp>
		default:;
	}
	return 0;
}

OGLPLUS_LIB_FUNC
GLuint ObjBindingOps<tag::Framebuffer>::
_binding(Target target)
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

OGLPLUS_LIB_FUNC
void ObjectOps<tag::ExplicitSel, tag::Framebuffer>::
HandleIncompleteError(Target target, FramebufferStatus status)
{
	OGLPLUS_HANDLE_ERROR_IF(
		true,
		GL_INVALID_FRAMEBUFFER_OPERATION,
		IncompleteFramebuffer::Message(),
		IncompleteFramebuffer,
		Status(status).
		ObjectBinding(target).
		GLFunc("CheckFramebufferStatus")
	);
}

} // namespace oglplus


/**
 *  @file oglplus/dsa/ext/framebuffer.ipp
 *  @brief Implementation of DSA Framebuffer functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {

#if GL_EXT_direct_state_access

OGLPLUS_LIB_FUNC
void ObjectOps<tag::DirectStateEXT, tag::Framebuffer>::
HandleIncompleteError(FramebufferStatus status) const
{
	OGLPLUS_HANDLE_ERROR_IF(
		true,
		GL_INVALID_FRAMEBUFFER_OPERATION,
		IncompleteFramebuffer::Message(),
		IncompleteFramebuffer,
		Status(status).
		Object(*this).
		GLFunc("CheckNamedFramebufferStatusEXT")
	);
}

#endif // GL_EXT_direct_state_access

} // namespace oglplus


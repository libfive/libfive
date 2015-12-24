/**
 *  @file oglplus/error/framebuffer.ipp
 *  @brief Implementation of Framebuffer exceptions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {

OGLPLUS_LIB_FUNC
const char* IncompleteFramebuffer::Message(void)
{
	return "Framebuffer is incomplete";
}

} // namespace oglplus


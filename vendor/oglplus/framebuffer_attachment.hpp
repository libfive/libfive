/**
 *  @file oglplus/framebuffer_attachment.hpp
 *  @brief OpenGL Framebuffer attachment enumerations.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_FRAMEBUFFER_ATTACHMENT_1312081013_HPP
#define OGLPLUS_FRAMEBUFFER_ATTACHMENT_1312081013_HPP

#include <oglplus/limited_value.hpp>
#include <oglplus/enums/framebuffer_buffer.hpp>
#include <oglplus/enums/framebuffer_attachment.hpp>
#include <oglplus/enums/framebuffer_color_attachment.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY
/// Type for the framebuffer color attachment (implementation-dependent) number
class FramebufferColorAttachmentNumber
 : public LimitedCount
{
public:
	FramebufferColorAttachmentNumber(GLuint count);
};
#else
OGLPLUS_DECLARE_LIMITED_COUNT_TYPE(
	FramebufferColorAttachmentNumber,
	MAX_COLOR_ATTACHMENTS
)
#endif

} // namespace oglplus

#endif // include guard

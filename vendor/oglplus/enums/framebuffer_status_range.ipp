//  File implement/oglplus/enums/framebuffer_status_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/framebuffer_status.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLenum*,
	FramebufferStatus
> ValueRange_(FramebufferStatus*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_FRAMEBUFFERSTATUS)
#define OGLPLUS_IMPL_EVR_FRAMEBUFFERSTATUS
{
static const GLenum _values[] = {
#if defined GL_FRAMEBUFFER_COMPLETE
GL_FRAMEBUFFER_COMPLETE,
#endif
#if defined GL_FRAMEBUFFER_UNDEFINED
GL_FRAMEBUFFER_UNDEFINED,
#endif
#if defined GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT,
#endif
#if defined GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT,
#endif
#if defined GL_FRAMEBUFFER_UNSUPPORTED
GL_FRAMEBUFFER_UNSUPPORTED,
#endif
#if defined GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE
GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE,
#endif
#if defined GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS
GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	FramebufferStatus
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


//  File implement/oglplus/enums/framebuffer_buffer_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/framebuffer_buffer.txt'
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
	FramebufferBuffer
> ValueRange_(FramebufferBuffer*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_FRAMEBUFFERBUFFER)
#define OGLPLUS_IMPL_EVR_FRAMEBUFFERBUFFER
{
static const GLenum _values[] = {
#if defined GL_COLOR
GL_COLOR,
#endif
#if defined GL_DEPTH
GL_DEPTH,
#endif
#if defined GL_STENCIL
GL_STENCIL,
#endif
#if defined GL_DEPTH_STENCIL
GL_DEPTH_STENCIL,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	FramebufferBuffer
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


//  File implement/oglplus/enums/buffer_usage_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/buffer_usage.txt'
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
	BufferUsage
> ValueRange_(BufferUsage*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_BUFFERUSAGE)
#define OGLPLUS_IMPL_EVR_BUFFERUSAGE
{
static const GLenum _values[] = {
#if defined GL_STREAM_DRAW
GL_STREAM_DRAW,
#endif
#if defined GL_STREAM_READ
GL_STREAM_READ,
#endif
#if defined GL_STREAM_COPY
GL_STREAM_COPY,
#endif
#if defined GL_STATIC_DRAW
GL_STATIC_DRAW,
#endif
#if defined GL_STATIC_READ
GL_STATIC_READ,
#endif
#if defined GL_STATIC_COPY
GL_STATIC_COPY,
#endif
#if defined GL_DYNAMIC_DRAW
GL_DYNAMIC_DRAW,
#endif
#if defined GL_DYNAMIC_READ
GL_DYNAMIC_READ,
#endif
#if defined GL_DYNAMIC_COPY
GL_DYNAMIC_COPY,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	BufferUsage
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


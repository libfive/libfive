//  File implement/oglplus/enums/buffer_usage_names.ipp
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
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	BufferUsage*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_BUFFERUSAGE)
#define OGLPLUS_IMPL_EVN_BUFFERUSAGE
{
switch(value)
{
#if defined GL_STREAM_DRAW
	case GL_STREAM_DRAW: return StrCRef("STREAM_DRAW");
#endif
#if defined GL_STREAM_READ
	case GL_STREAM_READ: return StrCRef("STREAM_READ");
#endif
#if defined GL_STREAM_COPY
	case GL_STREAM_COPY: return StrCRef("STREAM_COPY");
#endif
#if defined GL_STATIC_DRAW
	case GL_STATIC_DRAW: return StrCRef("STATIC_DRAW");
#endif
#if defined GL_STATIC_READ
	case GL_STATIC_READ: return StrCRef("STATIC_READ");
#endif
#if defined GL_STATIC_COPY
	case GL_STATIC_COPY: return StrCRef("STATIC_COPY");
#endif
#if defined GL_DYNAMIC_DRAW
	case GL_DYNAMIC_DRAW: return StrCRef("DYNAMIC_DRAW");
#endif
#if defined GL_DYNAMIC_READ
	case GL_DYNAMIC_READ: return StrCRef("DYNAMIC_READ");
#endif
#if defined GL_DYNAMIC_COPY
	case GL_DYNAMIC_COPY: return StrCRef("DYNAMIC_COPY");
#endif
	default:;
}
OGLPLUS_FAKE_USE(value);
return StrCRef();
}
#else
;
#endif
} // namespace enums


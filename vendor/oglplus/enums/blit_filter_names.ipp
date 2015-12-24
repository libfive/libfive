//  File implement/oglplus/enums/blit_filter_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/blit_filter.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	BlitFilter*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_BLITFILTER)
#define OGLPLUS_IMPL_EVN_BLITFILTER
{
switch(value)
{
#if defined GL_NEAREST
	case GL_NEAREST: return StrCRef("NEAREST");
#endif
#if defined GL_LINEAR
	case GL_LINEAR: return StrCRef("LINEAR");
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


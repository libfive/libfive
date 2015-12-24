//  File implement/oglplus/enums/clip_origin_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/clip_origin.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	ClipOrigin*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_CLIPORIGIN)
#define OGLPLUS_IMPL_EVN_CLIPORIGIN
{
switch(value)
{
#if defined GL_LOWER_LEFT
	case GL_LOWER_LEFT: return StrCRef("LOWER_LEFT");
#endif
#if defined GL_UPPER_LEFT
	case GL_UPPER_LEFT: return StrCRef("UPPER_LEFT");
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


//  File implement/oglplus/enums/texture_swizzle_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/texture_swizzle.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	TextureSwizzle*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_TEXTURESWIZZLE)
#define OGLPLUS_IMPL_EVN_TEXTURESWIZZLE
{
switch(value)
{
#if defined GL_RED
	case GL_RED: return StrCRef("RED");
#endif
#if defined GL_GREEN
	case GL_GREEN: return StrCRef("GREEN");
#endif
#if defined GL_BLUE
	case GL_BLUE: return StrCRef("BLUE");
#endif
#if defined GL_ALPHA
	case GL_ALPHA: return StrCRef("ALPHA");
#endif
#if defined GL_ZERO
	case GL_ZERO: return StrCRef("ZERO");
#endif
#if defined GL_ONE
	case GL_ONE: return StrCRef("ONE");
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


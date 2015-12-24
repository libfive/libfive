//  File implement/oglplus/enums/texture_swizzle_coord_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/texture_swizzle_coord.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	TextureSwizzleCoord*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_TEXTURESWIZZLECOORD)
#define OGLPLUS_IMPL_EVN_TEXTURESWIZZLECOORD
{
switch(value)
{
#if defined GL_TEXTURE_SWIZZLE_R
	case GL_TEXTURE_SWIZZLE_R: return StrCRef("TEXTURE_SWIZZLE_R");
#endif
#if defined GL_TEXTURE_SWIZZLE_G
	case GL_TEXTURE_SWIZZLE_G: return StrCRef("TEXTURE_SWIZZLE_G");
#endif
#if defined GL_TEXTURE_SWIZZLE_B
	case GL_TEXTURE_SWIZZLE_B: return StrCRef("TEXTURE_SWIZZLE_B");
#endif
#if defined GL_TEXTURE_SWIZZLE_A
	case GL_TEXTURE_SWIZZLE_A: return StrCRef("TEXTURE_SWIZZLE_A");
#endif
#if defined GL_TEXTURE_SWIZZLE_RGBA
	case GL_TEXTURE_SWIZZLE_RGBA: return StrCRef("TEXTURE_SWIZZLE_RGBA");
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


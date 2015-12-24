//  File implement/oglplus/enums/texture_wrap_coord_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/texture_wrap_coord.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	TextureWrapCoord*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_TEXTUREWRAPCOORD)
#define OGLPLUS_IMPL_EVN_TEXTUREWRAPCOORD
{
switch(value)
{
#if defined GL_TEXTURE_WRAP_S
	case GL_TEXTURE_WRAP_S: return StrCRef("TEXTURE_WRAP_S");
#endif
#if defined GL_TEXTURE_WRAP_T
	case GL_TEXTURE_WRAP_T: return StrCRef("TEXTURE_WRAP_T");
#endif
#if defined GL_TEXTURE_WRAP_R
	case GL_TEXTURE_WRAP_R: return StrCRef("TEXTURE_WRAP_R");
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


//  File implement/oglplus/enums/ext/nv_path_color_format_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_color_format.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	PathNVColorFormat*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PATHNVCOLORFORMAT)
#define OGLPLUS_IMPL_EVN_PATHNVCOLORFORMAT
{
switch(value)
{
#if defined GL_LUMINANCE
	case GL_LUMINANCE: return StrCRef("LUMINANCE");
#endif
#if defined GL_ALPHA
	case GL_ALPHA: return StrCRef("ALPHA");
#endif
#if defined GL_INTENSITY
	case GL_INTENSITY: return StrCRef("INTENSITY");
#endif
#if defined GL_LUMINANCE_ALPHA
	case GL_LUMINANCE_ALPHA: return StrCRef("LUMINANCE_ALPHA");
#endif
#if defined GL_RGB
	case GL_RGB: return StrCRef("RGB");
#endif
#if defined GL_RGBA
	case GL_RGBA: return StrCRef("RGBA");
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


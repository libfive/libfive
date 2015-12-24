//  File implement/oglplus/enums/pixel_data_format_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/pixel_data_format.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	PixelDataFormat*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PIXELDATAFORMAT)
#define OGLPLUS_IMPL_EVN_PIXELDATAFORMAT
{
switch(value)
{
#if defined GL_DEPTH_COMPONENT
	case GL_DEPTH_COMPONENT: return StrCRef("DEPTH_COMPONENT");
#endif
#if defined GL_DEPTH_STENCIL
	case GL_DEPTH_STENCIL: return StrCRef("DEPTH_STENCIL");
#endif
#if defined GL_STENCIL_INDEX
	case GL_STENCIL_INDEX: return StrCRef("STENCIL_INDEX");
#endif
#if defined GL_RED
	case GL_RED: return StrCRef("RED");
#endif
#if defined GL_GREEN
	case GL_GREEN: return StrCRef("GREEN");
#endif
#if defined GL_BLUE
	case GL_BLUE: return StrCRef("BLUE");
#endif
#if defined GL_RG
	case GL_RG: return StrCRef("RG");
#endif
#if defined GL_RGB
	case GL_RGB: return StrCRef("RGB");
#endif
#if defined GL_RGBA
	case GL_RGBA: return StrCRef("RGBA");
#endif
#if defined GL_BGR
	case GL_BGR: return StrCRef("BGR");
#endif
#if defined GL_BGRA
	case GL_BGRA: return StrCRef("BGRA");
#endif
#if defined GL_RED_INTEGER
	case GL_RED_INTEGER: return StrCRef("RED_INTEGER");
#endif
#if defined GL_GREEN_INTEGER
	case GL_GREEN_INTEGER: return StrCRef("GREEN_INTEGER");
#endif
#if defined GL_BLUE_INTEGER
	case GL_BLUE_INTEGER: return StrCRef("BLUE_INTEGER");
#endif
#if defined GL_RG_INTEGER
	case GL_RG_INTEGER: return StrCRef("RG_INTEGER");
#endif
#if defined GL_RGB_INTEGER
	case GL_RGB_INTEGER: return StrCRef("RGB_INTEGER");
#endif
#if defined GL_RGBA_INTEGER
	case GL_RGBA_INTEGER: return StrCRef("RGBA_INTEGER");
#endif
#if defined GL_BGR_INTEGER
	case GL_BGR_INTEGER: return StrCRef("BGR_INTEGER");
#endif
#if defined GL_BGRA_INTEGER
	case GL_BGRA_INTEGER: return StrCRef("BGRA_INTEGER");
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


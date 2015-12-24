//  File implement/oglplus/enums/image_unit_format_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/image_unit_format.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	ImageUnitFormat*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_IMAGEUNITFORMAT)
#define OGLPLUS_IMPL_EVN_IMAGEUNITFORMAT
{
switch(value)
{
#if defined GL_RGBA32F
	case GL_RGBA32F: return StrCRef("RGBA32F");
#endif
#if defined GL_RGBA16F
	case GL_RGBA16F: return StrCRef("RGBA16F");
#endif
#if defined GL_RG32F
	case GL_RG32F: return StrCRef("RG32F");
#endif
#if defined GL_RG16F
	case GL_RG16F: return StrCRef("RG16F");
#endif
#if defined GL_R11F_G11F_B10F
	case GL_R11F_G11F_B10F: return StrCRef("R11F_G11F_B10F");
#endif
#if defined GL_R32F
	case GL_R32F: return StrCRef("R32F");
#endif
#if defined GL_R16F
	case GL_R16F: return StrCRef("R16F");
#endif
#if defined GL_RGBA32UI
	case GL_RGBA32UI: return StrCRef("RGBA32UI");
#endif
#if defined GL_RGBA16UI
	case GL_RGBA16UI: return StrCRef("RGBA16UI");
#endif
#if defined GL_RGB10_A2UI
	case GL_RGB10_A2UI: return StrCRef("RGB10_A2UI");
#endif
#if defined GL_RGBA8UI
	case GL_RGBA8UI: return StrCRef("RGBA8UI");
#endif
#if defined GL_RG32UI
	case GL_RG32UI: return StrCRef("RG32UI");
#endif
#if defined GL_RG16UI
	case GL_RG16UI: return StrCRef("RG16UI");
#endif
#if defined GL_RG8UI
	case GL_RG8UI: return StrCRef("RG8UI");
#endif
#if defined GL_R32UI
	case GL_R32UI: return StrCRef("R32UI");
#endif
#if defined GL_R16UI
	case GL_R16UI: return StrCRef("R16UI");
#endif
#if defined GL_R8UI
	case GL_R8UI: return StrCRef("R8UI");
#endif
#if defined GL_RGBA32I
	case GL_RGBA32I: return StrCRef("RGBA32I");
#endif
#if defined GL_RGBA16I
	case GL_RGBA16I: return StrCRef("RGBA16I");
#endif
#if defined GL_RGBA8I
	case GL_RGBA8I: return StrCRef("RGBA8I");
#endif
#if defined GL_RG32I
	case GL_RG32I: return StrCRef("RG32I");
#endif
#if defined GL_RG16I
	case GL_RG16I: return StrCRef("RG16I");
#endif
#if defined GL_RG8I
	case GL_RG8I: return StrCRef("RG8I");
#endif
#if defined GL_R32I
	case GL_R32I: return StrCRef("R32I");
#endif
#if defined GL_R16I
	case GL_R16I: return StrCRef("R16I");
#endif
#if defined GL_R8I
	case GL_R8I: return StrCRef("R8I");
#endif
#if defined GL_RGBA16
	case GL_RGBA16: return StrCRef("RGBA16");
#endif
#if defined GL_RGB10_A2
	case GL_RGB10_A2: return StrCRef("RGB10_A2");
#endif
#if defined GL_RGBA8
	case GL_RGBA8: return StrCRef("RGBA8");
#endif
#if defined GL_RG16
	case GL_RG16: return StrCRef("RG16");
#endif
#if defined GL_RG8
	case GL_RG8: return StrCRef("RG8");
#endif
#if defined GL_R16
	case GL_R16: return StrCRef("R16");
#endif
#if defined GL_R8
	case GL_R8: return StrCRef("R8");
#endif
#if defined GL_RGBA16_SNORM
	case GL_RGBA16_SNORM: return StrCRef("RGBA16_SNORM");
#endif
#if defined GL_RGBA8_SNORM
	case GL_RGBA8_SNORM: return StrCRef("RGBA8_SNORM");
#endif
#if defined GL_RG16_SNORM
	case GL_RG16_SNORM: return StrCRef("RG16_SNORM");
#endif
#if defined GL_RG8_SNORM
	case GL_RG8_SNORM: return StrCRef("RG8_SNORM");
#endif
#if defined GL_R16_SNORM
	case GL_R16_SNORM: return StrCRef("R16_SNORM");
#endif
#if defined GL_R8_SNORM
	case GL_R8_SNORM: return StrCRef("R8_SNORM");
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


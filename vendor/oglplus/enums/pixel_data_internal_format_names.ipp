//  File implement/oglplus/enums/pixel_data_internal_format_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/pixel_data_internal_format.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	PixelDataInternalFormat*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PIXELDATAINTERNALFORMAT)
#define OGLPLUS_IMPL_EVN_PIXELDATAINTERNALFORMAT
{
switch(value)
{
#if defined GL_DEPTH_COMPONENT
	case GL_DEPTH_COMPONENT: return StrCRef("DEPTH_COMPONENT");
#endif
#if defined GL_DEPTH_STENCIL
	case GL_DEPTH_STENCIL: return StrCRef("DEPTH_STENCIL");
#endif
#if defined GL_STENCIL_INDEX8
	case GL_STENCIL_INDEX8: return StrCRef("STENCIL_INDEX8");
#endif
#if defined GL_RED
	case GL_RED: return StrCRef("RED");
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
#if defined GL_R8
	case GL_R8: return StrCRef("R8");
#endif
#if defined GL_R8_SNORM
	case GL_R8_SNORM: return StrCRef("R8_SNORM");
#endif
#if defined GL_R16
	case GL_R16: return StrCRef("R16");
#endif
#if defined GL_R16_SNORM
	case GL_R16_SNORM: return StrCRef("R16_SNORM");
#endif
#if defined GL_RG8
	case GL_RG8: return StrCRef("RG8");
#endif
#if defined GL_RG8_SNORM
	case GL_RG8_SNORM: return StrCRef("RG8_SNORM");
#endif
#if defined GL_RG16
	case GL_RG16: return StrCRef("RG16");
#endif
#if defined GL_RG16_SNORM
	case GL_RG16_SNORM: return StrCRef("RG16_SNORM");
#endif
#if defined GL_R3_G3_B2
	case GL_R3_G3_B2: return StrCRef("R3_G3_B2");
#endif
#if defined GL_RGB4
	case GL_RGB4: return StrCRef("RGB4");
#endif
#if defined GL_RGB5
	case GL_RGB5: return StrCRef("RGB5");
#endif
#if defined GL_RGB8
	case GL_RGB8: return StrCRef("RGB8");
#endif
#if defined GL_RGB8_SNORM
	case GL_RGB8_SNORM: return StrCRef("RGB8_SNORM");
#endif
#if defined GL_RGB10
	case GL_RGB10: return StrCRef("RGB10");
#endif
#if defined GL_RGB12
	case GL_RGB12: return StrCRef("RGB12");
#endif
#if defined GL_RGB16
	case GL_RGB16: return StrCRef("RGB16");
#endif
#if defined GL_RGB16_SNORM
	case GL_RGB16_SNORM: return StrCRef("RGB16_SNORM");
#endif
#if defined GL_RGBA2
	case GL_RGBA2: return StrCRef("RGBA2");
#endif
#if defined GL_RGBA4
	case GL_RGBA4: return StrCRef("RGBA4");
#endif
#if defined GL_RGB5_A1
	case GL_RGB5_A1: return StrCRef("RGB5_A1");
#endif
#if defined GL_RGBA8
	case GL_RGBA8: return StrCRef("RGBA8");
#endif
#if defined GL_RGBA8_SNORM
	case GL_RGBA8_SNORM: return StrCRef("RGBA8_SNORM");
#endif
#if defined GL_RGB10_A2
	case GL_RGB10_A2: return StrCRef("RGB10_A2");
#endif
#if defined GL_RGB10_A2UI
	case GL_RGB10_A2UI: return StrCRef("RGB10_A2UI");
#endif
#if defined GL_RGBA12
	case GL_RGBA12: return StrCRef("RGBA12");
#endif
#if defined GL_RGBA16
	case GL_RGBA16: return StrCRef("RGBA16");
#endif
#if defined GL_RGBA16_SNORM
	case GL_RGBA16_SNORM: return StrCRef("RGBA16_SNORM");
#endif
#if defined GL_SRGB8
	case GL_SRGB8: return StrCRef("SRGB8");
#endif
#if defined GL_SRGB8_ALPHA8
	case GL_SRGB8_ALPHA8: return StrCRef("SRGB8_ALPHA8");
#endif
#if defined GL_R16F
	case GL_R16F: return StrCRef("R16F");
#endif
#if defined GL_RG16F
	case GL_RG16F: return StrCRef("RG16F");
#endif
#if defined GL_RGB16F
	case GL_RGB16F: return StrCRef("RGB16F");
#endif
#if defined GL_RGBA16F
	case GL_RGBA16F: return StrCRef("RGBA16F");
#endif
#if defined GL_R32F
	case GL_R32F: return StrCRef("R32F");
#endif
#if defined GL_RG32F
	case GL_RG32F: return StrCRef("RG32F");
#endif
#if defined GL_RGB32F
	case GL_RGB32F: return StrCRef("RGB32F");
#endif
#if defined GL_RGBA32F
	case GL_RGBA32F: return StrCRef("RGBA32F");
#endif
#if defined GL_R11F_G11F_B10F
	case GL_R11F_G11F_B10F: return StrCRef("R11F_G11F_B10F");
#endif
#if defined GL_RGB9_E5
	case GL_RGB9_E5: return StrCRef("RGB9_E5");
#endif
#if defined GL_R8I
	case GL_R8I: return StrCRef("R8I");
#endif
#if defined GL_R8UI
	case GL_R8UI: return StrCRef("R8UI");
#endif
#if defined GL_R16I
	case GL_R16I: return StrCRef("R16I");
#endif
#if defined GL_R16UI
	case GL_R16UI: return StrCRef("R16UI");
#endif
#if defined GL_R32I
	case GL_R32I: return StrCRef("R32I");
#endif
#if defined GL_R32UI
	case GL_R32UI: return StrCRef("R32UI");
#endif
#if defined GL_RG8I
	case GL_RG8I: return StrCRef("RG8I");
#endif
#if defined GL_RG8UI
	case GL_RG8UI: return StrCRef("RG8UI");
#endif
#if defined GL_RG16I
	case GL_RG16I: return StrCRef("RG16I");
#endif
#if defined GL_RG16UI
	case GL_RG16UI: return StrCRef("RG16UI");
#endif
#if defined GL_RG32I
	case GL_RG32I: return StrCRef("RG32I");
#endif
#if defined GL_RG32UI
	case GL_RG32UI: return StrCRef("RG32UI");
#endif
#if defined GL_RGB8I
	case GL_RGB8I: return StrCRef("RGB8I");
#endif
#if defined GL_RGB8UI
	case GL_RGB8UI: return StrCRef("RGB8UI");
#endif
#if defined GL_RGB16I
	case GL_RGB16I: return StrCRef("RGB16I");
#endif
#if defined GL_RGB16UI
	case GL_RGB16UI: return StrCRef("RGB16UI");
#endif
#if defined GL_RGB32I
	case GL_RGB32I: return StrCRef("RGB32I");
#endif
#if defined GL_RGB32UI
	case GL_RGB32UI: return StrCRef("RGB32UI");
#endif
#if defined GL_RGBA8I
	case GL_RGBA8I: return StrCRef("RGBA8I");
#endif
#if defined GL_RGBA8UI
	case GL_RGBA8UI: return StrCRef("RGBA8UI");
#endif
#if defined GL_RGBA16I
	case GL_RGBA16I: return StrCRef("RGBA16I");
#endif
#if defined GL_RGBA16UI
	case GL_RGBA16UI: return StrCRef("RGBA16UI");
#endif
#if defined GL_RGBA32I
	case GL_RGBA32I: return StrCRef("RGBA32I");
#endif
#if defined GL_RGBA32UI
	case GL_RGBA32UI: return StrCRef("RGBA32UI");
#endif
#if defined GL_DEPTH_COMPONENT16
	case GL_DEPTH_COMPONENT16: return StrCRef("DEPTH_COMPONENT16");
#endif
#if defined GL_DEPTH_COMPONENT24
	case GL_DEPTH_COMPONENT24: return StrCRef("DEPTH_COMPONENT24");
#endif
#if defined GL_DEPTH_COMPONENT32
	case GL_DEPTH_COMPONENT32: return StrCRef("DEPTH_COMPONENT32");
#endif
#if defined GL_DEPTH_COMPONENT32F
	case GL_DEPTH_COMPONENT32F: return StrCRef("DEPTH_COMPONENT32F");
#endif
#if defined GL_DEPTH24_STENCIL8
	case GL_DEPTH24_STENCIL8: return StrCRef("DEPTH24_STENCIL8");
#endif
#if defined GL_DEPTH32F_STENCIL8
	case GL_DEPTH32F_STENCIL8: return StrCRef("DEPTH32F_STENCIL8");
#endif
#if defined GL_COMPRESSED_RED
	case GL_COMPRESSED_RED: return StrCRef("COMPRESSED_RED");
#endif
#if defined GL_COMPRESSED_RG
	case GL_COMPRESSED_RG: return StrCRef("COMPRESSED_RG");
#endif
#if defined GL_COMPRESSED_RGB
	case GL_COMPRESSED_RGB: return StrCRef("COMPRESSED_RGB");
#endif
#if defined GL_COMPRESSED_RGBA
	case GL_COMPRESSED_RGBA: return StrCRef("COMPRESSED_RGBA");
#endif
#if defined GL_COMPRESSED_SRGB
	case GL_COMPRESSED_SRGB: return StrCRef("COMPRESSED_SRGB");
#endif
#if defined GL_COMPRESSED_SRGB_ALPHA
	case GL_COMPRESSED_SRGB_ALPHA: return StrCRef("COMPRESSED_SRGB_ALPHA");
#endif
#if defined GL_COMPRESSED_RED_RGTC1
	case GL_COMPRESSED_RED_RGTC1: return StrCRef("COMPRESSED_RED_RGTC1");
#endif
#if defined GL_COMPRESSED_SIGNED_RED_RGTC1
	case GL_COMPRESSED_SIGNED_RED_RGTC1: return StrCRef("COMPRESSED_SIGNED_RED_RGTC1");
#endif
#if defined GL_COMPRESSED_RG_RGTC2
	case GL_COMPRESSED_RG_RGTC2: return StrCRef("COMPRESSED_RG_RGTC2");
#endif
#if defined GL_COMPRESSED_SIGNED_RG_RGTC2
	case GL_COMPRESSED_SIGNED_RG_RGTC2: return StrCRef("COMPRESSED_SIGNED_RG_RGTC2");
#endif
#if defined GL_COMPRESSED_RGBA_BPTC_UNORM
	case GL_COMPRESSED_RGBA_BPTC_UNORM: return StrCRef("COMPRESSED_RGBA_BPTC_UNORM");
#endif
#if defined GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM
	case GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM: return StrCRef("COMPRESSED_SRGB_ALPHA_BPTC_UNORM");
#endif
#if defined GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT
	case GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT: return StrCRef("COMPRESSED_RGB_BPTC_SIGNED_FLOAT");
#endif
#if defined GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT
	case GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT: return StrCRef("COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT");
#endif
#if defined GL_COMPRESSED_RGB8_ETC2
	case GL_COMPRESSED_RGB8_ETC2: return StrCRef("COMPRESSED_RGB8_ETC2");
#endif
#if defined GL_COMPRESSED_SRGB8_ETC2
	case GL_COMPRESSED_SRGB8_ETC2: return StrCRef("COMPRESSED_SRGB8_ETC2");
#endif
#if defined GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2
	case GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2: return StrCRef("COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2");
#endif
#if defined GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2
	case GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2: return StrCRef("COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2");
#endif
#if defined GL_COMPRESSED_RGBA8_ETC2_EAC
	case GL_COMPRESSED_RGBA8_ETC2_EAC: return StrCRef("COMPRESSED_RGBA8_ETC2_EAC");
#endif
#if defined GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC
	case GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC: return StrCRef("COMPRESSED_SRGB8_ALPHA8_ETC2_EAC");
#endif
#if defined GL_COMPRESSED_R11_EAC
	case GL_COMPRESSED_R11_EAC: return StrCRef("COMPRESSED_R11_EAC");
#endif
#if defined GL_COMPRESSED_SIGNED_R11_EAC
	case GL_COMPRESSED_SIGNED_R11_EAC: return StrCRef("COMPRESSED_SIGNED_R11_EAC");
#endif
#if defined GL_COMPRESSED_RG11_EAC
	case GL_COMPRESSED_RG11_EAC: return StrCRef("COMPRESSED_RG11_EAC");
#endif
#if defined GL_COMPRESSED_SIGNED_RG11_EAC
	case GL_COMPRESSED_SIGNED_RG11_EAC: return StrCRef("COMPRESSED_SIGNED_RG11_EAC");
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


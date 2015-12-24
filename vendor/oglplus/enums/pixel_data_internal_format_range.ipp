//  File implement/oglplus/enums/pixel_data_internal_format_range.ipp
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
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLenum*,
	PixelDataInternalFormat
> ValueRange_(PixelDataInternalFormat*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_PIXELDATAINTERNALFORMAT)
#define OGLPLUS_IMPL_EVR_PIXELDATAINTERNALFORMAT
{
static const GLenum _values[] = {
#if defined GL_DEPTH_COMPONENT
GL_DEPTH_COMPONENT,
#endif
#if defined GL_DEPTH_STENCIL
GL_DEPTH_STENCIL,
#endif
#if defined GL_STENCIL_INDEX8
GL_STENCIL_INDEX8,
#endif
#if defined GL_RED
GL_RED,
#endif
#if defined GL_RG
GL_RG,
#endif
#if defined GL_RGB
GL_RGB,
#endif
#if defined GL_RGBA
GL_RGBA,
#endif
#if defined GL_R8
GL_R8,
#endif
#if defined GL_R8_SNORM
GL_R8_SNORM,
#endif
#if defined GL_R16
GL_R16,
#endif
#if defined GL_R16_SNORM
GL_R16_SNORM,
#endif
#if defined GL_RG8
GL_RG8,
#endif
#if defined GL_RG8_SNORM
GL_RG8_SNORM,
#endif
#if defined GL_RG16
GL_RG16,
#endif
#if defined GL_RG16_SNORM
GL_RG16_SNORM,
#endif
#if defined GL_R3_G3_B2
GL_R3_G3_B2,
#endif
#if defined GL_RGB4
GL_RGB4,
#endif
#if defined GL_RGB5
GL_RGB5,
#endif
#if defined GL_RGB8
GL_RGB8,
#endif
#if defined GL_RGB8_SNORM
GL_RGB8_SNORM,
#endif
#if defined GL_RGB10
GL_RGB10,
#endif
#if defined GL_RGB12
GL_RGB12,
#endif
#if defined GL_RGB16
GL_RGB16,
#endif
#if defined GL_RGB16_SNORM
GL_RGB16_SNORM,
#endif
#if defined GL_RGBA2
GL_RGBA2,
#endif
#if defined GL_RGBA4
GL_RGBA4,
#endif
#if defined GL_RGB5_A1
GL_RGB5_A1,
#endif
#if defined GL_RGBA8
GL_RGBA8,
#endif
#if defined GL_RGBA8_SNORM
GL_RGBA8_SNORM,
#endif
#if defined GL_RGB10_A2
GL_RGB10_A2,
#endif
#if defined GL_RGB10_A2UI
GL_RGB10_A2UI,
#endif
#if defined GL_RGBA12
GL_RGBA12,
#endif
#if defined GL_RGBA16
GL_RGBA16,
#endif
#if defined GL_RGBA16_SNORM
GL_RGBA16_SNORM,
#endif
#if defined GL_SRGB8
GL_SRGB8,
#endif
#if defined GL_SRGB8_ALPHA8
GL_SRGB8_ALPHA8,
#endif
#if defined GL_R16F
GL_R16F,
#endif
#if defined GL_RG16F
GL_RG16F,
#endif
#if defined GL_RGB16F
GL_RGB16F,
#endif
#if defined GL_RGBA16F
GL_RGBA16F,
#endif
#if defined GL_R32F
GL_R32F,
#endif
#if defined GL_RG32F
GL_RG32F,
#endif
#if defined GL_RGB32F
GL_RGB32F,
#endif
#if defined GL_RGBA32F
GL_RGBA32F,
#endif
#if defined GL_R11F_G11F_B10F
GL_R11F_G11F_B10F,
#endif
#if defined GL_RGB9_E5
GL_RGB9_E5,
#endif
#if defined GL_R8I
GL_R8I,
#endif
#if defined GL_R8UI
GL_R8UI,
#endif
#if defined GL_R16I
GL_R16I,
#endif
#if defined GL_R16UI
GL_R16UI,
#endif
#if defined GL_R32I
GL_R32I,
#endif
#if defined GL_R32UI
GL_R32UI,
#endif
#if defined GL_RG8I
GL_RG8I,
#endif
#if defined GL_RG8UI
GL_RG8UI,
#endif
#if defined GL_RG16I
GL_RG16I,
#endif
#if defined GL_RG16UI
GL_RG16UI,
#endif
#if defined GL_RG32I
GL_RG32I,
#endif
#if defined GL_RG32UI
GL_RG32UI,
#endif
#if defined GL_RGB8I
GL_RGB8I,
#endif
#if defined GL_RGB8UI
GL_RGB8UI,
#endif
#if defined GL_RGB16I
GL_RGB16I,
#endif
#if defined GL_RGB16UI
GL_RGB16UI,
#endif
#if defined GL_RGB32I
GL_RGB32I,
#endif
#if defined GL_RGB32UI
GL_RGB32UI,
#endif
#if defined GL_RGBA8I
GL_RGBA8I,
#endif
#if defined GL_RGBA8UI
GL_RGBA8UI,
#endif
#if defined GL_RGBA16I
GL_RGBA16I,
#endif
#if defined GL_RGBA16UI
GL_RGBA16UI,
#endif
#if defined GL_RGBA32I
GL_RGBA32I,
#endif
#if defined GL_RGBA32UI
GL_RGBA32UI,
#endif
#if defined GL_DEPTH_COMPONENT16
GL_DEPTH_COMPONENT16,
#endif
#if defined GL_DEPTH_COMPONENT24
GL_DEPTH_COMPONENT24,
#endif
#if defined GL_DEPTH_COMPONENT32
GL_DEPTH_COMPONENT32,
#endif
#if defined GL_DEPTH_COMPONENT32F
GL_DEPTH_COMPONENT32F,
#endif
#if defined GL_DEPTH24_STENCIL8
GL_DEPTH24_STENCIL8,
#endif
#if defined GL_DEPTH32F_STENCIL8
GL_DEPTH32F_STENCIL8,
#endif
#if defined GL_COMPRESSED_RED
GL_COMPRESSED_RED,
#endif
#if defined GL_COMPRESSED_RG
GL_COMPRESSED_RG,
#endif
#if defined GL_COMPRESSED_RGB
GL_COMPRESSED_RGB,
#endif
#if defined GL_COMPRESSED_RGBA
GL_COMPRESSED_RGBA,
#endif
#if defined GL_COMPRESSED_SRGB
GL_COMPRESSED_SRGB,
#endif
#if defined GL_COMPRESSED_SRGB_ALPHA
GL_COMPRESSED_SRGB_ALPHA,
#endif
#if defined GL_COMPRESSED_RED_RGTC1
GL_COMPRESSED_RED_RGTC1,
#endif
#if defined GL_COMPRESSED_SIGNED_RED_RGTC1
GL_COMPRESSED_SIGNED_RED_RGTC1,
#endif
#if defined GL_COMPRESSED_RG_RGTC2
GL_COMPRESSED_RG_RGTC2,
#endif
#if defined GL_COMPRESSED_SIGNED_RG_RGTC2
GL_COMPRESSED_SIGNED_RG_RGTC2,
#endif
#if defined GL_COMPRESSED_RGBA_BPTC_UNORM
GL_COMPRESSED_RGBA_BPTC_UNORM,
#endif
#if defined GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM
GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM,
#endif
#if defined GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT
GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT,
#endif
#if defined GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT
GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT,
#endif
#if defined GL_COMPRESSED_RGB8_ETC2
GL_COMPRESSED_RGB8_ETC2,
#endif
#if defined GL_COMPRESSED_SRGB8_ETC2
GL_COMPRESSED_SRGB8_ETC2,
#endif
#if defined GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2
GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2,
#endif
#if defined GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2
GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2,
#endif
#if defined GL_COMPRESSED_RGBA8_ETC2_EAC
GL_COMPRESSED_RGBA8_ETC2_EAC,
#endif
#if defined GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC
GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC,
#endif
#if defined GL_COMPRESSED_R11_EAC
GL_COMPRESSED_R11_EAC,
#endif
#if defined GL_COMPRESSED_SIGNED_R11_EAC
GL_COMPRESSED_SIGNED_R11_EAC,
#endif
#if defined GL_COMPRESSED_RG11_EAC
GL_COMPRESSED_RG11_EAC,
#endif
#if defined GL_COMPRESSED_SIGNED_RG11_EAC
GL_COMPRESSED_SIGNED_RG11_EAC,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	PixelDataInternalFormat
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


//  File implement/oglplus/enums/image_unit_format_range.ipp
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
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLenum*,
	ImageUnitFormat
> ValueRange_(ImageUnitFormat*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_IMAGEUNITFORMAT)
#define OGLPLUS_IMPL_EVR_IMAGEUNITFORMAT
{
static const GLenum _values[] = {
#if defined GL_RGBA32F
GL_RGBA32F,
#endif
#if defined GL_RGBA16F
GL_RGBA16F,
#endif
#if defined GL_RG32F
GL_RG32F,
#endif
#if defined GL_RG16F
GL_RG16F,
#endif
#if defined GL_R11F_G11F_B10F
GL_R11F_G11F_B10F,
#endif
#if defined GL_R32F
GL_R32F,
#endif
#if defined GL_R16F
GL_R16F,
#endif
#if defined GL_RGBA32UI
GL_RGBA32UI,
#endif
#if defined GL_RGBA16UI
GL_RGBA16UI,
#endif
#if defined GL_RGB10_A2UI
GL_RGB10_A2UI,
#endif
#if defined GL_RGBA8UI
GL_RGBA8UI,
#endif
#if defined GL_RG32UI
GL_RG32UI,
#endif
#if defined GL_RG16UI
GL_RG16UI,
#endif
#if defined GL_RG8UI
GL_RG8UI,
#endif
#if defined GL_R32UI
GL_R32UI,
#endif
#if defined GL_R16UI
GL_R16UI,
#endif
#if defined GL_R8UI
GL_R8UI,
#endif
#if defined GL_RGBA32I
GL_RGBA32I,
#endif
#if defined GL_RGBA16I
GL_RGBA16I,
#endif
#if defined GL_RGBA8I
GL_RGBA8I,
#endif
#if defined GL_RG32I
GL_RG32I,
#endif
#if defined GL_RG16I
GL_RG16I,
#endif
#if defined GL_RG8I
GL_RG8I,
#endif
#if defined GL_R32I
GL_R32I,
#endif
#if defined GL_R16I
GL_R16I,
#endif
#if defined GL_R8I
GL_R8I,
#endif
#if defined GL_RGBA16
GL_RGBA16,
#endif
#if defined GL_RGB10_A2
GL_RGB10_A2,
#endif
#if defined GL_RGBA8
GL_RGBA8,
#endif
#if defined GL_RG16
GL_RG16,
#endif
#if defined GL_RG8
GL_RG8,
#endif
#if defined GL_R16
GL_R16,
#endif
#if defined GL_R8
GL_R8,
#endif
#if defined GL_RGBA16_SNORM
GL_RGBA16_SNORM,
#endif
#if defined GL_RGBA8_SNORM
GL_RGBA8_SNORM,
#endif
#if defined GL_RG16_SNORM
GL_RG16_SNORM,
#endif
#if defined GL_RG8_SNORM
GL_RG8_SNORM,
#endif
#if defined GL_R16_SNORM
GL_R16_SNORM,
#endif
#if defined GL_R8_SNORM
GL_R8_SNORM,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	ImageUnitFormat
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


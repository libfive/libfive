//  File implement/oglplus/enums/pixel_data_type_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/pixel_data_type.txt'
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
	PixelDataType
> ValueRange_(PixelDataType*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_PIXELDATATYPE)
#define OGLPLUS_IMPL_EVR_PIXELDATATYPE
{
static const GLenum _values[] = {
#if defined GL_UNSIGNED_BYTE
GL_UNSIGNED_BYTE,
#endif
#if defined GL_BYTE
GL_BYTE,
#endif
#if defined GL_UNSIGNED_SHORT
GL_UNSIGNED_SHORT,
#endif
#if defined GL_SHORT
GL_SHORT,
#endif
#if defined GL_UNSIGNED_INT
GL_UNSIGNED_INT,
#endif
#if defined GL_INT
GL_INT,
#endif
#if defined GL_HALF_FLOAT
GL_HALF_FLOAT,
#endif
#if defined GL_FLOAT
GL_FLOAT,
#endif
#if defined GL_UNSIGNED_BYTE_3_3_2
GL_UNSIGNED_BYTE_3_3_2,
#endif
#if defined GL_UNSIGNED_BYTE_2_3_3_REV
GL_UNSIGNED_BYTE_2_3_3_REV,
#endif
#if defined GL_UNSIGNED_SHORT_5_6_5
GL_UNSIGNED_SHORT_5_6_5,
#endif
#if defined GL_UNSIGNED_SHORT_5_6_5_REV
GL_UNSIGNED_SHORT_5_6_5_REV,
#endif
#if defined GL_UNSIGNED_SHORT_4_4_4_4
GL_UNSIGNED_SHORT_4_4_4_4,
#endif
#if defined GL_UNSIGNED_SHORT_4_4_4_4_REV
GL_UNSIGNED_SHORT_4_4_4_4_REV,
#endif
#if defined GL_UNSIGNED_SHORT_5_5_5_1
GL_UNSIGNED_SHORT_5_5_5_1,
#endif
#if defined GL_UNSIGNED_SHORT_1_5_5_5_REV
GL_UNSIGNED_SHORT_1_5_5_5_REV,
#endif
#if defined GL_UNSIGNED_INT_8_8_8_8
GL_UNSIGNED_INT_8_8_8_8,
#endif
#if defined GL_UNSIGNED_INT_8_8_8_8_REV
GL_UNSIGNED_INT_8_8_8_8_REV,
#endif
#if defined GL_UNSIGNED_INT_10_10_10_2
GL_UNSIGNED_INT_10_10_10_2,
#endif
#if defined GL_UNSIGNED_INT_2_10_10_10_REV
GL_UNSIGNED_INT_2_10_10_10_REV,
#endif
#if defined GL_UNSIGNED_INT_24_8
GL_UNSIGNED_INT_24_8,
#endif
#if defined GL_UNSIGNED_INT_10F_11F_11F_REV
GL_UNSIGNED_INT_10F_11F_11F_REV,
#endif
#if defined GL_UNSIGNED_INT_5_9_9_9_REV
GL_UNSIGNED_INT_5_9_9_9_REV,
#endif
#if defined GL_FLOAT_32_UNSIGNED_INT_24_8_REV
GL_FLOAT_32_UNSIGNED_INT_24_8_REV,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	PixelDataType
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


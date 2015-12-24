//  File implement/oglplus/enums/pixel_data_type_names.ipp
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
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	PixelDataType*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PIXELDATATYPE)
#define OGLPLUS_IMPL_EVN_PIXELDATATYPE
{
switch(value)
{
#if defined GL_UNSIGNED_BYTE
	case GL_UNSIGNED_BYTE: return StrCRef("UNSIGNED_BYTE");
#endif
#if defined GL_BYTE
	case GL_BYTE: return StrCRef("BYTE");
#endif
#if defined GL_UNSIGNED_SHORT
	case GL_UNSIGNED_SHORT: return StrCRef("UNSIGNED_SHORT");
#endif
#if defined GL_SHORT
	case GL_SHORT: return StrCRef("SHORT");
#endif
#if defined GL_UNSIGNED_INT
	case GL_UNSIGNED_INT: return StrCRef("UNSIGNED_INT");
#endif
#if defined GL_INT
	case GL_INT: return StrCRef("INT");
#endif
#if defined GL_HALF_FLOAT
	case GL_HALF_FLOAT: return StrCRef("HALF_FLOAT");
#endif
#if defined GL_FLOAT
	case GL_FLOAT: return StrCRef("FLOAT");
#endif
#if defined GL_UNSIGNED_BYTE_3_3_2
	case GL_UNSIGNED_BYTE_3_3_2: return StrCRef("UNSIGNED_BYTE_3_3_2");
#endif
#if defined GL_UNSIGNED_BYTE_2_3_3_REV
	case GL_UNSIGNED_BYTE_2_3_3_REV: return StrCRef("UNSIGNED_BYTE_2_3_3_REV");
#endif
#if defined GL_UNSIGNED_SHORT_5_6_5
	case GL_UNSIGNED_SHORT_5_6_5: return StrCRef("UNSIGNED_SHORT_5_6_5");
#endif
#if defined GL_UNSIGNED_SHORT_5_6_5_REV
	case GL_UNSIGNED_SHORT_5_6_5_REV: return StrCRef("UNSIGNED_SHORT_5_6_5_REV");
#endif
#if defined GL_UNSIGNED_SHORT_4_4_4_4
	case GL_UNSIGNED_SHORT_4_4_4_4: return StrCRef("UNSIGNED_SHORT_4_4_4_4");
#endif
#if defined GL_UNSIGNED_SHORT_4_4_4_4_REV
	case GL_UNSIGNED_SHORT_4_4_4_4_REV: return StrCRef("UNSIGNED_SHORT_4_4_4_4_REV");
#endif
#if defined GL_UNSIGNED_SHORT_5_5_5_1
	case GL_UNSIGNED_SHORT_5_5_5_1: return StrCRef("UNSIGNED_SHORT_5_5_5_1");
#endif
#if defined GL_UNSIGNED_SHORT_1_5_5_5_REV
	case GL_UNSIGNED_SHORT_1_5_5_5_REV: return StrCRef("UNSIGNED_SHORT_1_5_5_5_REV");
#endif
#if defined GL_UNSIGNED_INT_8_8_8_8
	case GL_UNSIGNED_INT_8_8_8_8: return StrCRef("UNSIGNED_INT_8_8_8_8");
#endif
#if defined GL_UNSIGNED_INT_8_8_8_8_REV
	case GL_UNSIGNED_INT_8_8_8_8_REV: return StrCRef("UNSIGNED_INT_8_8_8_8_REV");
#endif
#if defined GL_UNSIGNED_INT_10_10_10_2
	case GL_UNSIGNED_INT_10_10_10_2: return StrCRef("UNSIGNED_INT_10_10_10_2");
#endif
#if defined GL_UNSIGNED_INT_2_10_10_10_REV
	case GL_UNSIGNED_INT_2_10_10_10_REV: return StrCRef("UNSIGNED_INT_2_10_10_10_REV");
#endif
#if defined GL_UNSIGNED_INT_24_8
	case GL_UNSIGNED_INT_24_8: return StrCRef("UNSIGNED_INT_24_8");
#endif
#if defined GL_UNSIGNED_INT_10F_11F_11F_REV
	case GL_UNSIGNED_INT_10F_11F_11F_REV: return StrCRef("UNSIGNED_INT_10F_11F_11F_REV");
#endif
#if defined GL_UNSIGNED_INT_5_9_9_9_REV
	case GL_UNSIGNED_INT_5_9_9_9_REV: return StrCRef("UNSIGNED_INT_5_9_9_9_REV");
#endif
#if defined GL_FLOAT_32_UNSIGNED_INT_24_8_REV
	case GL_FLOAT_32_UNSIGNED_INT_24_8_REV: return StrCRef("FLOAT_32_UNSIGNED_INT_24_8_REV");
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


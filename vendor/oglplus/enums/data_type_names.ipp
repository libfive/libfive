//  File implement/oglplus/enums/data_type_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/data_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	DataType*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_DATATYPE)
#define OGLPLUS_IMPL_EVN_DATATYPE
{
switch(value)
{
#if defined GL_BYTE
	case GL_BYTE: return StrCRef("BYTE");
#endif
#if defined GL_SHORT
	case GL_SHORT: return StrCRef("SHORT");
#endif
#if defined GL_INT
	case GL_INT: return StrCRef("INT");
#endif
#if defined GL_FIXED
	case GL_FIXED: return StrCRef("FIXED");
#endif
#if defined GL_FLOAT
	case GL_FLOAT: return StrCRef("FLOAT");
#endif
#if defined GL_HALF_FLOAT
	case GL_HALF_FLOAT: return StrCRef("HALF_FLOAT");
#endif
#if defined GL_DOUBLE
	case GL_DOUBLE: return StrCRef("DOUBLE");
#endif
#if defined GL_UNSIGNED_BYTE
	case GL_UNSIGNED_BYTE: return StrCRef("UNSIGNED_BYTE");
#endif
#if defined GL_UNSIGNED_SHORT
	case GL_UNSIGNED_SHORT: return StrCRef("UNSIGNED_SHORT");
#endif
#if defined GL_UNSIGNED_INT
	case GL_UNSIGNED_INT: return StrCRef("UNSIGNED_INT");
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


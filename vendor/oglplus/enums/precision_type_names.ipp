//  File implement/oglplus/enums/precision_type_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/precision_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	PrecisionType*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PRECISIONTYPE)
#define OGLPLUS_IMPL_EVN_PRECISIONTYPE
{
switch(value)
{
#if defined GL_LOW_FLOAT
	case GL_LOW_FLOAT: return StrCRef("LOW_FLOAT");
#endif
#if defined GL_MEDIUM_FLOAT
	case GL_MEDIUM_FLOAT: return StrCRef("MEDIUM_FLOAT");
#endif
#if defined GL_HIGH_FLOAT
	case GL_HIGH_FLOAT: return StrCRef("HIGH_FLOAT");
#endif
#if defined GL_LOW_INT
	case GL_LOW_INT: return StrCRef("LOW_INT");
#endif
#if defined GL_MEDIUM_INT
	case GL_MEDIUM_INT: return StrCRef("MEDIUM_INT");
#endif
#if defined GL_HIGH_INT
	case GL_HIGH_INT: return StrCRef("HIGH_INT");
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


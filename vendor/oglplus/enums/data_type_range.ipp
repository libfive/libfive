//  File implement/oglplus/enums/data_type_range.ipp
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
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLenum*,
	DataType
> ValueRange_(DataType*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_DATATYPE)
#define OGLPLUS_IMPL_EVR_DATATYPE
{
static const GLenum _values[] = {
#if defined GL_BYTE
GL_BYTE,
#endif
#if defined GL_SHORT
GL_SHORT,
#endif
#if defined GL_INT
GL_INT,
#endif
#if defined GL_FIXED
GL_FIXED,
#endif
#if defined GL_FLOAT
GL_FLOAT,
#endif
#if defined GL_HALF_FLOAT
GL_HALF_FLOAT,
#endif
#if defined GL_DOUBLE
GL_DOUBLE,
#endif
#if defined GL_UNSIGNED_BYTE
GL_UNSIGNED_BYTE,
#endif
#if defined GL_UNSIGNED_SHORT
GL_UNSIGNED_SHORT,
#endif
#if defined GL_UNSIGNED_INT
GL_UNSIGNED_INT,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	DataType
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


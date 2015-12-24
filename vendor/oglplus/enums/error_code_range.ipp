//  File implement/oglplus/enums/error_code_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/error_code.txt'
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
	ErrorCode
> ValueRange_(ErrorCode*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_ERRORCODE)
#define OGLPLUS_IMPL_EVR_ERRORCODE
{
static const GLenum _values[] = {
#if defined GL_NO_ERROR
GL_NO_ERROR,
#endif
#if defined GL_OUT_OF_MEMORY
GL_OUT_OF_MEMORY,
#endif
#if defined GL_INVALID_ENUM
GL_INVALID_ENUM,
#endif
#if defined GL_INVALID_VALUE
GL_INVALID_VALUE,
#endif
#if defined GL_INVALID_OPERATION
GL_INVALID_OPERATION,
#endif
#if defined GL_INVALID_FRAMEBUFFER_OPERATION
GL_INVALID_FRAMEBUFFER_OPERATION,
#endif
#if defined GL_STACK_OVERFLOW
GL_STACK_OVERFLOW,
#endif
#if defined GL_STACK_UNDERFLOW
GL_STACK_UNDERFLOW,
#endif
#if defined GL_TABLE_TOO_LARGE
GL_TABLE_TOO_LARGE,
#endif
#if defined GL_CONTEXT_LOST
GL_CONTEXT_LOST,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	ErrorCode
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


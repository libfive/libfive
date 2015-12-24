//  File implement/oglplus/enums/color_logic_operation_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/color_logic_operation.txt'
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
	ColorLogicOperation
> ValueRange_(ColorLogicOperation*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_COLORLOGICOPERATION)
#define OGLPLUS_IMPL_EVR_COLORLOGICOPERATION
{
static const GLenum _values[] = {
#if defined GL_CLEAR
GL_CLEAR,
#endif
#if defined GL_AND
GL_AND,
#endif
#if defined GL_AND_REVERSE
GL_AND_REVERSE,
#endif
#if defined GL_COPY
GL_COPY,
#endif
#if defined GL_AND_INVERTED
GL_AND_INVERTED,
#endif
#if defined GL_NOOP
GL_NOOP,
#endif
#if defined GL_XOR
GL_XOR,
#endif
#if defined GL_OR
GL_OR,
#endif
#if defined GL_NOR
GL_NOR,
#endif
#if defined GL_EQUIV
GL_EQUIV,
#endif
#if defined GL_INVERT
GL_INVERT,
#endif
#if defined GL_OR_REVERSE
GL_OR_REVERSE,
#endif
#if defined GL_COPY_INVERTED
GL_COPY_INVERTED,
#endif
#if defined GL_OR_INVERTED
GL_OR_INVERTED,
#endif
#if defined GL_NAND
GL_NAND,
#endif
#if defined GL_SET
GL_SET,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	ColorLogicOperation
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


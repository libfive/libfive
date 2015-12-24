//  File implement/oglplus/enums/context_flag_bit_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/context_flag_bit.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLbitfield*,
	ContextFlagBit
> ValueRange_(ContextFlagBit*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_CONTEXTFLAGBIT)
#define OGLPLUS_IMPL_EVR_CONTEXTFLAGBIT
{
static const GLbitfield _values[] = {
#if defined GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT
GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT,
#endif
#if defined GL_CONTEXT_FLAG_DEBUG_BIT
GL_CONTEXT_FLAG_DEBUG_BIT,
#endif
#if defined GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT_ARB
GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT_ARB,
#endif
0
};
return aux::CastIterRange<
	const GLbitfield*,
	ContextFlagBit
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


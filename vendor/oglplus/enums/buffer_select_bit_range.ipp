//  File implement/oglplus/enums/buffer_select_bit_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/buffer_select_bit.txt'
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
	BufferSelectBit
> ValueRange_(BufferSelectBit*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_BUFFERSELECTBIT)
#define OGLPLUS_IMPL_EVR_BUFFERSELECTBIT
{
static const GLbitfield _values[] = {
#if defined GL_COLOR_BUFFER_BIT
GL_COLOR_BUFFER_BIT,
#endif
#if defined GL_DEPTH_BUFFER_BIT
GL_DEPTH_BUFFER_BIT,
#endif
#if defined GL_STENCIL_BUFFER_BIT
GL_STENCIL_BUFFER_BIT,
#endif
0
};
return aux::CastIterRange<
	const GLbitfield*,
	BufferSelectBit
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


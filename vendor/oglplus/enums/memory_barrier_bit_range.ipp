//  File implement/oglplus/enums/memory_barrier_bit_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/memory_barrier_bit.txt'
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
	MemoryBarrierBit
> ValueRange_(MemoryBarrierBit*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_MEMORYBARRIERBIT)
#define OGLPLUS_IMPL_EVR_MEMORYBARRIERBIT
{
static const GLbitfield _values[] = {
#if defined GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT
GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT,
#endif
#if defined GL_ELEMENT_ARRAY_BARRIER_BIT
GL_ELEMENT_ARRAY_BARRIER_BIT,
#endif
#if defined GL_UNIFORM_BARRIER_BIT
GL_UNIFORM_BARRIER_BIT,
#endif
#if defined GL_TEXTURE_FETCH_BARRIER_BIT
GL_TEXTURE_FETCH_BARRIER_BIT,
#endif
#if defined GL_SHADER_IMAGE_ACCESS_BARRIER_BIT
GL_SHADER_IMAGE_ACCESS_BARRIER_BIT,
#endif
#if defined GL_COMMAND_BARRIER_BIT
GL_COMMAND_BARRIER_BIT,
#endif
#if defined GL_PIXEL_BUFFER_BARRIER_BIT
GL_PIXEL_BUFFER_BARRIER_BIT,
#endif
#if defined GL_TEXTURE_UPDATE_BARRIER_BIT
GL_TEXTURE_UPDATE_BARRIER_BIT,
#endif
#if defined GL_BUFFER_UPDATE_BARRIER_BIT
GL_BUFFER_UPDATE_BARRIER_BIT,
#endif
#if defined GL_FRAMEBUFFER_BARRIER_BIT
GL_FRAMEBUFFER_BARRIER_BIT,
#endif
#if defined GL_TRANSFORM_FEEDBACK_BARRIER_BIT
GL_TRANSFORM_FEEDBACK_BARRIER_BIT,
#endif
#if defined GL_ATOMIC_COUNTER_BARRIER_BIT
GL_ATOMIC_COUNTER_BARRIER_BIT,
#endif
#if defined GL_SHADER_STORAGE_BARRIER_BIT
GL_SHADER_STORAGE_BARRIER_BIT,
#endif
#if defined GL_CLIENT_MAPPED_BUFFER_BARRIER_BIT
GL_CLIENT_MAPPED_BUFFER_BARRIER_BIT,
#endif
#if defined GL_ALL_BARRIER_BITS
GL_ALL_BARRIER_BITS,
#endif
0
};
return aux::CastIterRange<
	const GLbitfield*,
	MemoryBarrierBit
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


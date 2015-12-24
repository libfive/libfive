//  File implement/oglplus/enums/memory_barrier_bit_names.ipp
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
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	MemoryBarrierBit*,
	GLbitfield value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_MEMORYBARRIERBIT)
#define OGLPLUS_IMPL_EVN_MEMORYBARRIERBIT
{
switch(value)
{
#if defined GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT
	case GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT: return StrCRef("VERTEX_ATTRIB_ARRAY_BARRIER_BIT");
#endif
#if defined GL_ELEMENT_ARRAY_BARRIER_BIT
	case GL_ELEMENT_ARRAY_BARRIER_BIT: return StrCRef("ELEMENT_ARRAY_BARRIER_BIT");
#endif
#if defined GL_UNIFORM_BARRIER_BIT
	case GL_UNIFORM_BARRIER_BIT: return StrCRef("UNIFORM_BARRIER_BIT");
#endif
#if defined GL_TEXTURE_FETCH_BARRIER_BIT
	case GL_TEXTURE_FETCH_BARRIER_BIT: return StrCRef("TEXTURE_FETCH_BARRIER_BIT");
#endif
#if defined GL_SHADER_IMAGE_ACCESS_BARRIER_BIT
	case GL_SHADER_IMAGE_ACCESS_BARRIER_BIT: return StrCRef("SHADER_IMAGE_ACCESS_BARRIER_BIT");
#endif
#if defined GL_COMMAND_BARRIER_BIT
	case GL_COMMAND_BARRIER_BIT: return StrCRef("COMMAND_BARRIER_BIT");
#endif
#if defined GL_PIXEL_BUFFER_BARRIER_BIT
	case GL_PIXEL_BUFFER_BARRIER_BIT: return StrCRef("PIXEL_BUFFER_BARRIER_BIT");
#endif
#if defined GL_TEXTURE_UPDATE_BARRIER_BIT
	case GL_TEXTURE_UPDATE_BARRIER_BIT: return StrCRef("TEXTURE_UPDATE_BARRIER_BIT");
#endif
#if defined GL_BUFFER_UPDATE_BARRIER_BIT
	case GL_BUFFER_UPDATE_BARRIER_BIT: return StrCRef("BUFFER_UPDATE_BARRIER_BIT");
#endif
#if defined GL_FRAMEBUFFER_BARRIER_BIT
	case GL_FRAMEBUFFER_BARRIER_BIT: return StrCRef("FRAMEBUFFER_BARRIER_BIT");
#endif
#if defined GL_TRANSFORM_FEEDBACK_BARRIER_BIT
	case GL_TRANSFORM_FEEDBACK_BARRIER_BIT: return StrCRef("TRANSFORM_FEEDBACK_BARRIER_BIT");
#endif
#if defined GL_ATOMIC_COUNTER_BARRIER_BIT
	case GL_ATOMIC_COUNTER_BARRIER_BIT: return StrCRef("ATOMIC_COUNTER_BARRIER_BIT");
#endif
#if defined GL_SHADER_STORAGE_BARRIER_BIT
	case GL_SHADER_STORAGE_BARRIER_BIT: return StrCRef("SHADER_STORAGE_BARRIER_BIT");
#endif
#if defined GL_CLIENT_MAPPED_BUFFER_BARRIER_BIT
	case GL_CLIENT_MAPPED_BUFFER_BARRIER_BIT: return StrCRef("CLIENT_MAPPED_BUFFER_BARRIER_BIT");
#endif
#if defined GL_ALL_BARRIER_BITS
	case GL_ALL_BARRIER_BITS: return StrCRef("ALL_BARRIER_BITS");
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


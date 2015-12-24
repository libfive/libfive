//  File implement/oglplus/enums/buffer_indexed_target_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/buffer_indexed_target.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	BufferIndexedTarget*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_BUFFERINDEXEDTARGET)
#define OGLPLUS_IMPL_EVN_BUFFERINDEXEDTARGET
{
switch(value)
{
#if defined GL_ATOMIC_COUNTER_BUFFER
	case GL_ATOMIC_COUNTER_BUFFER: return StrCRef("ATOMIC_COUNTER_BUFFER");
#endif
#if defined GL_SHADER_STORAGE_BUFFER
	case GL_SHADER_STORAGE_BUFFER: return StrCRef("SHADER_STORAGE_BUFFER");
#endif
#if defined GL_TRANSFORM_FEEDBACK_BUFFER
	case GL_TRANSFORM_FEEDBACK_BUFFER: return StrCRef("TRANSFORM_FEEDBACK_BUFFER");
#endif
#if defined GL_UNIFORM_BUFFER
	case GL_UNIFORM_BUFFER: return StrCRef("UNIFORM_BUFFER");
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


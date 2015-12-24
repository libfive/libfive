//  File implement/oglplus/enums/buffer_indexed_target_range.ipp
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
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLenum*,
	BufferIndexedTarget
> ValueRange_(BufferIndexedTarget*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_BUFFERINDEXEDTARGET)
#define OGLPLUS_IMPL_EVR_BUFFERINDEXEDTARGET
{
static const GLenum _values[] = {
#if defined GL_ATOMIC_COUNTER_BUFFER
GL_ATOMIC_COUNTER_BUFFER,
#endif
#if defined GL_SHADER_STORAGE_BUFFER
GL_SHADER_STORAGE_BUFFER,
#endif
#if defined GL_TRANSFORM_FEEDBACK_BUFFER
GL_TRANSFORM_FEEDBACK_BUFFER,
#endif
#if defined GL_UNIFORM_BUFFER
GL_UNIFORM_BUFFER,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	BufferIndexedTarget
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


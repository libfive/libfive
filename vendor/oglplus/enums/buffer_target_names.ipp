//  File implement/oglplus/enums/buffer_target_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/buffer_target.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	BufferTarget*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_BUFFERTARGET)
#define OGLPLUS_IMPL_EVN_BUFFERTARGET
{
switch(value)
{
#if defined GL_ARRAY_BUFFER
	case GL_ARRAY_BUFFER: return StrCRef("ARRAY_BUFFER");
#endif
#if defined GL_ATOMIC_COUNTER_BUFFER
	case GL_ATOMIC_COUNTER_BUFFER: return StrCRef("ATOMIC_COUNTER_BUFFER");
#endif
#if defined GL_COPY_READ_BUFFER
	case GL_COPY_READ_BUFFER: return StrCRef("COPY_READ_BUFFER");
#endif
#if defined GL_COPY_WRITE_BUFFER
	case GL_COPY_WRITE_BUFFER: return StrCRef("COPY_WRITE_BUFFER");
#endif
#if defined GL_DISPATCH_INDIRECT_BUFFER
	case GL_DISPATCH_INDIRECT_BUFFER: return StrCRef("DISPATCH_INDIRECT_BUFFER");
#endif
#if defined GL_DRAW_INDIRECT_BUFFER
	case GL_DRAW_INDIRECT_BUFFER: return StrCRef("DRAW_INDIRECT_BUFFER");
#endif
#if defined GL_ELEMENT_ARRAY_BUFFER
	case GL_ELEMENT_ARRAY_BUFFER: return StrCRef("ELEMENT_ARRAY_BUFFER");
#endif
#if defined GL_PIXEL_PACK_BUFFER
	case GL_PIXEL_PACK_BUFFER: return StrCRef("PIXEL_PACK_BUFFER");
#endif
#if defined GL_PIXEL_UNPACK_BUFFER
	case GL_PIXEL_UNPACK_BUFFER: return StrCRef("PIXEL_UNPACK_BUFFER");
#endif
#if defined GL_SHADER_STORAGE_BUFFER
	case GL_SHADER_STORAGE_BUFFER: return StrCRef("SHADER_STORAGE_BUFFER");
#endif
#if defined GL_TEXTURE_BUFFER
	case GL_TEXTURE_BUFFER: return StrCRef("TEXTURE_BUFFER");
#endif
#if defined GL_TRANSFORM_FEEDBACK_BUFFER
	case GL_TRANSFORM_FEEDBACK_BUFFER: return StrCRef("TRANSFORM_FEEDBACK_BUFFER");
#endif
#if defined GL_UNIFORM_BUFFER
	case GL_UNIFORM_BUFFER: return StrCRef("UNIFORM_BUFFER");
#endif
#if defined GL_QUERY_BUFFER
	case GL_QUERY_BUFFER: return StrCRef("QUERY_BUFFER");
#endif
#if defined GL_PARAMETER_BUFFER_ARB
	case GL_PARAMETER_BUFFER_ARB: return StrCRef("PARAMETER_BUFFER_ARB");
#endif
#if defined GL_EXTERNAL_VIRTUAL_MEMORY_BUFFER_AMD
	case GL_EXTERNAL_VIRTUAL_MEMORY_BUFFER_AMD: return StrCRef("EXTERNAL_VIRTUAL_MEMORY_BUFFER_AMD");
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


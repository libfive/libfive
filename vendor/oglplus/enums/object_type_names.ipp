//  File implement/oglplus/enums/object_type_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/object_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	ObjectType*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_OBJECTTYPE)
#define OGLPLUS_IMPL_EVN_OBJECTTYPE
{
switch(value)
{
#if defined GL_BUFFER
	case GL_BUFFER: return StrCRef("BUFFER");
#endif
#if defined GL_FRAMEBUFFER
	case GL_FRAMEBUFFER: return StrCRef("FRAMEBUFFER");
#endif
#if defined GL_PROGRAM_PIPELINE
	case GL_PROGRAM_PIPELINE: return StrCRef("PROGRAM_PIPELINE");
#endif
#if defined GL_PROGRAM
	case GL_PROGRAM: return StrCRef("PROGRAM");
#endif
#if defined GL_QUERY
	case GL_QUERY: return StrCRef("QUERY");
#endif
#if defined GL_RENDERBUFFER
	case GL_RENDERBUFFER: return StrCRef("RENDERBUFFER");
#endif
#if defined GL_SAMPLER
	case GL_SAMPLER: return StrCRef("SAMPLER");
#endif
#if defined GL_SHADER
	case GL_SHADER: return StrCRef("SHADER");
#endif
#if defined GL_TEXTURE
	case GL_TEXTURE: return StrCRef("TEXTURE");
#endif
#if defined GL_TRANSFORM_FEEDBACK
	case GL_TRANSFORM_FEEDBACK: return StrCRef("TRANSFORM_FEEDBACK");
#endif
#if defined GL_VERTEX_ARRAY
	case GL_VERTEX_ARRAY: return StrCRef("VERTEX_ARRAY");
#endif
#if defined GL_NONE
	case GL_NONE: return StrCRef("NONE");
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


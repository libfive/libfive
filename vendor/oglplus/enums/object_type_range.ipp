//  File implement/oglplus/enums/object_type_range.ipp
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
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLenum*,
	ObjectType
> ValueRange_(ObjectType*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_OBJECTTYPE)
#define OGLPLUS_IMPL_EVR_OBJECTTYPE
{
static const GLenum _values[] = {
#if defined GL_BUFFER
GL_BUFFER,
#endif
#if defined GL_FRAMEBUFFER
GL_FRAMEBUFFER,
#endif
#if defined GL_PROGRAM_PIPELINE
GL_PROGRAM_PIPELINE,
#endif
#if defined GL_PROGRAM
GL_PROGRAM,
#endif
#if defined GL_QUERY
GL_QUERY,
#endif
#if defined GL_RENDERBUFFER
GL_RENDERBUFFER,
#endif
#if defined GL_SAMPLER
GL_SAMPLER,
#endif
#if defined GL_SHADER
GL_SHADER,
#endif
#if defined GL_TEXTURE
GL_TEXTURE,
#endif
#if defined GL_TRANSFORM_FEEDBACK
GL_TRANSFORM_FEEDBACK,
#endif
#if defined GL_VERTEX_ARRAY
GL_VERTEX_ARRAY,
#endif
#if defined GL_NONE
GL_NONE,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	ObjectType
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


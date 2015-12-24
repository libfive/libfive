//  File implement/oglplus/enums/face_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/face.txt'
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
	Face
> ValueRange_(Face*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_FACE)
#define OGLPLUS_IMPL_EVR_FACE
{
static const GLenum _values[] = {
#if defined GL_FRONT
GL_FRONT,
#endif
#if defined GL_BACK
GL_BACK,
#endif
#if defined GL_FRONT_AND_BACK
GL_FRONT_AND_BACK,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	Face
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


//  File implement/oglplus/enums/color_buffer_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/color_buffer.txt'
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
	ColorBuffer
> ValueRange_(ColorBuffer*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_COLORBUFFER)
#define OGLPLUS_IMPL_EVR_COLORBUFFER
{
static const GLenum _values[] = {
#if defined GL_NONE
GL_NONE,
#endif
#if defined GL_FRONT_LEFT
GL_FRONT_LEFT,
#endif
#if defined GL_FRONT_RIGHT
GL_FRONT_RIGHT,
#endif
#if defined GL_BACK_LEFT
GL_BACK_LEFT,
#endif
#if defined GL_BACK_RIGHT
GL_BACK_RIGHT,
#endif
#if defined GL_FRONT
GL_FRONT,
#endif
#if defined GL_BACK
GL_BACK,
#endif
#if defined GL_LEFT
GL_LEFT,
#endif
#if defined GL_RIGHT
GL_RIGHT,
#endif
#if defined GL_FRONT_AND_BACK
GL_FRONT_AND_BACK,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	ColorBuffer
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


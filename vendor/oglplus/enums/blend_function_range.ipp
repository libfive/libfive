//  File implement/oglplus/enums/blend_function_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/blend_function.txt'
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
	BlendFunction
> ValueRange_(BlendFunction*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_BLENDFUNCTION)
#define OGLPLUS_IMPL_EVR_BLENDFUNCTION
{
static const GLenum _values[] = {
#if defined GL_ZERO
GL_ZERO,
#endif
#if defined GL_ONE
GL_ONE,
#endif
#if defined GL_SRC_COLOR
GL_SRC_COLOR,
#endif
#if defined GL_ONE_MINUS_SRC_COLOR
GL_ONE_MINUS_SRC_COLOR,
#endif
#if defined GL_DST_COLOR
GL_DST_COLOR,
#endif
#if defined GL_ONE_MINUS_DST_COLOR
GL_ONE_MINUS_DST_COLOR,
#endif
#if defined GL_SRC_ALPHA
GL_SRC_ALPHA,
#endif
#if defined GL_ONE_MINUS_SRC_ALPHA
GL_ONE_MINUS_SRC_ALPHA,
#endif
#if defined GL_DST_ALPHA
GL_DST_ALPHA,
#endif
#if defined GL_ONE_MINUS_DST_ALPHA
GL_ONE_MINUS_DST_ALPHA,
#endif
#if defined GL_CONSTANT_COLOR
GL_CONSTANT_COLOR,
#endif
#if defined GL_ONE_MINUS_CONSTANT_COLOR
GL_ONE_MINUS_CONSTANT_COLOR,
#endif
#if defined GL_CONSTANT_ALPHA
GL_CONSTANT_ALPHA,
#endif
#if defined GL_ONE_MINUS_CONSTANT_ALPHA
GL_ONE_MINUS_CONSTANT_ALPHA,
#endif
#if defined GL_SRC_ALPHA_SATURATE
GL_SRC_ALPHA_SATURATE,
#endif
#if defined GL_SRC1_COLOR
GL_SRC1_COLOR,
#endif
#if defined GL_ONE_MINUS_SRC1_COLOR
GL_ONE_MINUS_SRC1_COLOR,
#endif
#if defined GL_SRC1_ALPHA
GL_SRC1_ALPHA,
#endif
#if defined GL_ONE_MINUS_SRC1_ALPHA
GL_ONE_MINUS_SRC1_ALPHA,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	BlendFunction
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


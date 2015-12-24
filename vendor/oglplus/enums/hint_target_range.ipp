//  File implement/oglplus/enums/hint_target_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/hint_target.txt'
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
	HintTarget
> ValueRange_(HintTarget*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_HINTTARGET)
#define OGLPLUS_IMPL_EVR_HINTTARGET
{
static const GLenum _values[] = {
#if defined GL_LINE_SMOOTH_HINT
GL_LINE_SMOOTH_HINT,
#endif
#if defined GL_POLYGON_SMOOTH_HINT
GL_POLYGON_SMOOTH_HINT,
#endif
#if defined GL_TEXTURE_COMPRESSION_HINT
GL_TEXTURE_COMPRESSION_HINT,
#endif
#if defined GL_FRAGMENT_SHADER_DERIVATIVE_HINT
GL_FRAGMENT_SHADER_DERIVATIVE_HINT,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	HintTarget
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


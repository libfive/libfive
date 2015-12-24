//  File implement/oglplus/enums/ext/compat_matrix_mode_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/compat_matrix_mode.txt'
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
	CompatibilityMatrixMode
> ValueRange_(CompatibilityMatrixMode*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_COMPATIBILITYMATRIXMODE)
#define OGLPLUS_IMPL_EVR_COMPATIBILITYMATRIXMODE
{
static const GLenum _values[] = {
#if defined GL_PROJECTION
GL_PROJECTION,
#endif
#if defined GL_MODELVIEW
GL_MODELVIEW,
#endif
#if defined GL_TEXTURE
GL_TEXTURE,
#endif
#if defined GL_COLOR
GL_COLOR,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	CompatibilityMatrixMode
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


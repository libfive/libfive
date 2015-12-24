//  File implement/oglplus/enums/ext/nv_path_fill_mode_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_fill_mode.txt'
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
	PathNVFillMode
> ValueRange_(PathNVFillMode*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_PATHNVFILLMODE)
#define OGLPLUS_IMPL_EVR_PATHNVFILLMODE
{
static const GLenum _values[] = {
#if defined GL_INVERT
GL_INVERT,
#endif
#if defined GL_COUNT_UP_NV
GL_COUNT_UP_NV,
#endif
#if defined GL_COUNT_DOWN_NV
GL_COUNT_DOWN_NV,
#endif
#if defined GL_PATH_FILL_MODE_NV
GL_PATH_FILL_MODE_NV,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	PathNVFillMode
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


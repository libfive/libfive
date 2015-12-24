//  File implement/oglplus/enums/ext/nv_path_list_mode_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_list_mode.txt'
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
	PathNVListMode
> ValueRange_(PathNVListMode*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_PATHNVLISTMODE)
#define OGLPLUS_IMPL_EVR_PATHNVLISTMODE
{
static const GLenum _values[] = {
#if defined GL_ACCUM_ADJACENT_PAIRS_NV
GL_ACCUM_ADJACENT_PAIRS_NV,
#endif
#if defined GL_ADJACENT_PAIRS_NV
GL_ADJACENT_PAIRS_NV,
#endif
#if defined GL_FIRST_TO_REST_NV
GL_FIRST_TO_REST_NV,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	PathNVListMode
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


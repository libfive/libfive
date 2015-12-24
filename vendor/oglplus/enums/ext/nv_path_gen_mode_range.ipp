//  File implement/oglplus/enums/ext/nv_path_gen_mode_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_gen_mode.txt'
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
	PathNVGenMode
> ValueRange_(PathNVGenMode*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_PATHNVGENMODE)
#define OGLPLUS_IMPL_EVR_PATHNVGENMODE
{
static const GLenum _values[] = {
#if defined GL_NONE
GL_NONE,
#endif
#if defined GL_EYE_LINEAR
GL_EYE_LINEAR,
#endif
#if defined GL_OBJECT_LINEAR
GL_OBJECT_LINEAR,
#endif
#if defined GL_PATH_OBJECT_BOUNDING_BOX_NV
GL_PATH_OBJECT_BOUNDING_BOX_NV,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	PathNVGenMode
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


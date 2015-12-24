//  File implement/oglplus/enums/ext/nv_path_font_target_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_font_target.txt'
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
	PathNVFontTarget
> ValueRange_(PathNVFontTarget*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_PATHNVFONTTARGET)
#define OGLPLUS_IMPL_EVR_PATHNVFONTTARGET
{
static const GLenum _values[] = {
#if defined GL_STANDARD_FONT_NAME_NV
GL_STANDARD_FONT_NAME_NV,
#endif
#if defined GL_SYSTEM_FONT_NAME_NV
GL_SYSTEM_FONT_NAME_NV,
#endif
#if defined GL_FILE_NAME_NV
GL_FILE_NAME_NV,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	PathNVFontTarget
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


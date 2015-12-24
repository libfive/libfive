//  File implement/oglplus/enums/ext/nv_path_font_style_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_font_style.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLbitfield*,
	PathNVFontStyle
> ValueRange_(PathNVFontStyle*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_PATHNVFONTSTYLE)
#define OGLPLUS_IMPL_EVR_PATHNVFONTSTYLE
{
static const GLbitfield _values[] = {
#if defined GL_BOLD_BIT_NV
GL_BOLD_BIT_NV,
#endif
#if defined GL_ITALIC_BIT_NV
GL_ITALIC_BIT_NV,
#endif
0
};
return aux::CastIterRange<
	const GLbitfield*,
	PathNVFontStyle
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


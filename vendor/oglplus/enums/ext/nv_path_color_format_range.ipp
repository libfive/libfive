//  File implement/oglplus/enums/ext/nv_path_color_format_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_color_format.txt'
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
	PathNVColorFormat
> ValueRange_(PathNVColorFormat*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_PATHNVCOLORFORMAT)
#define OGLPLUS_IMPL_EVR_PATHNVCOLORFORMAT
{
static const GLenum _values[] = {
#if defined GL_LUMINANCE
GL_LUMINANCE,
#endif
#if defined GL_ALPHA
GL_ALPHA,
#endif
#if defined GL_INTENSITY
GL_INTENSITY,
#endif
#if defined GL_LUMINANCE_ALPHA
GL_LUMINANCE_ALPHA,
#endif
#if defined GL_RGB
GL_RGB,
#endif
#if defined GL_RGBA
GL_RGBA,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	PathNVColorFormat
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


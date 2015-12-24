//  File implement/oglplus/enums/pixel_data_format_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/pixel_data_format.txt'
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
	PixelDataFormat
> ValueRange_(PixelDataFormat*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_PIXELDATAFORMAT)
#define OGLPLUS_IMPL_EVR_PIXELDATAFORMAT
{
static const GLenum _values[] = {
#if defined GL_DEPTH_COMPONENT
GL_DEPTH_COMPONENT,
#endif
#if defined GL_DEPTH_STENCIL
GL_DEPTH_STENCIL,
#endif
#if defined GL_STENCIL_INDEX
GL_STENCIL_INDEX,
#endif
#if defined GL_RED
GL_RED,
#endif
#if defined GL_GREEN
GL_GREEN,
#endif
#if defined GL_BLUE
GL_BLUE,
#endif
#if defined GL_RG
GL_RG,
#endif
#if defined GL_RGB
GL_RGB,
#endif
#if defined GL_RGBA
GL_RGBA,
#endif
#if defined GL_BGR
GL_BGR,
#endif
#if defined GL_BGRA
GL_BGRA,
#endif
#if defined GL_RED_INTEGER
GL_RED_INTEGER,
#endif
#if defined GL_GREEN_INTEGER
GL_GREEN_INTEGER,
#endif
#if defined GL_BLUE_INTEGER
GL_BLUE_INTEGER,
#endif
#if defined GL_RG_INTEGER
GL_RG_INTEGER,
#endif
#if defined GL_RGB_INTEGER
GL_RGB_INTEGER,
#endif
#if defined GL_RGBA_INTEGER
GL_RGBA_INTEGER,
#endif
#if defined GL_BGR_INTEGER
GL_BGR_INTEGER,
#endif
#if defined GL_BGRA_INTEGER
GL_BGRA_INTEGER,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	PixelDataFormat
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


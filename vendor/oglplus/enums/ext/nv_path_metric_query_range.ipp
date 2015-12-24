//  File implement/oglplus/enums/ext/nv_path_metric_query_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_metric_query.txt'
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
	PathNVMetricQuery
> ValueRange_(PathNVMetricQuery*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_PATHNVMETRICQUERY)
#define OGLPLUS_IMPL_EVR_PATHNVMETRICQUERY
{
static const GLbitfield _values[] = {
#if defined GL_GLYPH_WIDTH_BIT_NV
GL_GLYPH_WIDTH_BIT_NV,
#endif
#if defined GL_GLYPH_HEIGHT_BIT_NV
GL_GLYPH_HEIGHT_BIT_NV,
#endif
#if defined GL_GLYPH_HORIZONTAL_BEARING_X_BIT_NV
GL_GLYPH_HORIZONTAL_BEARING_X_BIT_NV,
#endif
#if defined GL_GLYPH_HORIZONTAL_BEARING_Y_BIT_NV
GL_GLYPH_HORIZONTAL_BEARING_Y_BIT_NV,
#endif
#if defined GL_GLYPH_HORIZONTAL_BEARING_ADVANCE_BIT_NV
GL_GLYPH_HORIZONTAL_BEARING_ADVANCE_BIT_NV,
#endif
#if defined GL_GLYPH_VERTICAL_BEARING_X_BIT_NV
GL_GLYPH_VERTICAL_BEARING_X_BIT_NV,
#endif
#if defined GL_GLYPH_VERTICAL_BEARING_Y_BIT_NV
GL_GLYPH_VERTICAL_BEARING_Y_BIT_NV,
#endif
#if defined GL_GLYPH_VERTICAL_BEARING_ADVANCE_BIT_NV
GL_GLYPH_VERTICAL_BEARING_ADVANCE_BIT_NV,
#endif
#if defined GL_GLYPH_HAS_KERNING_BIT_NV
GL_GLYPH_HAS_KERNING_BIT_NV,
#endif
#if defined GL_FONT_X_MIN_BOUNDS_BIT_NV
GL_FONT_X_MIN_BOUNDS_BIT_NV,
#endif
#if defined GL_FONT_Y_MIN_BOUNDS_BIT_NV
GL_FONT_Y_MIN_BOUNDS_BIT_NV,
#endif
#if defined GL_FONT_X_MAX_BOUNDS_BIT_NV
GL_FONT_X_MAX_BOUNDS_BIT_NV,
#endif
#if defined GL_FONT_Y_MAX_BOUNDS_BIT_NV
GL_FONT_Y_MAX_BOUNDS_BIT_NV,
#endif
#if defined GL_FONT_UNITS_PER_EM_BIT_NV
GL_FONT_UNITS_PER_EM_BIT_NV,
#endif
#if defined GL_FONT_ASCENDER_BIT_NV
GL_FONT_ASCENDER_BIT_NV,
#endif
#if defined GL_FONT_DESCENDER_BIT_NV
GL_FONT_DESCENDER_BIT_NV,
#endif
#if defined GL_FONT_HEIGHT_BIT_NV
GL_FONT_HEIGHT_BIT_NV,
#endif
#if defined GL_FONT_MAX_ADVANCE_WIDTH_BIT_NV
GL_FONT_MAX_ADVANCE_WIDTH_BIT_NV,
#endif
#if defined GL_FONT_MAX_ADVANCE_HEIGHT_BIT_NV
GL_FONT_MAX_ADVANCE_HEIGHT_BIT_NV,
#endif
#if defined GL_FONT_UNDERLINE_POSITION_BIT_NV
GL_FONT_UNDERLINE_POSITION_BIT_NV,
#endif
#if defined GL_FONT_UNDERLINE_THICKNESS_BIT_NV
GL_FONT_UNDERLINE_THICKNESS_BIT_NV,
#endif
#if defined GL_FONT_HAS_KERNING_BIT_NV
GL_FONT_HAS_KERNING_BIT_NV,
#endif
0
};
return aux::CastIterRange<
	const GLbitfield*,
	PathNVMetricQuery
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


//  File implement/oglplus/enums/ext/nv_path_metric_query_class.ipp
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
template <typename Base, template<PathNVMetricQuery> class Transform>
class EnumToClass<Base, PathNVMetricQuery, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_GLYPH_WIDTH_BIT_NV
# if defined GlyphWidth
#  pragma push_macro("GlyphWidth")
#  undef GlyphWidth
	Transform<PathNVMetricQuery::GlyphWidth> GlyphWidth;
#  pragma pop_macro("GlyphWidth")
# else
	Transform<PathNVMetricQuery::GlyphWidth> GlyphWidth;
# endif
#endif
#if defined GL_GLYPH_HEIGHT_BIT_NV
# if defined GlyphHeight
#  pragma push_macro("GlyphHeight")
#  undef GlyphHeight
	Transform<PathNVMetricQuery::GlyphHeight> GlyphHeight;
#  pragma pop_macro("GlyphHeight")
# else
	Transform<PathNVMetricQuery::GlyphHeight> GlyphHeight;
# endif
#endif
#if defined GL_GLYPH_HORIZONTAL_BEARING_X_BIT_NV
# if defined GlyphHorizontalBearingX
#  pragma push_macro("GlyphHorizontalBearingX")
#  undef GlyphHorizontalBearingX
	Transform<PathNVMetricQuery::GlyphHorizontalBearingX> GlyphHorizontalBearingX;
#  pragma pop_macro("GlyphHorizontalBearingX")
# else
	Transform<PathNVMetricQuery::GlyphHorizontalBearingX> GlyphHorizontalBearingX;
# endif
#endif
#if defined GL_GLYPH_HORIZONTAL_BEARING_Y_BIT_NV
# if defined GlyphHorizontalBearingY
#  pragma push_macro("GlyphHorizontalBearingY")
#  undef GlyphHorizontalBearingY
	Transform<PathNVMetricQuery::GlyphHorizontalBearingY> GlyphHorizontalBearingY;
#  pragma pop_macro("GlyphHorizontalBearingY")
# else
	Transform<PathNVMetricQuery::GlyphHorizontalBearingY> GlyphHorizontalBearingY;
# endif
#endif
#if defined GL_GLYPH_HORIZONTAL_BEARING_ADVANCE_BIT_NV
# if defined GlyphHorizontalBearingAdvance
#  pragma push_macro("GlyphHorizontalBearingAdvance")
#  undef GlyphHorizontalBearingAdvance
	Transform<PathNVMetricQuery::GlyphHorizontalBearingAdvance> GlyphHorizontalBearingAdvance;
#  pragma pop_macro("GlyphHorizontalBearingAdvance")
# else
	Transform<PathNVMetricQuery::GlyphHorizontalBearingAdvance> GlyphHorizontalBearingAdvance;
# endif
#endif
#if defined GL_GLYPH_VERTICAL_BEARING_X_BIT_NV
# if defined GlyphVerticalBearingX
#  pragma push_macro("GlyphVerticalBearingX")
#  undef GlyphVerticalBearingX
	Transform<PathNVMetricQuery::GlyphVerticalBearingX> GlyphVerticalBearingX;
#  pragma pop_macro("GlyphVerticalBearingX")
# else
	Transform<PathNVMetricQuery::GlyphVerticalBearingX> GlyphVerticalBearingX;
# endif
#endif
#if defined GL_GLYPH_VERTICAL_BEARING_Y_BIT_NV
# if defined GlyphVerticalBearingY
#  pragma push_macro("GlyphVerticalBearingY")
#  undef GlyphVerticalBearingY
	Transform<PathNVMetricQuery::GlyphVerticalBearingY> GlyphVerticalBearingY;
#  pragma pop_macro("GlyphVerticalBearingY")
# else
	Transform<PathNVMetricQuery::GlyphVerticalBearingY> GlyphVerticalBearingY;
# endif
#endif
#if defined GL_GLYPH_VERTICAL_BEARING_ADVANCE_BIT_NV
# if defined GlyphVerticalBearingAdvance
#  pragma push_macro("GlyphVerticalBearingAdvance")
#  undef GlyphVerticalBearingAdvance
	Transform<PathNVMetricQuery::GlyphVerticalBearingAdvance> GlyphVerticalBearingAdvance;
#  pragma pop_macro("GlyphVerticalBearingAdvance")
# else
	Transform<PathNVMetricQuery::GlyphVerticalBearingAdvance> GlyphVerticalBearingAdvance;
# endif
#endif
#if defined GL_GLYPH_HAS_KERNING_BIT_NV
# if defined GlyphHasKerning
#  pragma push_macro("GlyphHasKerning")
#  undef GlyphHasKerning
	Transform<PathNVMetricQuery::GlyphHasKerning> GlyphHasKerning;
#  pragma pop_macro("GlyphHasKerning")
# else
	Transform<PathNVMetricQuery::GlyphHasKerning> GlyphHasKerning;
# endif
#endif
#if defined GL_FONT_X_MIN_BOUNDS_BIT_NV
# if defined FontXMinBounds
#  pragma push_macro("FontXMinBounds")
#  undef FontXMinBounds
	Transform<PathNVMetricQuery::FontXMinBounds> FontXMinBounds;
#  pragma pop_macro("FontXMinBounds")
# else
	Transform<PathNVMetricQuery::FontXMinBounds> FontXMinBounds;
# endif
#endif
#if defined GL_FONT_Y_MIN_BOUNDS_BIT_NV
# if defined FontYMinBounds
#  pragma push_macro("FontYMinBounds")
#  undef FontYMinBounds
	Transform<PathNVMetricQuery::FontYMinBounds> FontYMinBounds;
#  pragma pop_macro("FontYMinBounds")
# else
	Transform<PathNVMetricQuery::FontYMinBounds> FontYMinBounds;
# endif
#endif
#if defined GL_FONT_X_MAX_BOUNDS_BIT_NV
# if defined FontXMaxBounds
#  pragma push_macro("FontXMaxBounds")
#  undef FontXMaxBounds
	Transform<PathNVMetricQuery::FontXMaxBounds> FontXMaxBounds;
#  pragma pop_macro("FontXMaxBounds")
# else
	Transform<PathNVMetricQuery::FontXMaxBounds> FontXMaxBounds;
# endif
#endif
#if defined GL_FONT_Y_MAX_BOUNDS_BIT_NV
# if defined FontYMaxBounds
#  pragma push_macro("FontYMaxBounds")
#  undef FontYMaxBounds
	Transform<PathNVMetricQuery::FontYMaxBounds> FontYMaxBounds;
#  pragma pop_macro("FontYMaxBounds")
# else
	Transform<PathNVMetricQuery::FontYMaxBounds> FontYMaxBounds;
# endif
#endif
#if defined GL_FONT_UNITS_PER_EM_BIT_NV
# if defined FontUnitsPerEm
#  pragma push_macro("FontUnitsPerEm")
#  undef FontUnitsPerEm
	Transform<PathNVMetricQuery::FontUnitsPerEm> FontUnitsPerEm;
#  pragma pop_macro("FontUnitsPerEm")
# else
	Transform<PathNVMetricQuery::FontUnitsPerEm> FontUnitsPerEm;
# endif
#endif
#if defined GL_FONT_ASCENDER_BIT_NV
# if defined FontAscender
#  pragma push_macro("FontAscender")
#  undef FontAscender
	Transform<PathNVMetricQuery::FontAscender> FontAscender;
#  pragma pop_macro("FontAscender")
# else
	Transform<PathNVMetricQuery::FontAscender> FontAscender;
# endif
#endif
#if defined GL_FONT_DESCENDER_BIT_NV
# if defined FontDescender
#  pragma push_macro("FontDescender")
#  undef FontDescender
	Transform<PathNVMetricQuery::FontDescender> FontDescender;
#  pragma pop_macro("FontDescender")
# else
	Transform<PathNVMetricQuery::FontDescender> FontDescender;
# endif
#endif
#if defined GL_FONT_HEIGHT_BIT_NV
# if defined FontHeight
#  pragma push_macro("FontHeight")
#  undef FontHeight
	Transform<PathNVMetricQuery::FontHeight> FontHeight;
#  pragma pop_macro("FontHeight")
# else
	Transform<PathNVMetricQuery::FontHeight> FontHeight;
# endif
#endif
#if defined GL_FONT_MAX_ADVANCE_WIDTH_BIT_NV
# if defined FontMaxAdvanceWidth
#  pragma push_macro("FontMaxAdvanceWidth")
#  undef FontMaxAdvanceWidth
	Transform<PathNVMetricQuery::FontMaxAdvanceWidth> FontMaxAdvanceWidth;
#  pragma pop_macro("FontMaxAdvanceWidth")
# else
	Transform<PathNVMetricQuery::FontMaxAdvanceWidth> FontMaxAdvanceWidth;
# endif
#endif
#if defined GL_FONT_MAX_ADVANCE_HEIGHT_BIT_NV
# if defined FontMaxAdvanceHeight
#  pragma push_macro("FontMaxAdvanceHeight")
#  undef FontMaxAdvanceHeight
	Transform<PathNVMetricQuery::FontMaxAdvanceHeight> FontMaxAdvanceHeight;
#  pragma pop_macro("FontMaxAdvanceHeight")
# else
	Transform<PathNVMetricQuery::FontMaxAdvanceHeight> FontMaxAdvanceHeight;
# endif
#endif
#if defined GL_FONT_UNDERLINE_POSITION_BIT_NV
# if defined FontUnderlinePosition
#  pragma push_macro("FontUnderlinePosition")
#  undef FontUnderlinePosition
	Transform<PathNVMetricQuery::FontUnderlinePosition> FontUnderlinePosition;
#  pragma pop_macro("FontUnderlinePosition")
# else
	Transform<PathNVMetricQuery::FontUnderlinePosition> FontUnderlinePosition;
# endif
#endif
#if defined GL_FONT_UNDERLINE_THICKNESS_BIT_NV
# if defined FontUnderlineThickness
#  pragma push_macro("FontUnderlineThickness")
#  undef FontUnderlineThickness
	Transform<PathNVMetricQuery::FontUnderlineThickness> FontUnderlineThickness;
#  pragma pop_macro("FontUnderlineThickness")
# else
	Transform<PathNVMetricQuery::FontUnderlineThickness> FontUnderlineThickness;
# endif
#endif
#if defined GL_FONT_HAS_KERNING_BIT_NV
# if defined FontHasKerning
#  pragma push_macro("FontHasKerning")
#  undef FontHasKerning
	Transform<PathNVMetricQuery::FontHasKerning> FontHasKerning;
#  pragma pop_macro("FontHasKerning")
# else
	Transform<PathNVMetricQuery::FontHasKerning> FontHasKerning;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_GLYPH_WIDTH_BIT_NV
# if defined GlyphWidth
#  pragma push_macro("GlyphWidth")
#  undef GlyphWidth
	 , GlyphWidth(_base())
#  pragma pop_macro("GlyphWidth")
# else
	 , GlyphWidth(_base())
# endif
#endif
#if defined GL_GLYPH_HEIGHT_BIT_NV
# if defined GlyphHeight
#  pragma push_macro("GlyphHeight")
#  undef GlyphHeight
	 , GlyphHeight(_base())
#  pragma pop_macro("GlyphHeight")
# else
	 , GlyphHeight(_base())
# endif
#endif
#if defined GL_GLYPH_HORIZONTAL_BEARING_X_BIT_NV
# if defined GlyphHorizontalBearingX
#  pragma push_macro("GlyphHorizontalBearingX")
#  undef GlyphHorizontalBearingX
	 , GlyphHorizontalBearingX(_base())
#  pragma pop_macro("GlyphHorizontalBearingX")
# else
	 , GlyphHorizontalBearingX(_base())
# endif
#endif
#if defined GL_GLYPH_HORIZONTAL_BEARING_Y_BIT_NV
# if defined GlyphHorizontalBearingY
#  pragma push_macro("GlyphHorizontalBearingY")
#  undef GlyphHorizontalBearingY
	 , GlyphHorizontalBearingY(_base())
#  pragma pop_macro("GlyphHorizontalBearingY")
# else
	 , GlyphHorizontalBearingY(_base())
# endif
#endif
#if defined GL_GLYPH_HORIZONTAL_BEARING_ADVANCE_BIT_NV
# if defined GlyphHorizontalBearingAdvance
#  pragma push_macro("GlyphHorizontalBearingAdvance")
#  undef GlyphHorizontalBearingAdvance
	 , GlyphHorizontalBearingAdvance(_base())
#  pragma pop_macro("GlyphHorizontalBearingAdvance")
# else
	 , GlyphHorizontalBearingAdvance(_base())
# endif
#endif
#if defined GL_GLYPH_VERTICAL_BEARING_X_BIT_NV
# if defined GlyphVerticalBearingX
#  pragma push_macro("GlyphVerticalBearingX")
#  undef GlyphVerticalBearingX
	 , GlyphVerticalBearingX(_base())
#  pragma pop_macro("GlyphVerticalBearingX")
# else
	 , GlyphVerticalBearingX(_base())
# endif
#endif
#if defined GL_GLYPH_VERTICAL_BEARING_Y_BIT_NV
# if defined GlyphVerticalBearingY
#  pragma push_macro("GlyphVerticalBearingY")
#  undef GlyphVerticalBearingY
	 , GlyphVerticalBearingY(_base())
#  pragma pop_macro("GlyphVerticalBearingY")
# else
	 , GlyphVerticalBearingY(_base())
# endif
#endif
#if defined GL_GLYPH_VERTICAL_BEARING_ADVANCE_BIT_NV
# if defined GlyphVerticalBearingAdvance
#  pragma push_macro("GlyphVerticalBearingAdvance")
#  undef GlyphVerticalBearingAdvance
	 , GlyphVerticalBearingAdvance(_base())
#  pragma pop_macro("GlyphVerticalBearingAdvance")
# else
	 , GlyphVerticalBearingAdvance(_base())
# endif
#endif
#if defined GL_GLYPH_HAS_KERNING_BIT_NV
# if defined GlyphHasKerning
#  pragma push_macro("GlyphHasKerning")
#  undef GlyphHasKerning
	 , GlyphHasKerning(_base())
#  pragma pop_macro("GlyphHasKerning")
# else
	 , GlyphHasKerning(_base())
# endif
#endif
#if defined GL_FONT_X_MIN_BOUNDS_BIT_NV
# if defined FontXMinBounds
#  pragma push_macro("FontXMinBounds")
#  undef FontXMinBounds
	 , FontXMinBounds(_base())
#  pragma pop_macro("FontXMinBounds")
# else
	 , FontXMinBounds(_base())
# endif
#endif
#if defined GL_FONT_Y_MIN_BOUNDS_BIT_NV
# if defined FontYMinBounds
#  pragma push_macro("FontYMinBounds")
#  undef FontYMinBounds
	 , FontYMinBounds(_base())
#  pragma pop_macro("FontYMinBounds")
# else
	 , FontYMinBounds(_base())
# endif
#endif
#if defined GL_FONT_X_MAX_BOUNDS_BIT_NV
# if defined FontXMaxBounds
#  pragma push_macro("FontXMaxBounds")
#  undef FontXMaxBounds
	 , FontXMaxBounds(_base())
#  pragma pop_macro("FontXMaxBounds")
# else
	 , FontXMaxBounds(_base())
# endif
#endif
#if defined GL_FONT_Y_MAX_BOUNDS_BIT_NV
# if defined FontYMaxBounds
#  pragma push_macro("FontYMaxBounds")
#  undef FontYMaxBounds
	 , FontYMaxBounds(_base())
#  pragma pop_macro("FontYMaxBounds")
# else
	 , FontYMaxBounds(_base())
# endif
#endif
#if defined GL_FONT_UNITS_PER_EM_BIT_NV
# if defined FontUnitsPerEm
#  pragma push_macro("FontUnitsPerEm")
#  undef FontUnitsPerEm
	 , FontUnitsPerEm(_base())
#  pragma pop_macro("FontUnitsPerEm")
# else
	 , FontUnitsPerEm(_base())
# endif
#endif
#if defined GL_FONT_ASCENDER_BIT_NV
# if defined FontAscender
#  pragma push_macro("FontAscender")
#  undef FontAscender
	 , FontAscender(_base())
#  pragma pop_macro("FontAscender")
# else
	 , FontAscender(_base())
# endif
#endif
#if defined GL_FONT_DESCENDER_BIT_NV
# if defined FontDescender
#  pragma push_macro("FontDescender")
#  undef FontDescender
	 , FontDescender(_base())
#  pragma pop_macro("FontDescender")
# else
	 , FontDescender(_base())
# endif
#endif
#if defined GL_FONT_HEIGHT_BIT_NV
# if defined FontHeight
#  pragma push_macro("FontHeight")
#  undef FontHeight
	 , FontHeight(_base())
#  pragma pop_macro("FontHeight")
# else
	 , FontHeight(_base())
# endif
#endif
#if defined GL_FONT_MAX_ADVANCE_WIDTH_BIT_NV
# if defined FontMaxAdvanceWidth
#  pragma push_macro("FontMaxAdvanceWidth")
#  undef FontMaxAdvanceWidth
	 , FontMaxAdvanceWidth(_base())
#  pragma pop_macro("FontMaxAdvanceWidth")
# else
	 , FontMaxAdvanceWidth(_base())
# endif
#endif
#if defined GL_FONT_MAX_ADVANCE_HEIGHT_BIT_NV
# if defined FontMaxAdvanceHeight
#  pragma push_macro("FontMaxAdvanceHeight")
#  undef FontMaxAdvanceHeight
	 , FontMaxAdvanceHeight(_base())
#  pragma pop_macro("FontMaxAdvanceHeight")
# else
	 , FontMaxAdvanceHeight(_base())
# endif
#endif
#if defined GL_FONT_UNDERLINE_POSITION_BIT_NV
# if defined FontUnderlinePosition
#  pragma push_macro("FontUnderlinePosition")
#  undef FontUnderlinePosition
	 , FontUnderlinePosition(_base())
#  pragma pop_macro("FontUnderlinePosition")
# else
	 , FontUnderlinePosition(_base())
# endif
#endif
#if defined GL_FONT_UNDERLINE_THICKNESS_BIT_NV
# if defined FontUnderlineThickness
#  pragma push_macro("FontUnderlineThickness")
#  undef FontUnderlineThickness
	 , FontUnderlineThickness(_base())
#  pragma pop_macro("FontUnderlineThickness")
# else
	 , FontUnderlineThickness(_base())
# endif
#endif
#if defined GL_FONT_HAS_KERNING_BIT_NV
# if defined FontHasKerning
#  pragma push_macro("FontHasKerning")
#  undef FontHasKerning
	 , FontHasKerning(_base())
#  pragma pop_macro("FontHasKerning")
# else
	 , FontHasKerning(_base())
# endif
#endif
	{ }
};

} // namespace enums


/**
 *  @file oglplus/text/pango_cairo/font.ipp
 *  @brief Implementation of pango/cairo-based text rendering, font
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2013 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */


namespace oglplus {
namespace text {

OGLPLUS_LIB_FUNC
PangoCairoFontEssence::PangoCairoFontEssence(const char* font_name)
 : _font_desc(
	::pango_font_description_from_string(font_name),
	::pango_font_description_free
), _font_map(
	::pango_cairo_font_map_get_default(),
	::g_object_unref
), _context(
	::pango_font_map_create_context(_font_map),
	::g_object_unref
), _font(
	::pango_font_map_load_font(_font_map, _context, _font_desc),
	::g_object_unref
), _font_metrics(
	::pango_font_get_metrics(_font, nullptr),
	::pango_font_metrics_unref
)
{ }

} // namespace text
} // namespace oglplus


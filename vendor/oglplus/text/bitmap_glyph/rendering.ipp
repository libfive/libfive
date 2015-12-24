/**
 *  @file oglplus/text/bitmap_glyph/fwd.hpp
 *  @brief Implementation of Bitmap-font-based text rendering utilities
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

namespace oglplus {
namespace text {

OGLPLUS_LIB_FUNC
std::string BitmapGlyphPageName(
	const BitmapGlyphRenderingBase& parent,
	GLuint page
)
{
	const char hexdigit[16] = {
		'0','1','2','3','4',
		'5','6','7','8','9',
		'A','B','C','D','E','F'
	};
	GLuint page_head = page*BitmapGlyphGlyphsPerPage(parent);
	const char result[6] = {
		hexdigit[(page_head >> 20) & 0x0F],
		hexdigit[(page_head >> 16) & 0x0F],
		hexdigit[(page_head >> 12) & 0x0F],
		hexdigit[(page_head >>  8) & 0x0F],
		hexdigit[(page_head >>  4) & 0x0F],
		hexdigit[(page_head >>  0) & 0x0F]
	};
	return std::string(result, 6);
}

OGLPLUS_LIB_FUNC
GLuint BitmapGlyphDefaultPageTexSide(
	const BitmapGlyphRenderingBase& /*parent*/,
	GLuint pixel_height
)
{
	GLuint h = pixel_height*16;
	GLuint s = 128;
	while(true)
	{
		if(s >= h) break;
		s *= 2;
	}
	return s;
}

} // namespace text
} // namespace oglplus


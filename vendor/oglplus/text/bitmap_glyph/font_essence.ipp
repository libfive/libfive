/**
 *  @file oglplus/text/bitmap_glyph/font_essence.ipp
 *  @brief Implementation of Bitmap-font-based text rendering, font essence
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/images/load.hpp>
#include <oglplus/lib/incl_end.ipp>

namespace oglplus {
namespace text {

OGLPLUS_LIB_FUNC
oglplus::images::Image BitmapGlyphFontEssence::_load_page_bitmap(GLuint page)
{
	return images::LoadByName(
		"fonts",
		_font_name + aux::FilesysPathSep() +
		BitmapGlyphPageName(_parent, page),
		true,
		true
	);
}

OGLPLUS_LIB_FUNC
void BitmapGlyphFontEssence::_check_input(std::istream& input)
{
	assert(!input.fail());
	if(input.eof())
		throw std::runtime_error("Unexpected EOF in .bgm file");
	if(input.bad())
		throw std::runtime_error("Error reading .bgm file");
}

OGLPLUS_NORETURN
OGLPLUS_LIB_FUNC
void BitmapGlyphFontEssence::_unexpected_char(char)
{
	throw std::runtime_error("Unexpected character in .bgm file");
}

OGLPLUS_LIB_FUNC
void BitmapGlyphFontEssence::_load_single_glyph(
	std::istream& input,
	char* line,
	const std::streamsize linelen,
	GLfloat* values,
	const size_t n_values
)
{
	// read the code-point of the glyph
	_check_input(input);
	unsigned cp;
	input >> cp;
	// eat the newline
	_check_input(input);
	input.getline(line, linelen);
	// skip the hex code line
	_check_input(input);
	input.getline(line, linelen);
	// skip the code point's quoted utf-8 sequence
	char c;
	_check_input(input);
	input.get(c);
	_check_input(input);
	if(c != '\'') _unexpected_char(c);
	do { input.get(c); _check_input(input); }
	while(c != '\'');
	input.getline(line, linelen);
	//
	// read the n metric values
	for(unsigned v=0; v!=n_values; ++v)
	{
		// read the value
		GLfloat value;
		_check_input(input);
		input >> value;
		// store the value
		values[v] = value;
		// eat the rest of the line
		input.getline(line, linelen);
	}
	// skip the separating line
	_check_input(input);
	input.getline(line, linelen);
}

OGLPLUS_LIB_FUNC
std::vector<GLfloat> BitmapGlyphFontEssence::_load_page_metric(GLuint page)
{
	ResourceFile input(
		"fonts",
		_font_name + aux::FilesysPathSep() +
		BitmapGlyphPageName(_parent, page),
		".bgm"
	);
	// 4 values * 3
	//
	// x - logical rectangle left bearing
	// y - logical rectangle right bearing
	// z - logical rectangle ascent
	// w - logical rectangle descent
	//
	// x - ink rectangle left bearing
	// y - ink rectangle right bearing
	// z - ink rectangle ascent
	// w - ink rectangle descent
	//
	// x - Glyph origin x in normalized texture space
	// y - Glyph origin y in normalized texture space
	// z - Glyph width in normalized texture space
	// w - Glyph height in normalized texture space
	unsigned values_per_glyph = 4*3;
	unsigned glyphs_per_page = BitmapGlyphGlyphsPerPage(_parent);
	std::vector<GLfloat> metrics(glyphs_per_page*values_per_glyph);

	const size_t linelen = 63;
	char line[linelen+1];
	for(unsigned g=0; g!=glyphs_per_page; ++g)
	{
		_load_single_glyph(
			input,
			line,
			linelen,
			metrics.data()+g*values_per_glyph,
			values_per_glyph
		);
	}
	return metrics;
}

OGLPLUS_LIB_FUNC
GLfloat BitmapGlyphFontEssence::QueryXOffsets(
	const CodePoint* cps,
	SizeType size,
	std::vector<GLfloat>& x_offsets
) const
{
	if(size <= 0) return 0.0f;
	x_offsets.resize(size);

	std::size_t i = 0, n = std::size_t(size);


	GLuint page = BitmapGlyphPageOfCP(_parent, cps[i]);
	GLuint cell = BitmapGlyphCellOfCP(_parent, cps[i]);
	GLint frame = _pager.FrameOfPage(page);
	assert(!(frame < 0));
	// Logical left bearing
	GLfloat sum = _page_storage.GetGlyphMetric(GLuint(frame), cell, 0);
	x_offsets[i] = sum;

	while(++i < n)
	{
		sum += _page_storage.GetGlyphWidth(GLuint(frame), cell);
		x_offsets[i] = sum;
		page = BitmapGlyphPageOfCP(_parent, cps[i]);
		cell = BitmapGlyphCellOfCP(_parent, cps[i]);
		frame = _pager.FrameOfPage(page);
		assert(!(frame < 0));
	}
	return sum + _page_storage.GetGlyphWidth(GLuint(frame), cell);
}

OGLPLUS_LIB_FUNC
Rectangle BitmapGlyphFontEssence::GetGlyphMetrics(
	CodePoint code_point,
	GLuint offs
) const
{
	assert(offs % 4 == 0);

	GLuint page = BitmapGlyphPageOfCP(_parent, code_point);
	GLuint cell = BitmapGlyphCellOfCP(_parent, code_point);
	GLint frame = _pager.FrameOfPage(page);
	assert(!(frame < 0));

	GLfloat buf[4];
	_page_storage.QueryGlyphMetrics(GLuint(frame), cell, offs, 4, buf);
	return Rectangle(buf[0], buf[1],-buf[3], buf[2]);
}

} // namespace text
} // namespace oglplus


/**
 *  @file oglplus/text/stb_truetype/font_essence.hpp
 *  @brief Implementation of STBTTFontEssence
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXT_STB_TRUETYPE_FONT_ESSENCE_HPP
#define OGLPLUS_TEXT_STB_TRUETYPE_FONT_ESSENCE_HPP

#include <oglplus/config/basic.hpp>
#include <oglplus/text/stb_truetype/font2d.hpp>
#include <oglplus/text/bitmap_glyph/fwd.hpp>
#include <oglplus/text/bitmap_glyph/page_storage.hpp>
#include <oglplus/text/bitmap_glyph/pager.hpp>

#include <oglplus/images/image.hpp>
#include <oglplus/opt/resources.hpp>

#include <cctype>
#include <string>
#include <fstream>

namespace oglplus {
namespace text {

class STBTTFontEssence
{
private:
	BitmapGlyphRenderingBase& _parent;
	const STBTTFont2D _tt_font;
	const GLuint _font_resolution;
	const GLuint _tex_side;

	void _do_make_page_bitmap_and_metric(
		GLuint page,
		unsigned char* bmp_data,
		float* metric
	) const;

	oglplus::images::Image _make_page_bitmap(GLuint page)
	{
		std::vector<unsigned char> bmp(_tex_side*_tex_side);
		_do_make_page_bitmap_and_metric(page, bmp.data(), nullptr);
		return images::Image(
			_tex_side,
			_tex_side,
			1,
			1,
			bmp.data(),
			PixelDataFormat::Red,
			PixelDataInternalFormat::R8
		);
	}

	std::vector<GLfloat> _make_page_metric(GLuint page)
	{
		unsigned glyphs_per_page = BitmapGlyphGlyphsPerPage(_parent);
		std::vector<GLfloat> metrics(glyphs_per_page*12);

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
		_do_make_page_bitmap_and_metric(page, nullptr, metrics.data());

		return metrics;
	}

	BitmapGlyphPager _pager;
	const GLuint _initial_frame;
	BitmapGlyphPageStorage _page_storage;

	void _do_load_pages(
		const GLuint* elem,
		GLsizei size
	);
public:
	STBTTFontEssence(
		BitmapGlyphRenderingBase& parent,
		TextureUnitSelector bitmap_tex_unit,
		TextureUnitSelector metric_tex_unit,
		TextureUnitSelector pg_map_tex_unit,
		const std::string& font_name,
		SizeType frames,
		GLuint default_page,
		GLuint pixel_height
	);

	void Use(void) const
	{
		_pager.Bind();
		_page_storage.Bind();
	}

	TextureUnitSelector BitmapTexUnit(void) const
	{
		return _page_storage.BitmapTexUnit();
	}

	TextureUnitSelector MetricTexUnit(void) const
	{
		return _page_storage.MetricTexUnit();
	}

	TextureUnitSelector PageMapTexUnit(void) const
	{
		return _pager.PageMapTexUnit();
	}

	void LoadPages(const GLuint* pages, SizeType size)
	{
		assert(size < GLsizei(_pager.FrameCount()));
		_do_load_pages(pages, size);
	}

	GLfloat QueryXOffsets(
		const CodePoint* cps,
		SizeType size,
		std::vector<GLfloat>& x_offsets
	) const;

	Rectangle GetGlyphMetrics(CodePoint code_point, GLint offs) const;
};

} // namespace text
} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/text/stb_truetype/font_essence.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

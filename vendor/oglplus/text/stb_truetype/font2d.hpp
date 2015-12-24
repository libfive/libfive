/**
 *  @file oglplus/text/stb_truetype/font2d.hpp
 *  @brief Helper wrapper around the Sean Barrett's truetype rendering lib.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2011-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#ifndef __OGLPLUS_TEXT_STB_TRUETYPE_1102101236_HPP
#define __OGLPLUS_TEXT_STB_TRUETYPE_1102101236_HPP

#include <oglplus/config/compiler.hpp>
#include <oglplus/config/basic.hpp>
#include <oglplus/text/unicode.hpp>

#ifndef OGLPLUS_NO_STB_TRUETYPE
#include <stb_truetype.h>
#endif // OGLPLUS_NO_STB_TRUETYPE

#include <vector>
#include <istream>

namespace oglplus {
namespace text {

class STBTTFont2D;

/// Wrapper around the Sean Barrett's true type font glyph functionality
/**
 *  This class is instantiated by STBTTFont2D class
 */
class STBTTFont2DGlyph
{
private:
	friend class STBTTFont2D;

	const ::stbtt_fontinfo* _font;
	int _index;

	STBTTFont2DGlyph(
		const ::stbtt_fontinfo& font
	): _font(&font)
	 , _index(0)
	{ }

	void _init_index(CodePoint code_point)
	{
		_index = ::stbtt_FindGlyphIndex(_font, int(code_point));
	}

	STBTTFont2DGlyph(
		const ::stbtt_fontinfo& font,
		CodePoint code_point
	): _font(&font)
	{
		_init_index(code_point);
	}
public:
	/// Queries the horizontal glyph metrics
	void GetHMetrics(int& left_bearing, int& width) const
	{
		::stbtt_GetGlyphHMetrics(_font, _index, &width, &left_bearing);
	}

	/// Queries the vertical glyph metrics
	void GetVMetrics(int& ascent, int& descent, int& line_gap) const
	{
		::stbtt_GetFontVMetrics(_font, &ascent, &descent, &line_gap);
	}

	/// Queries the glyph bitmap box
	void GetBitmapBox(
		float xscale,
		float yscale,
		int& x0,
		int& y0,
		int& x1,
		int& y1
	) const
	{
		::stbtt_GetGlyphBitmapBox(
			_font,
			_index,
			xscale,
			yscale,
			&x0,
			&y0,
			&x1,
			&y1
		);
	}

	/// Queries the glyph bitmap box with subpixel precision
	void GetBitmapBoxSubpixel(
		float xscale,
		float yscale,
		float xoffs,
		float yoffs,
		int& x0,
		int& y0,
		int& x1,
		int& y1
	) const
	{
		::stbtt_GetGlyphBitmapBoxSubpixel(
			_font,
			_index,
			xscale,
			yscale,
			xoffs,
			yoffs,
			&x0,
			&y0,
			&x1,
			&y1
		);
	}

	/// Returns the left bearing of the glyph in pixels
	int LeftBearing(void) const
	{
		int result = 0;
		::stbtt_GetGlyphHMetrics(_font, _index, nullptr, &result);
		return result;
	}

	/// Returns the right bearing of the glyph in pixels
	int RightBearing(void) const
	{
		int w = 0, lb = 0;
		::stbtt_GetGlyphHMetrics(_font, _index, &w, &lb);
		return lb + w;
	}

	/// Returns the Width of the glyph in pixels
	int Width(void) const
	{
		int result = 0;
		::stbtt_GetGlyphHMetrics(_font, _index, &result, nullptr);
		return result;
	}

	/// Returns the ascent of the glyph in pixels
	int Ascent(void) const
	{
		int result = 0;
		::stbtt_GetFontVMetrics(_font, &result, nullptr, nullptr);
		return result;
	}

	/// Returns the descent of the glyph in pixels
	int Descent(void) const
	{
		int result = 0;
		::stbtt_GetFontVMetrics(_font, nullptr, &result, nullptr);
		return result;
	}

	/// Returns the height of the glyph in pixels
	int Height(void) const
	{
		int asc = 0, dsc = 0;
		::stbtt_GetFontVMetrics(_font, &asc, &dsc, nullptr);
		return asc - dsc;
	}

	/// Returns the line gap value in pixels
	int LineGap(void) const
	{
		int result = 0;
		::stbtt_GetFontVMetrics(_font, nullptr, nullptr, &result);
		return result;
	}

	/// Renders the glyph into a buffer at start with the specified parameters
	void Render(
		unsigned char* start,
		int frame_width,
		int frame_height,
		int stride,
		float scale
	) const
	{
		::stbtt_MakeGlyphBitmap(
			_font,
			start,
			frame_width,
			frame_height,
			stride,
			scale,
			scale,
			_index
		);
	}
};


/// Wrapper arund the Sean Barrett's true type font functionality
class STBTTFont2D
{
private:
	std::vector<unsigned char> _ttf_data;

	static std::vector<unsigned char> _load_ttf(std::istream& input);

	::stbtt_fontinfo _font;

	void _load_font(const unsigned char* ttf_buffer);
public:
	/// Creates a font from an open ttf input stream
	STBTTFont2D(std::istream&& input)
	 : _ttf_data(_load_ttf(input))
	{
		_load_font(_ttf_data.data());
	}

	/// Creates a font from an open ttf input stream
	STBTTFont2D(std::istream& input)
	 : _ttf_data(_load_ttf(input))
	{
		_load_font(_ttf_data.data());
	}

	/// Creates a font from an open ttf input data
	STBTTFont2D(std::vector<unsigned char>&& ttf_data)
	 : _ttf_data(std::move(ttf_data))
	{
		_load_font(_ttf_data.data());
	}

	/// A Glyph type
	typedef STBTTFont2DGlyph Glyph;

	/// Returns the glyph for the specified code point
	Glyph GetGlyph(CodePoint code_point) const
	{
		return Glyph(_font, code_point);
	}

	/// Gets the kerning advance for two glyphs
	static int KernAdvance(const Glyph& glyph1, const Glyph& glyph2)
	{
		assert(glyph1._font == glyph2._font);
		return ::stbtt_GetGlyphKernAdvance(
			glyph1._font,
			glyph1._index,
			glyph2._index
		);
	}

	/// Returns the scale for pixel height
	float ScaleForPixelHeight(float pixels) const
	{
		return ::stbtt_ScaleForPixelHeight(&_font, pixels);
	}

	/// A layout of glyphs from the specified code points
	typedef std::vector<Glyph> Layout;

	/// Makes a glyph sequence from code points
	Layout MakeLayout(
		const CodePoint* code_points,
		const std::size_t cp_n
	) const;

	/// Makes a glyph sequence from code points
	Layout MakeLayout(const CodePoints& code_points) const
	{
		return MakeLayout(code_points.data(), code_points.size());
	}

	/// Get the base line for the layout in pixels
	float Baseline(
		std::size_t size_in_pixels,
		const Layout& /*layout*/
	) const
	{
		int result = 0;
		::stbtt_GetFontVMetrics(&_font, &result, nullptr, nullptr);
		return result*ScaleForPixelHeight(float(size_in_pixels));
	}

	/// Get the height of the layout in pixels
	float Height(
		std::size_t size_in_pixels,
		const Layout& /*layout*/
	) const
	{
		int asc = 0, dsc = 0;
		::stbtt_GetFontVMetrics(&_font, &asc, &dsc, nullptr);
		return ( asc - dsc )*ScaleForPixelHeight(float(size_in_pixels));
	}

	/// Get the width of the layout in pixels
	float Width(
		std::size_t size_in_pixels,
		const Layout& layout
	) const;

	/// Render the specified text into a buffer
	void Render(
		std::size_t size_in_pixels,
		const Layout& layout,
		unsigned char* buffer_start,
		const std::size_t buffer_width,
		const std::size_t buffer_height,
		const int xposition = 0,
		const int yposition = 0
	) const;
};

} // namespace aux
} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/text/stb_truetype/font2d.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif

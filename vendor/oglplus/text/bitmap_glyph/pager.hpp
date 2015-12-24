/**
 *  @file oglplus/text/bitmap_glyph/pager.hpp
 *  @brief Bitmap-font-based text rendering, page swapping
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXT_BITMAP_GLYPH_PAGER_HPP
#define OGLPLUS_TEXT_BITMAP_GLYPH_PAGER_HPP

#include <oglplus/buffer.hpp>
#include <oglplus/text/unicode.hpp>
#include <oglplus/text/bitmap_glyph/fwd.hpp>

#include <vector>
#include <unordered_map>
#include <cassert>

namespace oglplus {
namespace text {

class BitmapGlyphPager
{
private:
	// reference to the parent rendering system
	BitmapGlyphRenderingBase& _parent;

	// the frames into which pages are loaded
	std::vector<GLint> _frames;

	// basic logical consistency check
	bool _is_ok(void) const
	{
		return (_frames.size() == _ages.size()) &&
			(_active_pages.size() <= _frames.size());
	}

	// checks if the values in _frames and _gpu_page_map
	// are consistent
	bool _frames_consistent(void) const;

	// checks if the specified page is in _frames
	bool _page_in_frames(GLuint page) const;

	typedef unsigned long age_t;

	static age_t _zero_age(void)
	{
		return age_t(0);
	}

	static age_t _full_age(void)
	{
		return ~_zero_age();
	}

	static age_t _init_age(void)
	{
		return ~(_full_age() >> 1);
	}

	std::vector<age_t> _ages;

	std::unordered_map<GLuint, GLuint> _active_pages;

	typedef GLubyte gpu_frame_t;

	static gpu_frame_t _invalid_gpu_frame(void)
	{
		return gpu_frame_t(~gpu_frame_t(0));
	}

	// notes frame usage
	void _touch_frame(std::size_t frame)
	{
		_ages[frame] >>= 1;
		_ages[frame] |= _init_age();
	}

	Buffer _gpu_page_map;
	TextureUnitSelector _pg_map_tex_unit;
	Texture _page_map_tex;

	// replaces the page in the specified frame
	// with a new one
	void _replace_page(GLuint frame, GLuint page);
public:
	BitmapGlyphPager(
		BitmapGlyphRenderingBase& parent,
		TextureUnitSelector pg_map_tex_unit,
		SizeType frame_count
	);

	void Bind(void) const
	{
		Texture::Active(_pg_map_tex_unit);
		_page_map_tex.Bind(Texture::Target::Buffer);
	}

	std::size_t FrameCount(void) const
	{
		assert(_is_ok());
		return _frames.size();
	}

	TextureUnitSelector PageMapTexUnit(void) const
	{
		return _pg_map_tex_unit;
	}

	void Update(void)
	{
		assert(_is_ok());
		for(auto i=_ages.begin(), e=_ages.end(); i!=e; ++i)
		{
			*i >>= 1;
		}
	}

	// finds the best frame for a new page
	GLuint FindFrame(void);

	// Checks if a page is available for usage
	bool UsePage(GLuint page);

	GLint FrameOfPage(GLuint page) const;

	// Swaps the specified page into a frame
	// Use only if the page is not already swapped in
	void SwapPageIn(GLuint frame, GLuint page)
	{
		assert(_is_ok());
		_replace_page(frame, page);
		assert(_frames_consistent());
	}
};

} // namespace text
} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/text/bitmap_glyph/pager.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

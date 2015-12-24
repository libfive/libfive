/**
 *  @file oglplus/text/bitmap_glyph/font_essence.hpp
 *  @brief Bitmap-font-based text rendering, font essence class
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXT_BITMAP_GLYPH_FONT_ESSENCE_HPP
#define OGLPLUS_TEXT_BITMAP_GLYPH_FONT_ESSENCE_HPP

#include <oglplus/config/basic.hpp>
#include <oglplus/text/bitmap_glyph/fwd.hpp>
#include <oglplus/text/bitmap_glyph/page_storage.hpp>
#include <oglplus/text/bitmap_glyph/pager.hpp>

#include <oglplus/utils/filesystem.hpp>
#include <oglplus/opt/resources.hpp>

namespace oglplus {
namespace text {

class BitmapGlyphFontEssence
{
private:
	BitmapGlyphRenderingBase& _parent;
	const std::string _font_name;

	oglplus::images::Image _load_page_bitmap(GLuint page);

	static void _check_input(std::istream& input);

	OGLPLUS_NORETURN
	static void _unexpected_char(char);

	static void _load_single_glyph(
		std::istream& input,
		char* line,
		const std::streamsize linelen,
		GLfloat* values,
		const size_t n_values
	);

	std::vector<GLfloat> _load_page_metric(GLuint page);

	BitmapGlyphPager _pager;
	const GLuint _initial_frame;
	BitmapGlyphPageStorage _page_storage;

	template <typename PageGetter, typename Element>
	void _do_load_pages(
		PageGetter get_page,
		const Element* elem,
		GLsizei size
	)
	{
		_page_storage.Bind();
		// go through the list of code-points
		for(GLsizei i=0; i!=size; ++i)
		{
			// get the page number for the glyph
			GLuint page = get_page(elem[i]);
			// check if the page is active
			if(!_pager.UsePage(page))
			{
				// if not let the pager find
				// a frame for the new page
				auto frame = _pager.FindFrame();
				// load the bitmap image
				_page_storage.LoadPage(
					frame,
					_load_page_bitmap(page),
					_load_page_metric(page)
				);
				// tell the pages that the page
				// is successfully loaded in the frame
				_pager.SwapPageIn(frame, page);
			}
		}
	}

	struct _page_to_page
	{
		GLuint operator()(GLuint page) const
		{
			return page;
		}
	};
public:
	BitmapGlyphFontEssence(
		BitmapGlyphRenderingBase& parent,
		TextureUnitSelector bitmap_tex_unit,
		TextureUnitSelector metric_tex_unit,
		TextureUnitSelector pg_map_tex_unit,
		const std::string& font_name,
		SizeType frames,
		GLuint default_page,
		GLuint /* pixel_height*/
	): _parent(parent)
	 , _font_name(font_name)
	 , _pager(
		parent,
		pg_map_tex_unit,
		frames
	), _initial_frame(_pager.FindFrame())
	 , _page_storage(
		parent,
		bitmap_tex_unit,
		metric_tex_unit,
		_initial_frame,
		frames,
		_load_page_bitmap(default_page),
		_load_page_metric(default_page)
	)
	{
		_pager.SwapPageIn(_initial_frame, default_page);
	}

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
		_do_load_pages(_page_to_page(), pages, size);
	}

	GLfloat QueryXOffsets(
		const CodePoint* cps,
		SizeType size,
		std::vector<GLfloat>& x_offsets
	) const;

	Rectangle GetGlyphMetrics(CodePoint code_point, GLuint offs) const;
};

} // namespace text
} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/text/bitmap_glyph/font_essence.ipp>
#endif

#endif // include guard

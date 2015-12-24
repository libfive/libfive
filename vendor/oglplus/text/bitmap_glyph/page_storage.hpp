/**
 *  @file oglplus/text/bitmap_glyph/page_storage.hpp
 *  @brief Bitmap-font-based text rendering, glyph page storage
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXT_BITMAP_GLYPH_PAGE_STORAGE_HPP
#define OGLPLUS_TEXT_BITMAP_GLYPH_PAGE_STORAGE_HPP

#include <oglplus/texture.hpp>
#include <oglplus/images/image.hpp>
#include <oglplus/text/common.hpp>
#include <oglplus/text/unicode.hpp>
#include <oglplus/text/bitmap_glyph/fwd.hpp>

#include <vector>

namespace oglplus {
namespace text {

// Stores the active glyph pages
// 1) the bitmap image
// 2) the metric values
class BitmapGlyphPageStorage
{
private:
	TextureUnitSelector _bitmap_tex_unit;
	TextureUnitSelector _metric_tex_unit;

	const PixelDataInternalFormat _internal_format;
	const GLsizei _width;
	const GLsizei _height;
	const GLsizei _frames;

	Texture _bitmap_storage;
	Texture _metric_storage;

	const GLuint _glyphs_per_page;
	const GLuint _vects_per_glyph;

	std::vector<std::vector<GLfloat>> _metrics;
public:
	BitmapGlyphPageStorage(
		BitmapGlyphRenderingBase& parent,
		TextureUnitSelector bitmap_tex_unit,
		TextureUnitSelector metric_tex_unit,
		const GLuint init_frame,
		const SizeType frames,
		const oglplus::images::Image& image,
		const std::vector<GLfloat>& metrics
	);

	TextureUnitSelector BitmapTexUnit(void) const
	{
		return _bitmap_tex_unit;
	}

	TextureUnitSelector MetricTexUnit(void) const
	{
		return _metric_tex_unit;
	}

	void Bind(void) const
	{
		Texture::Active(_bitmap_tex_unit);
		_bitmap_storage.Bind(Texture::Target::_2DArray);
		Texture::Active(_metric_tex_unit);
		_metric_storage.Bind(Texture::Target::Rectangle);
	}

	void LoadPage(
		const GLuint frame,
		const oglplus::images::Image& image,
		const std::vector<GLfloat>& metrics
	);

	void QueryGlyphMetrics(
		GLuint frame,
		GLuint cell,
		GLuint metric,
		GLuint count,
		GLfloat* result
	) const;

	GLfloat GetGlyphMetric(GLuint frame, GLuint cell, GLuint metric) const
	{
		return _metrics[frame][4*_vects_per_glyph*cell+metric];
	}

	GLfloat GetGlyphWidth(GLuint frame, GLuint offset) const
	{
		return	GetGlyphMetric(frame, offset, 1)-
			GetGlyphMetric(frame, offset, 0);
	}
};

} // namespace text
} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
# include <oglplus/text/bitmap_glyph/page_storage.ipp>
#endif

#endif // include guard

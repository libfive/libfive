/**
 *  @file oglplus/text/bitmap_glyph/font.hpp
 *  @brief Bitmap-font-based text rendering, font class
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXT_BITMAP_GLYPH_FONT_HPP
#define OGLPLUS_TEXT_BITMAP_GLYPH_FONT_HPP

#include <oglplus/texture_unit.hpp>
#include <oglplus/text/common.hpp>
#include <oglplus/text/bitmap_glyph/fwd.hpp>

#include <memory>

namespace oglplus {
namespace text {

template <class Essence>
class BitmapGlyphFontTpl
{
private:
	std::shared_ptr<Essence> _essence;

	friend class BitmapGlyphRenderer;
	friend class BitmapGlyphLayoutTpl<BitmapGlyphFontTpl>;
public:
	BitmapGlyphFontTpl(
		BitmapGlyphRenderingTpl<BitmapGlyphFontTpl>& parent,
		TextureUnitSelector bitmap_tex_unit,
		TextureUnitSelector metric_tex_unit,
		TextureUnitSelector pg_map_tex_unit,
		const std::string& name,
		SizeType frames,
		GLint default_page,
		GLuint pixel_height
#ifdef _MSC_VER
	): _essence(new Essence(
#else
	): _essence(std::make_shared<Essence>(
#endif
		parent,
		bitmap_tex_unit,
		metric_tex_unit,
		pg_map_tex_unit,
		name,
		frames,
		default_page,
		pixel_height
	))
	{ }

	GLfloat QueryXOffsets(
		const CodePoint* cps,
		SizeType size,
		std::vector<GLfloat>& x_offsets
	)
	{
		return _essence->QueryXOffsets(cps, size, x_offsets);
	}

	Rectangle GlyphLogicalMetrics(CodePoint cp)
	{
		return _essence->GetGlyphMetrics(cp, 0);
	}
};

} // namespace text
} // namespace oglplus

#endif // include guard

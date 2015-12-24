/**
 *  @file oglplus/text/bitmap_glyph/fwd.hpp
 *  @brief Bitmap-font-based text rendering, forward declarations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXT_BITMAP_GLYPH_FWD_HPP
#define OGLPLUS_TEXT_BITMAP_GLYPH_FWD_HPP

#include <oglplus/text/unicode.hpp>
#include <string>

namespace oglplus {
namespace text {

// Forward declaration of the bitmap glyph rendering utility
class BitmapGlyphRenderingBase;

template <class BitmapFont>
class BitmapGlyphRenderingTpl;

// Forward declaration of the renderer
class BitmapGlyphRenderer;

// Forward declarations of font essences
class BitmapGlyphFontEssence;
class STBTTFontEssence;

// Forward declaration of the font
template <typename Essence>
class BitmapGlyphFontTpl;

// Forward declaration of the layout
template <typename BitmapFont>
class BitmapGlyphLayoutTpl;

// Forward declaration of layout storage
class BitmapGlyphLayoutStorage;

// Forward declaration of layout data
struct BitmapGlyphLayoutData;

// Returns the number of glyphs per a font page
unsigned BitmapGlyphGlyphsPerPage(const BitmapGlyphRenderingBase&);

// Returns the number of font pages per a unicode plane
unsigned BitmapGlyphPagesPerPlane(const BitmapGlyphRenderingBase&);

// Returns the number of planes supported by the bitmap font rendering utility
// One plane consists of PagesPerPlane pages of GlyphsPerPage glyphs
unsigned BitmapGlyphPlaneCount(const BitmapGlyphRenderingBase&);

void BitmapGlyphAllocateLayoutData(
	BitmapGlyphRenderingBase& parent,
	BitmapGlyphLayoutData& layout_data
);
template <typename BitmapFont>
void BitmapGlyphInitializeLayoutData(
	BitmapGlyphRenderingBase& parent,
	BitmapGlyphLayoutData& layout_data,
	BitmapFont& font,
	const CodePoint* cps,
	SizeType length
);
void BitmapGlyphDeallocateLayoutData(
	BitmapGlyphRenderingBase& parent,
	BitmapGlyphLayoutData& layout_data
);

// Returns the page of the font containing the glyph for the specified codepoint
inline GLuint BitmapGlyphPageOfCP(
	const BitmapGlyphRenderingBase& parent,
	CodePoint code_point
)
{
	assert(BitmapGlyphGlyphsPerPage(parent) != 0);
	assert(BitmapGlyphGlyphsPerPage(parent) % 4 == 0);
	return GLuint(code_point / BitmapGlyphGlyphsPerPage(parent));
}

inline GLuint BitmapGlyphCellOfCP(
	const BitmapGlyphRenderingBase& parent,
	CodePoint code_point
)
{
	assert(BitmapGlyphGlyphsPerPage(parent) != 0);
	assert(BitmapGlyphGlyphsPerPage(parent) % 4 == 0);
	return GLuint(code_point % BitmapGlyphGlyphsPerPage(parent));
}

std::string BitmapGlyphPageName(
	const BitmapGlyphRenderingBase& parent,
	GLuint page
);

GLuint BitmapGlyphDefaultPageTexSide(
	const BitmapGlyphRenderingBase& parent,
	GLuint pixel_height
);

} // namespace text
} // namespace oglplus

#endif // include guard

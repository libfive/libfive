/**
 *  @file oglplus/text/bitmap_glyph/layout_storage.hpp
 *  @brief Bitmap-font-based text rendering, attribute storage for static layouts
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXT_BITMAP_GLYPH_LAYOUT_STORAGE_HPP
#define OGLPLUS_TEXT_BITMAP_GLYPH_LAYOUT_STORAGE_HPP

#include <oglplus/buffer.hpp>
#include <oglplus/vertex_array.hpp>
#include <oglplus/vertex_attrib.hpp>
#include <oglplus/text/bitmap_glyph/fwd.hpp>
#include <oglplus/text/bitmap_glyph/font.hpp>

#include <cassert>
#include <vector>

namespace oglplus {
namespace text {

struct BitmapGlyphLayoutData
{
	GLint _offset;
	GLsizei _length;
	GLsizei _capacity;
	BitmapGlyphLayoutStorage* _storage;
	GLfloat _width;

	BitmapGlyphLayoutData(SizeType capacity)
	 : _offset(-1)
	 , _length(0)
	 , _capacity(capacity)
	 , _storage(nullptr)
	 , _width(0.0f)
	{ }
};

// Manages the codepoints for layouts that remain static
class BitmapGlyphLayoutStorage
{
private:
	BitmapGlyphRenderingBase& _parent;
	GLuint _list_head;
	GLsizei _free;
	const GLsizei _capacity;
	const GLsizei _alloc_unit;

	static inline GLuint _list_nil(void)
	{
		return GLuint(-1);
	}

	VertexArray _vao;
	Buffer _code_points, _x_offsets;

	BitmapGlyphLayoutStorage(const BitmapGlyphLayoutStorage&);
	BitmapGlyphLayoutStorage(BitmapGlyphLayoutStorage&&);
public:
	BitmapGlyphLayoutStorage(
		BitmapGlyphRenderingBase& parent,
		SizeType capacity,
		SizeType alloc_unit = 4
	);

	void Use(void) const
	{
		_vao.Bind();
	}

	SizeType Capacity(void) const
	{
		return MakeSizeType(_capacity, std::nothrow);
	}

	SizeType Free(void) const
	{
		return MakeSizeType(_free, std::nothrow);
	}

	bool Empty(void) const
	{
		return _free == _capacity;
	}

	bool Allocate(BitmapGlyphLayoutData& layout_data);

	void Deallocate(BitmapGlyphLayoutData& layout_data);

	void Initialize(
		BitmapGlyphLayoutData& layout_data,
		GLfloat width,
		const std::vector<GLfloat>& x_offsets,
		const CodePoint* cps,
		SizeType length
	);
};

} // namespace text
} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
# include <oglplus/text/bitmap_glyph/layout_storage.ipp>
#endif

#endif // include guard

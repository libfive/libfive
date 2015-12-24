/**
 *  @file oglplus/text/pango_cairo.hpp
 *  @brief Pango/Cairo-based text rendering.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXT_PANGO_CAIRO_HPP
#define OGLPLUS_TEXT_PANGO_CAIRO_HPP

#include <oglplus/text/pango_cairo/font.hpp>
#include <oglplus/text/pango_cairo/layout.hpp>
#include <oglplus/text/pango_cairo/renderer.hpp>

#include <oglplus/texture.hpp>
#include <oglplus/size_type.hpp>

#include <vector>
#include <cassert>

namespace oglplus {
namespace text {

class PangoCairoRendering
{
private:
	TextureUnitSelector _main_tex_unit;

	friend void PangoCairoAllocateLayoutData(
		PangoCairoRendering& that,
		PangoCairoLayoutData& layout_data,
		SizeType width,
		SizeType height
	);

	friend void PangoCairoDeallocateLayoutData(
		PangoCairoRendering&,
		PangoCairoLayoutData&
	);

	friend void PangoCairoInitializeLayoutData(
		PangoCairoRendering& that,
		PangoCairoLayoutData& layout_data,
		SizeType width,
		SizeType height,
		const void* raw_data
	);

	friend TextureUnitSelector PangoCairoUseLayoutData(
		PangoCairoRendering& that,
		const PangoCairoLayoutData& layout_data
	);
public:
	PangoCairoRendering(TextureUnitSelector main_tex_unit)
	 : _main_tex_unit(main_tex_unit)
	{ }

	typedef PangoCairoFont Font;

	Font LoadFont(const char* font_name)
	{
		return Font(font_name);
	}

	typedef PangoCairoLayout Layout;

	Layout MakeLayout(const Font& font, SizeType capacity)
	{
		return Layout(*this, font, capacity);
	}

	Layout MakeLayout(const Font& font, StrCRef str)
	{
		CodePoints cps;
		UTF8ToCodePoints(str.begin(), str.size(), cps);

		Layout layout(MakeLayout(font, str.size()));
		layout.Set(cps.data(), cps.size());
		return std::move(layout);
	}

	typedef PangoCairoDefaultRenderer Renderer;

	Renderer GetRenderer(const FragmentShader& fragment_shader)
	{
		return Renderer(*this, fragment_shader);
	}
};

} // namespace text
} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/text/pango_cairo.ipp>
#endif

#endif // include guard

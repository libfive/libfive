/**
 *  @file oglplus/text/bitmap_glyph/renderer.hpp
 *  @brief Bitmap-font-based text rendering, renderer class
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXT_BITMAP_GLYPH_RENDERER_HPP
#define OGLPLUS_TEXT_BITMAP_GLYPH_RENDERER_HPP

#include <oglplus/shader.hpp>
#include <oglplus/program.hpp>
#include <oglplus/context.hpp>
#include <oglplus/config/basic.hpp>
#include <oglplus/object/group.hpp>
#include <oglplus/text/common.hpp>
#include <oglplus/text/bitmap_glyph/fwd.hpp>
#include <oglplus/text/bitmap_glyph/font.hpp>
#include <oglplus/text/bitmap_glyph/layout.hpp>

namespace oglplus {
namespace text {

class BitmapGlyphRenderer
{
private:
	BitmapGlyphRenderingBase& _parent;

	Program _program;

	ProgramUniform<GLint>
		_bitmap_sampler,
		_metric_sampler,
		_pg_map_sampler;

	ProgramUniform<GLfloat> _layout_width;
	bool _layout_width_active;

	const void* _prev_font_essence;

	template <typename BitmapFontEssence>
	void _use_font(const BitmapFontEssence& essence)
	{
		if(_prev_font_essence != static_cast<const void*>(&essence))
		{
			essence.Use();
			_bitmap_sampler.Set(GLint(essence.BitmapTexUnit()));
			_metric_sampler.Set(GLint(essence.MetricTexUnit()));
			_pg_map_sampler.Set(GLint(essence.PageMapTexUnit()));
			_prev_font_essence = &essence;
		}
	}

	const BitmapGlyphLayoutStorage* _prev_layout_storage;
	void _use_layout(const BitmapGlyphLayoutData& layout_data)
	{
		if(_prev_layout_storage != layout_data._storage)
		{
			assert(layout_data._storage);

			layout_data._storage->Use();
			_prev_layout_storage = layout_data._storage;
		}
	}
protected:
	const Program& _get_program(void) const
	{
		return _program;
	}
public:
	/*
	 *  @note the shaders group must contain a layout-transform-shader,
	 *  glyph-transform-shader and glyph-pixel-shader
	 */
	BitmapGlyphRenderer(
		BitmapGlyphRenderingBase& parent,
		const Sequence<ShaderName>& shaders
	);

#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	BitmapGlyphRenderer(BitmapGlyphRenderer&&) = default;
#else
	BitmapGlyphRenderer(BitmapGlyphRenderer&& tmp);
#endif

	void Use(void)
	{
		_program.Use();
	}

	template <typename T>
	ProgramUniform<T> GetUniform(const GLchar* name) const
	{
		return ProgramUniform<T>(_program, name);
	}

	template <class BitmapFont>
	void Render(const BitmapGlyphLayoutTpl<BitmapFont>& layout)
	{
		// we'll need the layout font's essence
		assert(layout._font._essence);
		// use the layout's font
		_use_font(*layout._font._essence);
		// use the layout's storage
		_use_layout(layout._data);

		// load the font pages referenced by the layout
		layout._font._essence->LoadPages(
			layout._pages.data(),
			layout._pages.size()
		);

		// set the Layout Width uniform value if necessary
		if(_layout_width_active)
			_layout_width.Set(layout._data._width);

		// draw the glyphs
		Context gl;
		gl.DrawArrays(
			PrimitiveType::Points,
			layout._data._offset,
			layout._data._length
		);
	}
};

class BitmapGlyphDefaultRenderer
 : public DefaultRendererTpl<BitmapGlyphRenderer>
{
public:
	BitmapGlyphDefaultRenderer(
		BitmapGlyphRenderingBase& parent,
		const FragmentShader& pixel_color_shader
	);

#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	BitmapGlyphDefaultRenderer(BitmapGlyphDefaultRenderer&&) = default;
#else
	BitmapGlyphDefaultRenderer(BitmapGlyphDefaultRenderer&& tmp);
#endif
};

} // namespace text
} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
# include <oglplus/text/bitmap_glyph/renderer.ipp>
#endif

#endif // include guard

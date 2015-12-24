/**
 *  @file oglplus/text/bitmap_glyph/rendering.hpp
 *  @brief Bitmap-font-based text rendering implementation.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXT_BITMAP_GLYPH_RENDERING_HPP
#define OGLPLUS_TEXT_BITMAP_GLYPH_RENDERING_HPP

#include <oglplus/config/basic.hpp>

#include <oglplus/text/bitmap_glyph/layout_storage.hpp>
#include <oglplus/text/bitmap_glyph/layout.hpp>
#include <oglplus/text/bitmap_glyph/renderer.hpp>

#include <oglplus/program.hpp>
#include <oglplus/uniform.hpp>

#include <vector>
#include <list>
#include <cassert>
#include <sstream>

namespace oglplus {

namespace text {

struct BitmapGlyphRenderingConfig
{
	/// Number of frames used for paging
	unsigned page_frames;

	/// Number of unicode planes that should be supported (1, 2, or 3)
	unsigned plane_count;

	/// number of glyph pages per unicode plane
	unsigned pages_per_plane;

	/// Number of glyphs per single page
	unsigned glyphs_per_page;

	/// Max number of glyphs in a single layout storage unit
	unsigned layout_storage_page;

	/// Minimal allocation unit for a layout storage unit
	unsigned layout_storage_unit;

	BitmapGlyphRenderingConfig(void)
	 : page_frames(8)
	 , plane_count(3)
	 , pages_per_plane(256)
	 , glyphs_per_page(256)
	 , layout_storage_page(1024)
	 , layout_storage_unit(4)
	{ }
};

class BitmapGlyphRenderingBase
{
protected:
	TextureUnitSelector _bitmap_tex_unit;
	TextureUnitSelector _metric_tex_unit;
	TextureUnitSelector _pg_map_tex_unit;

	const BitmapGlyphRenderingConfig _config;

	// the number of frames (texture images) that are used to store
	// active glyph pages for rendering
	friend unsigned BitmapGlyphPageFrames(const BitmapGlyphRenderingBase&);

	friend unsigned BitmapGlyphPlaneCount(const BitmapGlyphRenderingBase&);


	friend unsigned BitmapGlyphPagesPerPlane(const BitmapGlyphRenderingBase&);


	friend unsigned BitmapGlyphGlyphsPerPage(const BitmapGlyphRenderingBase&);

	std::list<BitmapGlyphLayoutStorage> _layout_storage;

	friend void BitmapGlyphAllocateLayoutData(
		BitmapGlyphRenderingBase& that,
		BitmapGlyphLayoutData& layout_data
	);

	template <typename BitmapFont>
	friend void BitmapGlyphInitializeLayoutData(
		BitmapGlyphRenderingBase& that,
		BitmapGlyphLayoutData& layout_data,
		BitmapFont& font,
		const CodePoint* cps,
		SizeType length
	);

	friend void BitmapGlyphDeallocateLayoutData(
		BitmapGlyphRenderingBase& that,
		BitmapGlyphLayoutData& layout_data
	);

	BitmapGlyphRenderingBase(
		TextureUnitSelector bitmap_tex_unit,
		TextureUnitSelector metric_tex_unit,
		TextureUnitSelector pg_map_tex_unit,
		const BitmapGlyphRenderingConfig& config
	): _bitmap_tex_unit(bitmap_tex_unit)
	 , _metric_tex_unit(metric_tex_unit)
	 , _pg_map_tex_unit(pg_map_tex_unit)
	 , _config(config)
	{ }

public:

#if !OGLPLUS_NO_DELETED_FUNCTIONS
	BitmapGlyphRenderingBase(const BitmapGlyphRenderingBase&) = delete;
#else
private:
	BitmapGlyphRenderingBase(const BitmapGlyphRenderingBase&);
public:
#endif

#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	BitmapGlyphRenderingBase(BitmapGlyphRenderingBase&&) = default;
#else
	BitmapGlyphRenderingBase(BitmapGlyphRenderingBase&& tmp)
	 : _bitmap_tex_unit(std::move(tmp._bitmap_tex_unit))
	 , _metric_tex_unit(std::move(tmp._metric_tex_unit))
	 , _pg_map_tex_unit(std::move(tmp._pg_map_tex_unit))
	 , _config(std::move(tmp._config))
	 , _layout_storage(std::move(tmp._layout_storage))
	{ }
#endif


	typedef BitmapGlyphRenderingConfig Config;
	typedef BitmapGlyphRenderer CustomRenderer;

	CustomRenderer GetRenderer(
		const GeometryShader& layout_transform_shader,
		const GeometryShader& glyph_transform_shader,
		const FragmentShader& pixel_color_shader
	)
	{
		return CustomRenderer(
			*this,
			StaticGroup<ShaderName, 3>(
				layout_transform_shader,
				glyph_transform_shader,
				pixel_color_shader
			)
		);
	}

	typedef BitmapGlyphDefaultRenderer Renderer;

	Renderer GetRenderer(const FragmentShader& pixel_color_shader)
	{
		return Renderer(*this, pixel_color_shader);
	}
};

inline unsigned BitmapGlyphPageFrames(const BitmapGlyphRenderingBase& that)
{
	return that._config.page_frames;
}


inline unsigned BitmapGlyphPlaneCount(const BitmapGlyphRenderingBase& that)
{
	return that._config.plane_count;
}


inline unsigned BitmapGlyphPagesPerPlane(const BitmapGlyphRenderingBase& that)
{
	return that._config.pages_per_plane;
}


inline unsigned BitmapGlyphGlyphsPerPage(const BitmapGlyphRenderingBase& that)
{
	return that._config.glyphs_per_page;
}

inline void BitmapGlyphAllocateLayoutData(
	BitmapGlyphRenderingBase& that,
	BitmapGlyphLayoutData& layout_data
)
{
	auto	i = that._layout_storage.begin(),
		e = that._layout_storage.end();
	while(i != e)
	{
		if(i->Allocate(layout_data)) return;
		++i;
	}
	that._layout_storage.emplace_back(
		that,
		that._config.layout_storage_page,
		that._config.layout_storage_unit
	);
	that._layout_storage.back().Allocate(layout_data);
}

template <typename BitmapFont>
inline void BitmapGlyphInitializeLayoutData(
	BitmapGlyphRenderingBase& that,
	BitmapGlyphLayoutData& layout_data,
	BitmapFont& font,
	const CodePoint* cps,
	SizeType length
)
{
	OGLPLUS_FAKE_USE(that);
	assert(layout_data._storage);
	BitmapGlyphLayoutStorage& _storage = *layout_data._storage;
	std::vector<GLfloat> x_offsets;
	GLfloat width = font.QueryXOffsets(cps, length, x_offsets);
	_storage.Initialize(
		layout_data,
		width,
		x_offsets,
		cps,
		length
	);
}

inline void BitmapGlyphDeallocateLayoutData(
	BitmapGlyphRenderingBase& /*that*/,
	BitmapGlyphLayoutData& layout_data
)
{
	assert(layout_data._storage);
	BitmapGlyphLayoutStorage& _storage = *layout_data._storage;
	_storage.Deallocate(layout_data);
}


template <class BitmapFont>
class BitmapGlyphRenderingTpl
 : public BitmapGlyphRenderingBase
{
public:
	BitmapGlyphRenderingTpl(
		TextureUnitSelector bitmap_tex_unit,
		TextureUnitSelector metric_tex_unit,
		TextureUnitSelector pg_map_tex_unit,
		const Config& config = Config()
	): BitmapGlyphRenderingBase(
		bitmap_tex_unit,
		metric_tex_unit,
		pg_map_tex_unit,
		config
	)
	{ }

#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	BitmapGlyphRenderingTpl(BitmapGlyphRenderingTpl&&) = default;
 #else
	BitmapGlyphRenderingTpl(BitmapGlyphRenderingTpl&& tmp)
	 : BitmapGlyphRenderingBase(static_cast<BitmapGlyphRenderingBase&&>(tmp))
	{ }
 #endif

	typedef BitmapFont Font;

	Font LoadFont(
		const std::string& font_name,
		TextureUnitSelector bitmap_tex_unit,
		TextureUnitSelector metric_tex_unit,
		TextureUnitSelector pg_map_tex_unit,
		SizeType frames,
		GLint page,
		GLuint pixel_height
	)
	{
		return Font(
			*this,
			bitmap_tex_unit,
			metric_tex_unit,
			pg_map_tex_unit,
			font_name,
			frames,
			page,
			pixel_height
		);
	}

	Font LoadFont(const std::string& font_name)
	{
		return LoadFont(
			font_name,
			_bitmap_tex_unit,
			_metric_tex_unit,
			_pg_map_tex_unit,
			BitmapGlyphPageFrames(*this),
			0,
			64
		);
	}

	typedef BitmapGlyphLayoutTpl<BitmapFont> Layout;

	Layout MakeLayout(const Font& font, SizeType max_len)
	{
		return Layout(*this, font, max_len);
	}

	Layout MakeLayout(const Font& font, StrCRef str)
	{
		CodePoints cps;
		UTF8ToCodePoints(str.begin(), str.size(), cps);

		Layout layout(MakeLayout(font, cps.size()));
		layout.Set(cps);
		return std::move(layout);
	}
};


} // namespace text
} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
# include <oglplus/text/bitmap_glyph/rendering.ipp>
#endif

#endif // include guard

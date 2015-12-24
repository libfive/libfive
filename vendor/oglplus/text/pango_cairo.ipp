/**
 *  @file oglplus/text/pango_cairo.ipp
 *  @brief Implementation of pango/cairo-based text rendering
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */


namespace oglplus {
namespace text {

OGLPLUS_LIB_FUNC
void PangoCairoAllocateLayoutData(
	PangoCairoRendering& that,
	PangoCairoLayoutData& layout_data,
	SizeType width,
	SizeType height
)
{
	// TODO: some smarter form of allocation
	// or use bindless textures where available
	Texture::Active(that._main_tex_unit);
	layout_data._storage.Bind(Texture::Target::Rectangle);
	Texture::Image2D(
		Texture::Target::Rectangle,
		0,
		PixelDataInternalFormat::Red,
		width,
		height,
		0,
		PixelDataFormat::Red,
		PixelDataType::UnsignedByte,
		nullptr
	);
}


OGLPLUS_LIB_FUNC
void PangoCairoDeallocateLayoutData(
	PangoCairoRendering&,
	PangoCairoLayoutData&
)
{
}

OGLPLUS_LIB_FUNC
void PangoCairoInitializeLayoutData(
	PangoCairoRendering& that,
	PangoCairoLayoutData& layout_data,
	SizeType width,
	SizeType height,
	const void* raw_data
)
{
	Texture::Active(that._main_tex_unit);
	layout_data._storage.Bind(Texture::Target::Rectangle);
	Texture::SubImage2D(
		Texture::Target::Rectangle,
		0,
		0, 0,
		width,
		height,
		PixelDataFormat::Red,
		PixelDataType::UnsignedByte,
		raw_data
	);
}

OGLPLUS_LIB_FUNC
TextureUnitSelector PangoCairoUseLayoutData(
	PangoCairoRendering& that,
	const PangoCairoLayoutData& layout_data
)
{
	Texture::Active(that._main_tex_unit);
	layout_data._storage.Bind(Texture::Target::Rectangle);
	return that._main_tex_unit;
}

} // namespace text
} // namespace oglplus


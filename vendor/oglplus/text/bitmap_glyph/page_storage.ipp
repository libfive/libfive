/**
 *  @file oglplus/text/bitmap_glyph/page_storage.ipp
 *  @brief Implementation of Bitmap-font-based text rendering glyph page storage
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
BitmapGlyphPageStorage::BitmapGlyphPageStorage(
	BitmapGlyphRenderingBase& parent,
	TextureUnitSelector bitmap_tex_unit,
	TextureUnitSelector metric_tex_unit,
	const GLuint init_frame,
	const SizeType frames,
	const oglplus::images::Image& image,
	const std::vector<GLfloat>& metrics
): _bitmap_tex_unit(bitmap_tex_unit)
 , _metric_tex_unit(metric_tex_unit)
 , _internal_format(image.InternalFormat())
 , _width(image.Width())
 , _height(image.Height())
 , _frames(frames)
 , _glyphs_per_page(BitmapGlyphGlyphsPerPage(parent))
 , _vects_per_glyph(3)
 , _metrics(std::size_t(frames))
{
	//
	// setup the bitmap texture
	// TODO: replace with Texture::Storage3D
	Texture::Active(_bitmap_tex_unit);
	_bitmap_storage.Bind(Texture::Target::_2DArray);
	Texture::Image3D(
		Texture::Target::_2DArray,
		0,
		_internal_format,
		_width,
		_height,
		_frames,
		0,
		image.Format(),
		image.Type(),
		nullptr
	);
	Texture::MinFilter(
		Texture::Target::_2DArray,
		TextureMinFilter::LinearMipmapLinear
	);
	Texture::MagFilter(
		Texture::Target::_2DArray,
		TextureMagFilter::Linear
	);
	Texture::WrapS(
		Texture::Target::_2DArray,
		TextureWrap::ClampToBorder
	);
	Texture::WrapT(
		Texture::Target::_2DArray,
		TextureWrap::ClampToBorder
	);
	//
	// setup the metric texture
	//
	// TODO: replace with Texture::Storage2D
	Texture::Active(_metric_tex_unit);
	_metric_storage.Bind(Texture::Target::Rectangle);
	Texture::Image2D(
		Texture::Target::Rectangle,
		0,
		PixelDataInternalFormat::RGBA32F,
		GLsizei(_glyphs_per_page*_vects_per_glyph),
		_frames,
		0,
		PixelDataFormat::RGBA,
		PixelDataType::Float,
		nullptr
	);
	Texture::MinFilter(
		Texture::Target::Rectangle,
		TextureMinFilter::Nearest
	);
	Texture::MagFilter(
		Texture::Target::Rectangle,
		TextureMagFilter::Nearest
	);
	// load the initial data
	LoadPage(init_frame, image, metrics);
}

OGLPLUS_LIB_FUNC
void BitmapGlyphPageStorage::LoadPage(
	const GLuint frame,
	const oglplus::images::Image& image,
	const std::vector<GLfloat>& metrics
)
{
	// TODO add a parameter indicating how many rows
	// of the image are really used and add InvalidateTexImage
	assert(image.Width() == _width);
	assert(image.Height() == _height);
	// load the bitmap image
	Texture::Active(_bitmap_tex_unit);
	Texture::SubImage3D(
		Texture::Target::_2DArray,
		0,
		0, 0, GLint(frame),
		_width,
		_height,
		1,
		image.Format(),
		image.Type(),
		image.RawData()
	);
	//
	Texture::GenerateMipmap(Texture::Target::_2DArray);

	// load the metric values
	Texture::Active(_metric_tex_unit);
	Texture::SubImage2D(
		Texture::Target::Rectangle,
		0,
		0, GLint(frame),
		GLsizei(_glyphs_per_page*_vects_per_glyph), 1,
		PixelDataFormat::RGBA,
		PixelDataType::Float,
		metrics.data()
	);
	_metrics[frame] = metrics;
}

OGLPLUS_LIB_FUNC
void BitmapGlyphPageStorage::QueryGlyphMetrics(
	GLuint frame,
	GLuint cell,
	GLuint metric,
	GLuint count,
	GLfloat* result
) const
{
	const std::vector<GLfloat>& frame_data = _metrics[frame];
	const GLuint offs = 4*_vects_per_glyph*cell+metric;

	for(GLuint i=0; i!=count; ++i)
	{
		result[i] = frame_data[offs+i];
	}
}

} // namespace text
} // namespace oglplus


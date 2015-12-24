/**
 *  @file oglplus/text/stb_truetype/font_essence.ipp
 *  @brief Bitmap font essence class using Sean Barrett's truetype text library
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
void STBTTFontEssence::_do_make_page_bitmap_and_metric(
	GLuint page,
	unsigned char* bmp_data,
	float* metric
) const
{
	float scale = _tt_font.ScaleForPixelHeight(float(_font_resolution));

	const GLuint px = _font_resolution;
	const GLuint ts = _tex_side;
	const float inv_px = 1.0f/float(px);
	const float inv_ts = 1.0f/float(ts);

	std::vector<unsigned char> tmp[2];
	tmp[0].resize(px*px, 0x00);
	tmp[1].resize(px*px, 0x00);

	unsigned glyphs_per_page = BitmapGlyphGlyphsPerPage(_parent);

	int xoffs = 0, oldxoffs = 0;
	int yoffs = 0;
	int row_height = 0;
	int x0=0, y0=0, x1=1, y1=1, lb=0, width=1, asc=0, dsc=0, lg=0;
	for(unsigned g=0; g!=glyphs_per_page; ++g)
	{
		CodePoint code_point = CodePoint(glyphs_per_page*page+g);
		auto glyph = _tt_font.GetGlyph(code_point);

		if(std::isspace(int(code_point)))
		{
			tmp[g%2]=std::vector<unsigned char>(px*px,0x00);
		}
		glyph.Render(tmp[g%2].data(), int(px), int(px), int(px), scale);

		// if this glyph is not the same as the previous
		if((g == 0) || (tmp[0] != tmp[1]))
		{
			glyph.GetBitmapBox(1, 1, x0, y0, x1, y1);
			glyph.GetVMetrics(asc, dsc, lg);
			glyph.GetHMetrics(lb, width);

			int advance = int((x1-x0)*scale)+2;
			int glyph_height = int((y1-y0)*scale)+2;

			if(row_height < glyph_height)
				row_height = glyph_height;

			if(xoffs+advance >= int(ts))
			{
				xoffs = 0;
				yoffs += row_height;
				row_height = 0;
			}

			if(bmp_data)
			{
				glyph.Render(
					bmp_data+int(ts)*yoffs+xoffs,
					advance,
					int(px),
					int(ts),
					scale
				);
			}
			oldxoffs = xoffs;
			xoffs += advance;
		}
		if(metric)
		{
			float* p = metric+12*g;
			// logical rectangle metrics
			p[ 0] = lb*scale*inv_px;
			p[ 1] = (lb+width)*scale*inv_px;
			p[ 2] = asc*scale*inv_px;
			p[ 3] = -dsc*scale*inv_px;
			// ink rectangle metrics
			p[ 4] = x0*scale*inv_px;
			p[ 5] = x1*scale*inv_px;
			p[ 6] =-y0*scale*inv_px;
			p[ 7] = y1*scale*inv_px;
			// texture-space rectangle
			p[ 8] = (oldxoffs)*inv_ts;
			p[ 9] = (yoffs-y0*scale)*inv_ts;
			p[10] = ((x1-x0)*scale+1)*inv_ts;
			p[11] = ((y0-y1)*scale+1)*inv_ts;
		}
	}
	// TODO: hint that only the upper part of the image is actually
	// used + InvalidateTexImage
	// or increase the px value to get better resolution
}

OGLPLUS_LIB_FUNC
void STBTTFontEssence::_do_load_pages(
	const GLuint* elem,
	GLsizei size
)
{
	_page_storage.Bind();
	// go through the list of code-points
	for(GLsizei i=0; i!=size; ++i)
	{
		// get the page number for the glyph
		GLuint page = elem[i];
		// check if the page is active
		if(!_pager.UsePage(page))
		{
			// if not let the pager find
			// a frame for the new page
			auto frame = _pager.FindFrame();
			// load the bitmap image
			unsigned glyphs_per_page =
				BitmapGlyphGlyphsPerPage(_parent);
			std::vector<unsigned char>
				bmp(_tex_side*_tex_side);
			std::vector<GLfloat>
				metrics(glyphs_per_page*12);

			_do_make_page_bitmap_and_metric(
				page,
				bmp.data(),
				metrics.data()
			);

			_page_storage.LoadPage(
				frame,
				images::Image(
					_tex_side,
					_tex_side,
					1,
					1,
					bmp.data(),
					PixelDataFormat::Red,
					PixelDataInternalFormat::R8
				),
				metrics
			);
			// tell the pager that the page
			// is successfully loaded in the frame
			_pager.SwapPageIn(frame, page);
		}
	}
}

OGLPLUS_LIB_FUNC
STBTTFontEssence::STBTTFontEssence(
	BitmapGlyphRenderingBase& parent,
	TextureUnitSelector bitmap_tex_unit,
	TextureUnitSelector metric_tex_unit,
	TextureUnitSelector pg_map_tex_unit,
	const std::string& font_name,
	SizeType frames,
	GLuint default_page,
	GLuint pixel_height
): _parent(parent)
 , _tt_font(ResourceFile("fonts", font_name, ".ttf"))
 , _font_resolution(pixel_height)
 , _tex_side(BitmapGlyphDefaultPageTexSide(parent, _font_resolution))
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
	_make_page_bitmap(default_page),
	_make_page_metric(default_page)
)
{
	_pager.SwapPageIn(_initial_frame, default_page);
}

OGLPLUS_LIB_FUNC
GLfloat STBTTFontEssence::QueryXOffsets(
	const CodePoint* cps,
	SizeType size,
	std::vector<GLfloat>& x_offsets
) const
{
	if(size <= 0) return 0.0f;
	x_offsets.resize(size);

	float scale = _tt_font.ScaleForPixelHeight(float(_font_resolution));
	scale /= _font_resolution;
	STBTTFont2DGlyph g = _tt_font.GetGlyph(cps[0]);

	GLfloat sum = g.LeftBearing()*scale;
	x_offsets[0] = sum;


	GLsizei i = 0;
	while(++i != size)
	{
		sum += g.Width()*scale;

		STBTTFont2DGlyph pg = g;
		g = _tt_font.GetGlyph(cps[i]);

		sum += STBTTFont2D::KernAdvance(pg, g)*scale;

		x_offsets[std::size_t(i)] = sum;
	}
	return sum + g.Width()*scale;
}

OGLPLUS_LIB_FUNC
Rectangle STBTTFontEssence::GetGlyphMetrics(CodePoint code_point, GLint offs) const
{
	int asc, dsc, lg, lb, wi;
	auto glyph = _tt_font.GetGlyph(code_point);
	glyph.GetVMetrics(asc, dsc, lg);
	glyph.GetHMetrics(lb, wi);
	float scale = _tt_font.ScaleForPixelHeight(float(_font_resolution));

	assert(offs % 4 == 0);
	// TODO other metrics
	return Rectangle(lb*scale, (lb+wi)*scale, dsc*scale, asc*scale);
}

} // namespace text
} // namespace oglplus


/**
 *  @file oglplus/text/stb_truetype/font2d.ipp
 *  @brief Implementation of STBTTFFont2D
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2011-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <algorithm>
#include <stdexcept>

#ifndef OGLPLUS_NO_STB_TRUETYPE
#define STB_TRUETYPE_IMPLEMENTATION
#include <stb_truetype.h>
#endif // OGLPLUS_NO_STB_TRUETYPE

namespace oglplus {
namespace text {

OGLPLUS_LIB_FUNC
void STBTTFont2D::_load_font(const unsigned char* ttf_buffer)
{
	if(!::stbtt_InitFont(
		&_font,
		ttf_buffer,
		::stbtt_GetFontOffsetForIndex(ttf_buffer, 0)
	)) throw std::runtime_error("Error loading true type font");
}

OGLPLUS_LIB_FUNC
std::vector<unsigned char> STBTTFont2D::_load_ttf(std::istream& input)
{
	if(!input.good())
	{
		throw std::runtime_error("Error reading TTF input");
	}
	input.seekg(0, std::ios_base::end);
	std::streampos size = input.tellg();
	input.seekg(0, std::ios_base::beg);
	std::vector<unsigned char> result(std::size_t(size), 0x00);
	if(input.read(
		reinterpret_cast<char*>(result.data()),
		size
	).gcount() != size)
	{
		throw std::runtime_error("Error reading TTF input");
	}
	return std::move(result);
}

OGLPLUS_LIB_FUNC
STBTTFont2D::Layout STBTTFont2D::MakeLayout(
	const CodePoint* code_points,
	const std::size_t cp_n
) const
{
	Layout layout(cp_n, Glyph(this->_font));
	for(std::size_t cp=0; cp!=cp_n; ++cp)
	{
		layout[cp]._init_index(code_points[cp]);
	}
	return layout;
}

OGLPLUS_LIB_FUNC
float STBTTFont2D::Width(
	std::size_t size_in_pixels,
	const Layout& layout
) const
{
	float scale = ScaleForPixelHeight(float(size_in_pixels));
	float width = 0.0f;
	for(auto i=layout.begin(), p=i, e=layout.end(); i!=e; p = i, ++i)
	{
		if(p != i) width += KernAdvance(*p, *i);
		width += i->Width();
	}
	return width*scale;
}

OGLPLUS_LIB_FUNC
void STBTTFont2D::Render(
	std::size_t size_in_pixels,
	const STBTTFont2D::Layout& layout,
	unsigned char* buffer_start,
	const std::size_t buffer_width,
	const std::size_t buffer_height,
	const int xposition,
	const int yposition
) const
{
	if(int(buffer_height)  <   yposition) return;
	if(int(size_in_pixels) <= -yposition) return;

	std::vector<unsigned char> tmp_buffer;
	int tmp_height = int(size_in_pixels);
	int tmp_width = 0;

	float scale = ScaleForPixelHeight(float(size_in_pixels));

	float xoffset = 0.0f;
	for(auto i=layout.begin(), p=i, e=layout.end(); i!=e; ++i)
	{
		const int xo = int(std::floor(xoffset))+xposition;
		const float advance = i->Width()*scale;

		int width_in_pixels = int(std::ceil(advance));

		if(xo >= int(buffer_width))
		{
			break;
		}
		if(xo+width_in_pixels < 0)
		{
			xoffset += advance;
			continue;
		}

		if(tmp_width < width_in_pixels)
		{
			tmp_width = width_in_pixels;
			tmp_buffer.resize(std::size_t(tmp_width*tmp_height));
		}
		std::fill(tmp_buffer.begin(), tmp_buffer.end(), 0x00);

		if(p != i) xoffset += KernAdvance(*p, *i)*scale;
		const float xshift = xoffset - std::floor(xoffset);
		int x0, y0, x1, y1;
		i->GetBitmapBoxSubpixel(
			scale, scale,
			xshift, 0,
			x0, y0,
			x1, y1
		);
		const float yshift = std::floor((i->Ascent()*scale+y0));

		::stbtt_MakeGlyphBitmapSubpixel(
			&_font,
			tmp_buffer.data(),
			tmp_width,
			tmp_height,
			tmp_width,
			scale,
			scale,
			xshift,
			yshift,
			i->_index
		);

		const int yo = yposition;

		int gb = xo<0?-xo:0;
		int gw = int(gb+1+(x1-x0));
		int bwxo = int(buffer_width)-xo;

		if(gw > tmp_width)
		{
			gw = tmp_width;
		}
		if(gw > bwxo)
		{
			gw = bwxo;
		}

		int gy = int(std::floor(yshift));

		if(gy < -yo)
		{
			gy = -yo;
		}

		int gh = tmp_height;
		int bhyo = int(buffer_height)-yo;

		if(gh > bhyo)
		{
			gh = bhyo;
		}

		while(gy < gh)
		{
			int gx = gb;
			while(gx < gw)
			{
				int si = int(gy*tmp_width+gx);
				unsigned src = tmp_buffer[std::size_t(si)];
				if(src != 0)
				{
					int di = (gy+yo)*int(buffer_width)+gx+xo+x0;

					unsigned dst = buffer_start[std::size_t(di)]+src;

					if(dst > 0xFF)
					{
						dst = 0xFF;
					}
					buffer_start[std::size_t(di)] = dst & 0xFF;
				}
				++gx;
			}
			++gy;
		}

		p = i;
		xoffset += advance;
	}
}

} // namespace aux
} // namespace oglplus


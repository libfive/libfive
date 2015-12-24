/**
 *  @file oglplus/images/xpm.ipp
 *  @brief Implementation if XPM image loader
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2013 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <stdexcept>
#include <map>
#include <array>
#include <string>
#include <sstream>
#include <cstring>
#include <cassert>

namespace oglplus {
namespace images {
namespace aux {

OGLPLUS_LIB_FUNC
bool xpm_load_is_header_line(
	const char* line,
	const std::size_t size,
	std::size_t &/*width*/,
	std::size_t &/*height*/,
	std::size_t &depth
)
{
	if(std::strncmp(line, "! XPM2", size<6?size:6) == 0)
	{
		depth = 1;
		return true;
	}
	if(std::strncmp(line, "! XPM3D", size<7?size:7) == 0)
	{
		depth = 0;
		return true;
	}
	return false;
}

OGLPLUS_LIB_FUNC
bool xpm_load_is_dims_line(const char* line, const std::size_t line_len)
{
	for(std::size_t i=0; i!=line_len; ++i)
	{
		char c = line[i];
		if(c >= '0' && c <= '9') continue;
		if(c == ' ') continue;
		if(c == '\t') continue;
		return false;
	}
	return true;
}

OGLPLUS_LIB_FUNC
bool xpm_load_parse_dims_line(
	const char* line,
	const std::size_t line_len,
	std::size_t& width,
	std::size_t& height,
	std::size_t& depth,
	std::size_t& colors,
	std::size_t& chpp
)
{
	std::stringstream ss(std::ios::in);
	ss.str(std::string(line, line_len));
	if(!width && (!(ss >> width).good() || width == 0)) return false;
	if(!height && (!(ss >> height).good() || height == 0)) return false;
	if(!depth && (!(ss >> depth).good() || depth == 0)) return false;
	if(!colors && (!(ss >> colors).good() || colors == 0)) return false;
	if(!chpp && (!(ss >> chpp).good() || chpp == 0)) return false;
	return true;
}

OGLPLUS_LIB_FUNC
bool xpm_load_parse_palette_entry(
	const char* line,
	const std::size_t line_len,
	const std::size_t chpp,
	std::string& color_code,
	std::string& color_value
)
{
	if(line_len == 0) return false;
	color_code.clear();
	std::size_t todo = chpp;
	std::size_t i=0;
	while((i != line_len) && (todo-- != 0))
	{
		color_code.append(1, line[i++]);
	}

	while((i != line_len) && ((line[i] == ' ') || (line[i] == '\t'))) ++i;
	color_value.assign(line+i, line_len-i);
	return (!color_code.empty()) && (!color_value.empty());
}

OGLPLUS_LIB_FUNC
std::size_t xpm_load_color_code_bipp(
	const std::string& color,
	const std::map<std::string, std::string>& color_names
)
{
	if(color.empty()) return 0;

	if(color.front() == '#')
	{
		return (color.size()-1)*4;
	}
	else
	{
		auto p = color_names.find(color);
		if(p != color_names.end())
		{
			return xpm_load_color_code_bipp(
				p->second,
				color_names
			);
		}
		else
		{
			throw std::runtime_error(
				"Unknown color name '"+
				color +
				"' in XPM palette"
			);
		}
	}
	return 0;
}

OGLPLUS_LIB_FUNC
void xpm_load_preload_col_ents(
	const std::map<std::string, std::string>& col_ents,
	const std::map<std::string, std::string>& color_names,
	std::size_t& chpp,
	std::size_t& bipp
)
{
	for(auto i = col_ents.begin(); i != col_ents.end(); ++i)
	{
		assert(!i->second.empty());

		if(chpp < i->first.size())
		{
			chpp = i->first.size();
		}
		const std::string& val = i->second;
		std::size_t ebipp = 0;
		std::size_t j = 0, n = val.size();
		while(j != n)
		{
			if(!val[j]) break;

			++j;
			while((j != n) && ((val[j] == ' ') || (val[j] == '\t')))
			{
				++j;
			}
			std::size_t v = j;
			while((j != n) && ((val[j] != ' ') && (val[j] != '\t') && val[j]))
			{
				++j;
			}
			if(v == j)
			{
				throw std::runtime_error(
					"Missing color name in XPM color entry"
				);
			}
			std::size_t cbipp = xpm_load_color_code_bipp(
				std::string(val.data()+v, val.data()+j),
				color_names
			);
			if(ebipp < cbipp)
			{
				ebipp = cbipp;
			}

			while((j != n) && ((val[j] == ' ') || (val[j] == '\t')))
			{
				++j;
			}
		}
		if(ebipp == 0)
		{
			throw std::runtime_error(
				"Failed to determine bits per pixel for XPM color entry"
			);
		}
		if(bipp < ebipp)
		{
			bipp = ebipp;
		}
	}
}

OGLPLUS_LIB_FUNC
unsigned char xpm_load_convert_hex_digit(const char c)
{
	if((c >= '0') && (c <= '9'))
		return static_cast<unsigned char>(c-'0');
	if((c >= 'a') && (c <= 'f'))
		return static_cast<unsigned char>(c-'a'+10);
	if((c >= 'A') && (c <= 'F'))
		return static_cast<unsigned char>(c-'A'+10);
	else throw std::runtime_error(
		"Invalid hexadecimal digit in XPM palette"
	);
	return 0;
}

OGLPLUS_LIB_FUNC
unsigned char xpm_load_convert_color_channel(const char*& begin, const char* end)
{
	if(end - begin < 2)
	{
		throw std::runtime_error(
			"Too few hex digits for color value '" +
			std::string(begin, end) +
			"' in XPM palette"
		);
	}
	unsigned char up = xpm_load_convert_hex_digit(*begin++);
	unsigned char dn = xpm_load_convert_hex_digit(*begin++);
	return ((up << 4)&0xF0)|(dn & 0x0F);
}

OGLPLUS_LIB_FUNC
bool xpm_load_convert_color(
	const std::string& color,
	const std::map<std::string, std::string>& color_names,
	const std::size_t bipp,
	std::array<unsigned char, 4>& color_buf
)
{
	if(color.empty()) return false;

	assert(bipp % 8 == 0);
	std::size_t bpp = bipp/8;

	if(color.front() == '#')
	{
		std::size_t ebipp = (color.size()-1)*4;
		const char* begin = color.data()+1;
		const char* end  = color.data()+color.size();

		if(bipp == ebipp)
		{
			for(std::size_t b=0; b!=bpp; ++b)
			{
				color_buf[b] = xpm_load_convert_color_channel(
					begin,
					end
				);
			}
			for(std::size_t b=bpp; b!=4; ++b)
			{
				color_buf[b] = 0xFF;
			}
		}
		else if((bipp == 32) && (ebipp == 24))
		{
			for(std::size_t b=0; b!=3; ++b)
			{
				color_buf[b] = xpm_load_convert_color_channel(
					begin,
					end
				);
			}
			color_buf[3] = 0xFF;
		}
		else if((bipp >= 24) && (ebipp == 8))
		{
			unsigned char c = xpm_load_convert_color_channel(
				begin,
				end
			);
			color_buf[0] = c;
			color_buf[1] = c;
			color_buf[2] = c;
			color_buf[3] = 0xFF;
		}
		else return false;
	}
	else
	{
		auto p = color_names.find(color);
		if(p != color_names.end())
		{
			return xpm_load_convert_color(
				p->second,
				color_names,
				bipp,
				color_buf
			);
		}
		else return false;
	}
	return true;
}

OGLPLUS_LIB_FUNC
void xpm_load_make_palette(
	const std::map<std::string, std::string>& col_ents,
	const std::map<std::string, std::string>& color_names,
	std::map<std::string, std::array<unsigned char, 4>>& palette,
	const std::size_t bipp
)
{
	for(auto i = col_ents.begin(); i != col_ents.end(); ++i)
	{
		assert(!i->second.empty());

		const std::string& val = i->second;
		std::array<unsigned char, 4> pal_color;
		std::size_t j = 0, n = val.size();
		char best_type = '\0';
		while(j != n)
		{
			const char type = val[j];
			if(!type) break;

			++j;
			while((j != n) && ((val[j] == ' ') || (val[j] == '\t')))
			{
				++j;
			}
			std::size_t v = j;
			while((j != n) && ((val[j] != ' ') && (val[j] != '\t')) && val[j])
			{
				++j;
			}
			assert(v != j);

			if(xpm_load_convert_color(
				std::string(val.data()+v, val.data()+j),
				color_names,
				bipp,
				pal_color
			))
			{
				best_type = type;
				if(type == 'c') break;
			}

			while((j != n) && ((val[j] == ' ') || (val[j] == '\t')))
			{
				++j;
			}
		}
		if(!best_type)
		{
			throw std::runtime_error(
				"Failed to load XPM palette color entry '" +
				i->second +
				"'"
			);
		}
		palette[i->first] = pal_color;
	}
}

OGLPLUS_LIB_FUNC
void xpm_load(
	std::istream& input,
	Image& image,
	const std::map<std::string, std::string>& color_names,
	bool y_is_up,
	bool x_is_right
)
{
	const std::size_t max_line = 63;
	std::size_t line_len = 0;
	char line[max_line+1];

	// read first line
	if(!input.getline(line, max_line).good())
	{
		throw std::runtime_error("Failed to read XPM header from input");
	}
	line_len = std::size_t(input.gcount());

	std::size_t width = 0, height = 0, depth = 0, colors = 0, chpp = 0, bipp = 0;

	// parse the first line
	if(xpm_load_is_dims_line(line, line_len))
	{
		depth = 1;
		if(!xpm_load_parse_dims_line(
			line,
			line_len,
			width,
			height,
			depth,
			colors,
			chpp
		))
		{
			throw std::runtime_error("Failed to parse XPM header data");
		}
	}
	// otherwise the first line must be the XPM2 header
	else if(!xpm_load_is_header_line(line, line_len, width, height, depth))
	{
		throw std::runtime_error("Failed to parse XPM header");
	}
	// so we need to read the line with the dimensions data
	else if(
		!input.getline(line, max_line).good() ||
		!(line_len = std::size_t(input.gcount()))
	)
	{
		throw std::runtime_error("Failed to read XPM header data from input");
	}
	else if(!xpm_load_parse_dims_line(
		line,
		line_len,
		width,
		height,
		depth,
		colors,
		chpp
	))
	{
		throw std::runtime_error("Failed to parse XPM header data");
	}

	std::map<std::string, std::string> col_ents;

	// read and parse the palette entries
	for(std::size_t c=0; c!=colors; ++c)
	{
		do
		{
			if(!input.getline(line, max_line).good())
			{
				throw std::runtime_error("Failed to read XPM palette entry");
			}
			line_len = std::size_t(input.gcount());
		}
		while(line_len == 0);

		std::string color_code, color_value;

		if(!xpm_load_parse_palette_entry(
			line,
			line_len,
			chpp,
			color_code,
			color_value
		))
		{
			throw std::runtime_error("Failed to parse XPM palette entry");
		}

		if(col_ents.find(color_code) != col_ents.end())
		{
			throw std::runtime_error(
				"Duplicate XPM palette entry code '"+
				color_code +
				"'"
			);
		}

		col_ents[color_code] = color_value;
	}
	assert(col_ents.size() == colors);

	xpm_load_preload_col_ents(col_ents, color_names, chpp, bipp);

	std::map<std::string, std::array<unsigned char, 4>> palette;

	xpm_load_make_palette(col_ents, color_names, palette, bipp);

	assert(palette.size() == colors);
	assert(bipp % 8 == 0);

	std::size_t channels = bipp / 8;
	GLenum gl_format = 0;
	if     (channels == 1) gl_format = GL_RED;
	else if(channels == 2) gl_format = GL_RG;
	else if(channels == 3) gl_format = GL_RGB;
	else if(channels == 4) gl_format = GL_RGBA;
	else throw std::runtime_error(
		"Unable to determine GL pixel format for XPM data"
	);

	std::vector<unsigned char> data(width*height*depth*channels);

	for(std::size_t iz=0; iz!=depth; ++iz)
	{
		std::size_t z = iz;
		std::size_t plane_offs = width*height*z*channels;
		for(std::size_t iy=0; iy!=height; ++iy)
		{
			std::size_t y = y_is_up ? iy : (height-iy-1);
			std::size_t row_offs = plane_offs+height*y*channels;
			for(std::size_t ix=0; ix!=width; ++ix)
			{
				std::size_t x = x_is_right ? ix : (width-ix-1);
				std::size_t cur_offs = row_offs+x*channels;
				char b;
				do
				{
					if(!input.get(b).good())
					{
						throw std::runtime_error(
							"Unexpected end of XPM pixel data"
						);
					}
				}
				while((b == '\n') || (b == '\r'));

				std::string code(1, b);

				std::size_t todo = chpp;

				while(--todo != 0)
				{
					if(!input.get(b).good())
					{
						throw std::runtime_error(
							"Unexpected end of XPM pixel data"
						);
					}
					code.append(1, b);
				}

				auto p = palette.find(code);
				if(p == palette.end())
				{
					throw std::runtime_error(
						"Color code '" +
						code +
						"' not found in XPM palette"
					);
				}
				for(std::size_t c=0; c!=channels; ++c)
				{
					data[cur_offs+c] = p->second[c];
				}
			}
		}
	}

	image = Image(
		width,
		height,
		depth,
		channels,
		data.data(),
		PixelDataFormat(gl_format),
		PixelDataInternalFormat(gl_format)
	);
}

} // namespace aux

OGLPLUS_LIB_FUNC
XPMImage::XPMImage(std::istream& input, bool y_is_up, bool x_is_right)
{
	std::map<std::string, std::string> color_names;
	color_names["black"] = "#000000";
	color_names["gray"] = "#808080";
	color_names["white"] = "#FFFFFF";
	color_names["red"] = "#FF0000";
	color_names["green"] = "#00FF00";
	color_names["blue"] = "#0000FF";
	color_names["yellow"] = "#FFFF00";
	color_names["magenta"] = "#FF00FF";
	color_names["cyan"] = "#00FFFF";
	aux::xpm_load(input, *this, color_names, y_is_up, x_is_right);
}

} // images
} // oglplus


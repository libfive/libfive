/**
 *  @file oglplus/images/png.ipp
 *  @brief Implementation if PNG image loader (based on libpng)
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <stdexcept>
#include <fstream>
#include <cassert>
#include <iostream>
#include <png.h>

namespace oglplus {
namespace images {
namespace aux {

class PNGLoader;

struct PNGHeaderValidator
{
	PNGHeaderValidator(std::istream& input);
};

// structure managing the png_struct pointer
struct PNGReadStruct
{
	::png_structp _read;

	// Error handling function
	OGLPLUS_NORETURN
	static
	void _png_handle_error(::png_structp /*sp*/, const char* msg);

	// Warning handling function
	static void _png_handle_warning(::png_structp/*sp*/, const char* /*msg*/);

	PNGReadStruct(PNGLoader& /*loader*/);
	~PNGReadStruct(void);
};

// structure managing the first png_info pointer
struct PNGReadInfoStruct : PNGReadStruct
{
	::png_infop _info;

	PNGReadInfoStruct(PNGLoader& loader);
	~PNGReadInfoStruct(void);
};

struct PNGReadInfoEndStruct : PNGReadInfoStruct
{
	::png_infop _end;

	// data read callback
	static void _png_read_data(::png_structp, ::png_bytep, ::png_size_t);
	static int _png_read_user_chunk(::png_structp, ::png_unknown_chunkp);

	PNGReadInfoEndStruct(PNGLoader& loader);
	~PNGReadInfoEndStruct(void);
};

class PNGLoader
{
private:
	// reference to an input stream to read from
	::std::istream& _input;

	PNGHeaderValidator _validate_header;

	friend struct PNGReadInfoEndStruct;

	// data read functions
	void _read_data(::png_bytep data, ::png_size_t size);
	int _read_user_chunk(::png_unknown_chunkp /*chunk*/);

	PNGReadInfoEndStruct _png;

	static GLenum _translate_format(GLuint color_type, bool /*has_alpha*/);
public:
	PNGLoader(
		std::istream& input,
		Image& image,
		bool y_is_up,
		bool x_is_right
	);
};

OGLPLUS_LIB_FUNC
PNGHeaderValidator::PNGHeaderValidator(std::istream& input)
{
	if(!input.good())
	{
		throw std::runtime_error(
			"Unable to open file for reading"
		);
	}

	const size_t sig_size = 8;
	::png_byte sig[sig_size];
	input.read(reinterpret_cast<char*>(sig), sig_size);

	if(!input.good())
	{
		throw std::runtime_error(
			"Unable to read PNG signature"
		);
	}

	if(::png_sig_cmp(sig, 0, sig_size) != 0)
	{
		throw std::runtime_error(
			"Invalid PNG signature"
		);
	}
}

OGLPLUS_NORETURN
OGLPLUS_LIB_FUNC
void PNGReadStruct::_png_handle_error(
	::png_structp /*sp*/,
	const char* msg
)
{
	throw ::std::runtime_error(msg);
}

OGLPLUS_LIB_FUNC
void PNGReadStruct::_png_handle_warning(
	::png_structp /*sp*/,
	const char* msg
)
{
	::std::cerr << "libpng warning: " << msg << ::std::endl;
}

OGLPLUS_LIB_FUNC
PNGReadStruct::PNGReadStruct(PNGLoader& /*loader*/)
 : _read(::png_create_read_struct(
	PNG_LIBPNG_VER_STRING,
	reinterpret_cast<::png_voidp>(this),
	&_png_handle_error,
	&_png_handle_warning
))
{
	assert(_read);
}

OGLPLUS_LIB_FUNC
PNGReadStruct::~PNGReadStruct(void)
{
	::png_destroy_read_struct(
		&_read,
		static_cast<::png_infopp>(nullptr),
		static_cast<::png_infopp>(nullptr)
	);
}

OGLPLUS_LIB_FUNC
PNGReadInfoStruct::PNGReadInfoStruct(PNGLoader& loader)
 : PNGReadStruct(loader)
 , _info(::png_create_info_struct(_read))
{
	assert(_info);
}

OGLPLUS_LIB_FUNC
PNGReadInfoStruct::~PNGReadInfoStruct(void)
{
	::png_destroy_read_struct(
		&_read,
		&_info,
		static_cast<::png_infopp>(nullptr)
	);
}

OGLPLUS_LIB_FUNC
void PNGReadInfoEndStruct::_png_read_data(
	::png_structp png,
	::png_bytep data,
	::png_size_t size
)
{
	::png_voidp p = ::png_get_io_ptr(png);
	assert(p != 0);
	(reinterpret_cast<PNGLoader*>(p))->_read_data(data, size);
}

OGLPLUS_LIB_FUNC
int PNGReadInfoEndStruct::_png_read_user_chunk(
	::png_structp png,
	::png_unknown_chunkp chunk
)
{
	::png_voidp p = ::png_get_user_chunk_ptr(png);
	assert(p != 0);
	return (reinterpret_cast<PNGLoader*>(p))->_read_user_chunk(chunk);
}

OGLPLUS_LIB_FUNC
PNGReadInfoEndStruct::PNGReadInfoEndStruct(PNGLoader& loader)
 : PNGReadInfoStruct(loader)
 , _end(::png_create_info_struct(_read))
{
	assert(_end);
	::png_set_read_fn(
		_read,
		reinterpret_cast<::png_voidp>(&loader),
		&_png_read_data
	);
	::png_set_read_user_chunk_fn(
		_read,
		reinterpret_cast<::png_voidp>(&loader),
		&_png_read_user_chunk
	);
	::png_set_keep_unknown_chunks(
		_read,
		PNG_HANDLE_CHUNK_NEVER,
		0, 0
	);

}

OGLPLUS_LIB_FUNC
PNGReadInfoEndStruct::~PNGReadInfoEndStruct(void)
{
	::png_destroy_read_struct(
		&_read,
		&_info,
		&_end
	);
}

OGLPLUS_LIB_FUNC
void PNGLoader::_read_data(::png_bytep data, ::png_size_t size)
{
	_input.read(reinterpret_cast<char*>(data), std::streamsize(size));
	if(!_input.good())
	{
		throw std::runtime_error(
			"Unable to read PNG signature"
		);
	}
}

OGLPLUS_LIB_FUNC
int PNGLoader::_read_user_chunk(::png_unknown_chunkp /*chunk*/)
{
	return 0;
}

OGLPLUS_LIB_FUNC
GLenum PNGLoader::_translate_format(GLuint color_type, bool /*has_alpha*/)
{
	switch(color_type)
	{
		case PNG_COLOR_TYPE_GRAY:
			return GL_RED;
		case PNG_COLOR_TYPE_GRAY_ALPHA:
			return GL_RG;
		case PNG_COLOR_TYPE_RGB:
			return GL_RGB;
		case PNG_COLOR_TYPE_RGB_ALPHA:
			return GL_RGBA;
		// TODO other color types
		default:;
	}
	OGLPLUS_ABORT("Unknown color type!");
	return 0;
}

OGLPLUS_LIB_FUNC
PNGLoader::PNGLoader(
	std::istream& input,
	Image& image,
	bool y_is_up,
	bool x_is_right
): _input(input)
 , _validate_header(_input)
 , _png(*this)
{
	const size_t sig_size = 8;
	::png_set_sig_bytes(_png._read, sig_size);
	::png_read_info(_png._read, _png._info);

	GLuint width = GLuint(png_get_image_width(_png._read, _png._info));
	GLuint height = GLuint(png_get_image_height(_png._read, _png._info));
	GLuint bitdepth = png_get_bit_depth(_png._read, _png._info);
	GLuint channels = png_get_channels(_png._read, _png._info);
	GLuint color_type = png_get_color_type(_png._read, _png._info);

	// color conversions
	switch(color_type)
	{
		case PNG_COLOR_TYPE_PALETTE:
			::png_set_palette_to_rgb(_png._read);
			channels = 3;
			break;
		case PNG_COLOR_TYPE_GRAY:
			if(bitdepth < 8)
				::png_set_expand_gray_1_2_4_to_8(_png._read);
			bitdepth = 8;
			break;
		// TODO: other conversions
		default:;
	}

	// handle transparency
	bool has_alpha = false;
	if(::png_get_valid(_png._read, _png._info, PNG_INFO_tRNS))
	{
		::png_set_tRNS_to_alpha(_png._read);
		channels += 1;
		has_alpha = true;
	}

	// if there are too many bits per channel strip them down
	if(bitdepth == 16)
	{
		::png_set_strip_16(_png._read);
	}

	// bytes per row
	GLuint rowsize = width * channels * bitdepth / 8;
	// allocate the buffer
	std::vector<GLubyte> data(rowsize * height, GLubyte(0));
	{
		// allocate and initialize the row pointers
		std::vector< ::png_bytep> rows(height);
		//
		for(GLuint r=0; r<height; ++r)
		{
			GLuint row = y_is_up? (height-r-1): r;
			GLuint offs = row * rowsize;
			rows[r] = reinterpret_cast<::png_bytep>(data.data()) + offs;
		}

		// read
		::png_read_image(_png._read, rows.data());

		if(!x_is_right)
		{
			for(GLuint r=0; r<height; ++r)
			{
				for(GLuint p=0; p<width/2; ++p)
				{
					for(GLuint c=0; c<channels; ++c)
					{
						::png_byte tmp = rows[r][p*channels+c];
						rows[r][p*channels+c] = rows[r][(width-p-1)*channels+c];
						rows[r][(width-p-1)*channels+c] = tmp;
					}
				}
			}
		}
	}

	GLenum gl_format = _translate_format(color_type, has_alpha);
	std::size_t size = data.size();

	assert(size % (width*height) == 0);

	image = Image(
		width,
		height,
		1,
		channels,
		data.data(),
		PixelDataFormat(gl_format),
		PixelDataInternalFormat(gl_format)
	);
}

} // namespace aux

OGLPLUS_LIB_FUNC
PNGImage::PNGImage(const char* file_path, bool y_is_up, bool x_is_right)
{
	std::ifstream  file(file_path, std::ios::binary);
	aux::PNGLoader(file, *this, y_is_up, x_is_right);
}

OGLPLUS_LIB_FUNC
PNGImage::PNGImage(std::istream& input, bool y_is_up, bool x_is_right)
{
	aux::PNGLoader(input, *this, y_is_up, x_is_right);
}

} // images
} // oglplus


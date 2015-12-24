//  File implement/oglplus/enums/pixel_parameter_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/pixel_parameter.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLenum*,
	PixelParameter
> ValueRange_(PixelParameter*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_PIXELPARAMETER)
#define OGLPLUS_IMPL_EVR_PIXELPARAMETER
{
static const GLenum _values[] = {
#if defined GL_PACK_SWAP_BYTES
GL_PACK_SWAP_BYTES,
#endif
#if defined GL_PACK_LSB_FIRST
GL_PACK_LSB_FIRST,
#endif
#if defined GL_PACK_ROW_LENGTH
GL_PACK_ROW_LENGTH,
#endif
#if defined GL_PACK_SKIP_ROWS
GL_PACK_SKIP_ROWS,
#endif
#if defined GL_PACK_SKIP_PIXELS
GL_PACK_SKIP_PIXELS,
#endif
#if defined GL_PACK_ALIGNMENT
GL_PACK_ALIGNMENT,
#endif
#if defined GL_PACK_IMAGE_HEIGHT
GL_PACK_IMAGE_HEIGHT,
#endif
#if defined GL_PACK_SKIP_IMAGES
GL_PACK_SKIP_IMAGES,
#endif
#if defined GL_PACK_COMPRESSED_BLOCK_WIDTH
GL_PACK_COMPRESSED_BLOCK_WIDTH,
#endif
#if defined GL_PACK_COMPRESSED_BLOCK_HEIGHT
GL_PACK_COMPRESSED_BLOCK_HEIGHT,
#endif
#if defined GL_PACK_COMPRESSED_BLOCK_DEPTH
GL_PACK_COMPRESSED_BLOCK_DEPTH,
#endif
#if defined GL_PACK_COMPRESSED_BLOCK_SIZE
GL_PACK_COMPRESSED_BLOCK_SIZE,
#endif
#if defined GL_UNPACK_SWAP_BYTES
GL_UNPACK_SWAP_BYTES,
#endif
#if defined GL_UNPACK_LSB_FIRST
GL_UNPACK_LSB_FIRST,
#endif
#if defined GL_UNPACK_ROW_LENGTH
GL_UNPACK_ROW_LENGTH,
#endif
#if defined GL_UNPACK_SKIP_ROWS
GL_UNPACK_SKIP_ROWS,
#endif
#if defined GL_UNPACK_SKIP_PIXELS
GL_UNPACK_SKIP_PIXELS,
#endif
#if defined GL_UNPACK_ALIGNMENT
GL_UNPACK_ALIGNMENT,
#endif
#if defined GL_UNPACK_IMAGE_HEIGHT
GL_UNPACK_IMAGE_HEIGHT,
#endif
#if defined GL_UNPACK_SKIP_IMAGES
GL_UNPACK_SKIP_IMAGES,
#endif
#if defined GL_UNPACK_COMPRESSED_BLOCK_WIDTH
GL_UNPACK_COMPRESSED_BLOCK_WIDTH,
#endif
#if defined GL_UNPACK_COMPRESSED_BLOCK_HEIGHT
GL_UNPACK_COMPRESSED_BLOCK_HEIGHT,
#endif
#if defined GL_UNPACK_COMPRESSED_BLOCK_DEPTH
GL_UNPACK_COMPRESSED_BLOCK_DEPTH,
#endif
#if defined GL_UNPACK_COMPRESSED_BLOCK_SIZE
GL_UNPACK_COMPRESSED_BLOCK_SIZE,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	PixelParameter
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


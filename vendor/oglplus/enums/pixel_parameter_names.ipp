//  File implement/oglplus/enums/pixel_parameter_names.ipp
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
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	PixelParameter*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PIXELPARAMETER)
#define OGLPLUS_IMPL_EVN_PIXELPARAMETER
{
switch(value)
{
#if defined GL_PACK_SWAP_BYTES
	case GL_PACK_SWAP_BYTES: return StrCRef("PACK_SWAP_BYTES");
#endif
#if defined GL_PACK_LSB_FIRST
	case GL_PACK_LSB_FIRST: return StrCRef("PACK_LSB_FIRST");
#endif
#if defined GL_PACK_ROW_LENGTH
	case GL_PACK_ROW_LENGTH: return StrCRef("PACK_ROW_LENGTH");
#endif
#if defined GL_PACK_SKIP_ROWS
	case GL_PACK_SKIP_ROWS: return StrCRef("PACK_SKIP_ROWS");
#endif
#if defined GL_PACK_SKIP_PIXELS
	case GL_PACK_SKIP_PIXELS: return StrCRef("PACK_SKIP_PIXELS");
#endif
#if defined GL_PACK_ALIGNMENT
	case GL_PACK_ALIGNMENT: return StrCRef("PACK_ALIGNMENT");
#endif
#if defined GL_PACK_IMAGE_HEIGHT
	case GL_PACK_IMAGE_HEIGHT: return StrCRef("PACK_IMAGE_HEIGHT");
#endif
#if defined GL_PACK_SKIP_IMAGES
	case GL_PACK_SKIP_IMAGES: return StrCRef("PACK_SKIP_IMAGES");
#endif
#if defined GL_PACK_COMPRESSED_BLOCK_WIDTH
	case GL_PACK_COMPRESSED_BLOCK_WIDTH: return StrCRef("PACK_COMPRESSED_BLOCK_WIDTH");
#endif
#if defined GL_PACK_COMPRESSED_BLOCK_HEIGHT
	case GL_PACK_COMPRESSED_BLOCK_HEIGHT: return StrCRef("PACK_COMPRESSED_BLOCK_HEIGHT");
#endif
#if defined GL_PACK_COMPRESSED_BLOCK_DEPTH
	case GL_PACK_COMPRESSED_BLOCK_DEPTH: return StrCRef("PACK_COMPRESSED_BLOCK_DEPTH");
#endif
#if defined GL_PACK_COMPRESSED_BLOCK_SIZE
	case GL_PACK_COMPRESSED_BLOCK_SIZE: return StrCRef("PACK_COMPRESSED_BLOCK_SIZE");
#endif
#if defined GL_UNPACK_SWAP_BYTES
	case GL_UNPACK_SWAP_BYTES: return StrCRef("UNPACK_SWAP_BYTES");
#endif
#if defined GL_UNPACK_LSB_FIRST
	case GL_UNPACK_LSB_FIRST: return StrCRef("UNPACK_LSB_FIRST");
#endif
#if defined GL_UNPACK_ROW_LENGTH
	case GL_UNPACK_ROW_LENGTH: return StrCRef("UNPACK_ROW_LENGTH");
#endif
#if defined GL_UNPACK_SKIP_ROWS
	case GL_UNPACK_SKIP_ROWS: return StrCRef("UNPACK_SKIP_ROWS");
#endif
#if defined GL_UNPACK_SKIP_PIXELS
	case GL_UNPACK_SKIP_PIXELS: return StrCRef("UNPACK_SKIP_PIXELS");
#endif
#if defined GL_UNPACK_ALIGNMENT
	case GL_UNPACK_ALIGNMENT: return StrCRef("UNPACK_ALIGNMENT");
#endif
#if defined GL_UNPACK_IMAGE_HEIGHT
	case GL_UNPACK_IMAGE_HEIGHT: return StrCRef("UNPACK_IMAGE_HEIGHT");
#endif
#if defined GL_UNPACK_SKIP_IMAGES
	case GL_UNPACK_SKIP_IMAGES: return StrCRef("UNPACK_SKIP_IMAGES");
#endif
#if defined GL_UNPACK_COMPRESSED_BLOCK_WIDTH
	case GL_UNPACK_COMPRESSED_BLOCK_WIDTH: return StrCRef("UNPACK_COMPRESSED_BLOCK_WIDTH");
#endif
#if defined GL_UNPACK_COMPRESSED_BLOCK_HEIGHT
	case GL_UNPACK_COMPRESSED_BLOCK_HEIGHT: return StrCRef("UNPACK_COMPRESSED_BLOCK_HEIGHT");
#endif
#if defined GL_UNPACK_COMPRESSED_BLOCK_DEPTH
	case GL_UNPACK_COMPRESSED_BLOCK_DEPTH: return StrCRef("UNPACK_COMPRESSED_BLOCK_DEPTH");
#endif
#if defined GL_UNPACK_COMPRESSED_BLOCK_SIZE
	case GL_UNPACK_COMPRESSED_BLOCK_SIZE: return StrCRef("UNPACK_COMPRESSED_BLOCK_SIZE");
#endif
	default:;
}
OGLPLUS_FAKE_USE(value);
return StrCRef();
}
#else
;
#endif
} // namespace enums


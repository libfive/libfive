//  File implement/oglplus/enums/pixel_parameter_class.ipp
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
template <typename Base, template<PixelParameter> class Transform>
class EnumToClass<Base, PixelParameter, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_PACK_SWAP_BYTES
# if defined PackSwapBytes
#  pragma push_macro("PackSwapBytes")
#  undef PackSwapBytes
	Transform<PixelParameter::PackSwapBytes> PackSwapBytes;
#  pragma pop_macro("PackSwapBytes")
# else
	Transform<PixelParameter::PackSwapBytes> PackSwapBytes;
# endif
#endif
#if defined GL_PACK_LSB_FIRST
# if defined PackLSBFirst
#  pragma push_macro("PackLSBFirst")
#  undef PackLSBFirst
	Transform<PixelParameter::PackLSBFirst> PackLSBFirst;
#  pragma pop_macro("PackLSBFirst")
# else
	Transform<PixelParameter::PackLSBFirst> PackLSBFirst;
# endif
#endif
#if defined GL_PACK_ROW_LENGTH
# if defined PackRowLength
#  pragma push_macro("PackRowLength")
#  undef PackRowLength
	Transform<PixelParameter::PackRowLength> PackRowLength;
#  pragma pop_macro("PackRowLength")
# else
	Transform<PixelParameter::PackRowLength> PackRowLength;
# endif
#endif
#if defined GL_PACK_SKIP_ROWS
# if defined PackSkipRows
#  pragma push_macro("PackSkipRows")
#  undef PackSkipRows
	Transform<PixelParameter::PackSkipRows> PackSkipRows;
#  pragma pop_macro("PackSkipRows")
# else
	Transform<PixelParameter::PackSkipRows> PackSkipRows;
# endif
#endif
#if defined GL_PACK_SKIP_PIXELS
# if defined PackSkipPixels
#  pragma push_macro("PackSkipPixels")
#  undef PackSkipPixels
	Transform<PixelParameter::PackSkipPixels> PackSkipPixels;
#  pragma pop_macro("PackSkipPixels")
# else
	Transform<PixelParameter::PackSkipPixels> PackSkipPixels;
# endif
#endif
#if defined GL_PACK_ALIGNMENT
# if defined PackAlignment
#  pragma push_macro("PackAlignment")
#  undef PackAlignment
	Transform<PixelParameter::PackAlignment> PackAlignment;
#  pragma pop_macro("PackAlignment")
# else
	Transform<PixelParameter::PackAlignment> PackAlignment;
# endif
#endif
#if defined GL_PACK_IMAGE_HEIGHT
# if defined PackImageHeight
#  pragma push_macro("PackImageHeight")
#  undef PackImageHeight
	Transform<PixelParameter::PackImageHeight> PackImageHeight;
#  pragma pop_macro("PackImageHeight")
# else
	Transform<PixelParameter::PackImageHeight> PackImageHeight;
# endif
#endif
#if defined GL_PACK_SKIP_IMAGES
# if defined PackSkipImages
#  pragma push_macro("PackSkipImages")
#  undef PackSkipImages
	Transform<PixelParameter::PackSkipImages> PackSkipImages;
#  pragma pop_macro("PackSkipImages")
# else
	Transform<PixelParameter::PackSkipImages> PackSkipImages;
# endif
#endif
#if defined GL_PACK_COMPRESSED_BLOCK_WIDTH
# if defined PackCompressedBlockWidth
#  pragma push_macro("PackCompressedBlockWidth")
#  undef PackCompressedBlockWidth
	Transform<PixelParameter::PackCompressedBlockWidth> PackCompressedBlockWidth;
#  pragma pop_macro("PackCompressedBlockWidth")
# else
	Transform<PixelParameter::PackCompressedBlockWidth> PackCompressedBlockWidth;
# endif
#endif
#if defined GL_PACK_COMPRESSED_BLOCK_HEIGHT
# if defined PackCompressedBlockHeight
#  pragma push_macro("PackCompressedBlockHeight")
#  undef PackCompressedBlockHeight
	Transform<PixelParameter::PackCompressedBlockHeight> PackCompressedBlockHeight;
#  pragma pop_macro("PackCompressedBlockHeight")
# else
	Transform<PixelParameter::PackCompressedBlockHeight> PackCompressedBlockHeight;
# endif
#endif
#if defined GL_PACK_COMPRESSED_BLOCK_DEPTH
# if defined PackCompressedBlockDepth
#  pragma push_macro("PackCompressedBlockDepth")
#  undef PackCompressedBlockDepth
	Transform<PixelParameter::PackCompressedBlockDepth> PackCompressedBlockDepth;
#  pragma pop_macro("PackCompressedBlockDepth")
# else
	Transform<PixelParameter::PackCompressedBlockDepth> PackCompressedBlockDepth;
# endif
#endif
#if defined GL_PACK_COMPRESSED_BLOCK_SIZE
# if defined PackCompressedBlockSize
#  pragma push_macro("PackCompressedBlockSize")
#  undef PackCompressedBlockSize
	Transform<PixelParameter::PackCompressedBlockSize> PackCompressedBlockSize;
#  pragma pop_macro("PackCompressedBlockSize")
# else
	Transform<PixelParameter::PackCompressedBlockSize> PackCompressedBlockSize;
# endif
#endif
#if defined GL_UNPACK_SWAP_BYTES
# if defined UnpackSwapBytes
#  pragma push_macro("UnpackSwapBytes")
#  undef UnpackSwapBytes
	Transform<PixelParameter::UnpackSwapBytes> UnpackSwapBytes;
#  pragma pop_macro("UnpackSwapBytes")
# else
	Transform<PixelParameter::UnpackSwapBytes> UnpackSwapBytes;
# endif
#endif
#if defined GL_UNPACK_LSB_FIRST
# if defined UnpackLSBFirst
#  pragma push_macro("UnpackLSBFirst")
#  undef UnpackLSBFirst
	Transform<PixelParameter::UnpackLSBFirst> UnpackLSBFirst;
#  pragma pop_macro("UnpackLSBFirst")
# else
	Transform<PixelParameter::UnpackLSBFirst> UnpackLSBFirst;
# endif
#endif
#if defined GL_UNPACK_ROW_LENGTH
# if defined UnpackRowLength
#  pragma push_macro("UnpackRowLength")
#  undef UnpackRowLength
	Transform<PixelParameter::UnpackRowLength> UnpackRowLength;
#  pragma pop_macro("UnpackRowLength")
# else
	Transform<PixelParameter::UnpackRowLength> UnpackRowLength;
# endif
#endif
#if defined GL_UNPACK_SKIP_ROWS
# if defined UnpackSkipRows
#  pragma push_macro("UnpackSkipRows")
#  undef UnpackSkipRows
	Transform<PixelParameter::UnpackSkipRows> UnpackSkipRows;
#  pragma pop_macro("UnpackSkipRows")
# else
	Transform<PixelParameter::UnpackSkipRows> UnpackSkipRows;
# endif
#endif
#if defined GL_UNPACK_SKIP_PIXELS
# if defined UnpackSkipPixels
#  pragma push_macro("UnpackSkipPixels")
#  undef UnpackSkipPixels
	Transform<PixelParameter::UnpackSkipPixels> UnpackSkipPixels;
#  pragma pop_macro("UnpackSkipPixels")
# else
	Transform<PixelParameter::UnpackSkipPixels> UnpackSkipPixels;
# endif
#endif
#if defined GL_UNPACK_ALIGNMENT
# if defined UnpackAlignment
#  pragma push_macro("UnpackAlignment")
#  undef UnpackAlignment
	Transform<PixelParameter::UnpackAlignment> UnpackAlignment;
#  pragma pop_macro("UnpackAlignment")
# else
	Transform<PixelParameter::UnpackAlignment> UnpackAlignment;
# endif
#endif
#if defined GL_UNPACK_IMAGE_HEIGHT
# if defined UnpackImageHeight
#  pragma push_macro("UnpackImageHeight")
#  undef UnpackImageHeight
	Transform<PixelParameter::UnpackImageHeight> UnpackImageHeight;
#  pragma pop_macro("UnpackImageHeight")
# else
	Transform<PixelParameter::UnpackImageHeight> UnpackImageHeight;
# endif
#endif
#if defined GL_UNPACK_SKIP_IMAGES
# if defined UnpackSkipImages
#  pragma push_macro("UnpackSkipImages")
#  undef UnpackSkipImages
	Transform<PixelParameter::UnpackSkipImages> UnpackSkipImages;
#  pragma pop_macro("UnpackSkipImages")
# else
	Transform<PixelParameter::UnpackSkipImages> UnpackSkipImages;
# endif
#endif
#if defined GL_UNPACK_COMPRESSED_BLOCK_WIDTH
# if defined UnpackCompressedBlockWidth
#  pragma push_macro("UnpackCompressedBlockWidth")
#  undef UnpackCompressedBlockWidth
	Transform<PixelParameter::UnpackCompressedBlockWidth> UnpackCompressedBlockWidth;
#  pragma pop_macro("UnpackCompressedBlockWidth")
# else
	Transform<PixelParameter::UnpackCompressedBlockWidth> UnpackCompressedBlockWidth;
# endif
#endif
#if defined GL_UNPACK_COMPRESSED_BLOCK_HEIGHT
# if defined UnpackCompressedBlockHeight
#  pragma push_macro("UnpackCompressedBlockHeight")
#  undef UnpackCompressedBlockHeight
	Transform<PixelParameter::UnpackCompressedBlockHeight> UnpackCompressedBlockHeight;
#  pragma pop_macro("UnpackCompressedBlockHeight")
# else
	Transform<PixelParameter::UnpackCompressedBlockHeight> UnpackCompressedBlockHeight;
# endif
#endif
#if defined GL_UNPACK_COMPRESSED_BLOCK_DEPTH
# if defined UnpackCompressedBlockDepth
#  pragma push_macro("UnpackCompressedBlockDepth")
#  undef UnpackCompressedBlockDepth
	Transform<PixelParameter::UnpackCompressedBlockDepth> UnpackCompressedBlockDepth;
#  pragma pop_macro("UnpackCompressedBlockDepth")
# else
	Transform<PixelParameter::UnpackCompressedBlockDepth> UnpackCompressedBlockDepth;
# endif
#endif
#if defined GL_UNPACK_COMPRESSED_BLOCK_SIZE
# if defined UnpackCompressedBlockSize
#  pragma push_macro("UnpackCompressedBlockSize")
#  undef UnpackCompressedBlockSize
	Transform<PixelParameter::UnpackCompressedBlockSize> UnpackCompressedBlockSize;
#  pragma pop_macro("UnpackCompressedBlockSize")
# else
	Transform<PixelParameter::UnpackCompressedBlockSize> UnpackCompressedBlockSize;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_PACK_SWAP_BYTES
# if defined PackSwapBytes
#  pragma push_macro("PackSwapBytes")
#  undef PackSwapBytes
	 , PackSwapBytes(_base())
#  pragma pop_macro("PackSwapBytes")
# else
	 , PackSwapBytes(_base())
# endif
#endif
#if defined GL_PACK_LSB_FIRST
# if defined PackLSBFirst
#  pragma push_macro("PackLSBFirst")
#  undef PackLSBFirst
	 , PackLSBFirst(_base())
#  pragma pop_macro("PackLSBFirst")
# else
	 , PackLSBFirst(_base())
# endif
#endif
#if defined GL_PACK_ROW_LENGTH
# if defined PackRowLength
#  pragma push_macro("PackRowLength")
#  undef PackRowLength
	 , PackRowLength(_base())
#  pragma pop_macro("PackRowLength")
# else
	 , PackRowLength(_base())
# endif
#endif
#if defined GL_PACK_SKIP_ROWS
# if defined PackSkipRows
#  pragma push_macro("PackSkipRows")
#  undef PackSkipRows
	 , PackSkipRows(_base())
#  pragma pop_macro("PackSkipRows")
# else
	 , PackSkipRows(_base())
# endif
#endif
#if defined GL_PACK_SKIP_PIXELS
# if defined PackSkipPixels
#  pragma push_macro("PackSkipPixels")
#  undef PackSkipPixels
	 , PackSkipPixels(_base())
#  pragma pop_macro("PackSkipPixels")
# else
	 , PackSkipPixels(_base())
# endif
#endif
#if defined GL_PACK_ALIGNMENT
# if defined PackAlignment
#  pragma push_macro("PackAlignment")
#  undef PackAlignment
	 , PackAlignment(_base())
#  pragma pop_macro("PackAlignment")
# else
	 , PackAlignment(_base())
# endif
#endif
#if defined GL_PACK_IMAGE_HEIGHT
# if defined PackImageHeight
#  pragma push_macro("PackImageHeight")
#  undef PackImageHeight
	 , PackImageHeight(_base())
#  pragma pop_macro("PackImageHeight")
# else
	 , PackImageHeight(_base())
# endif
#endif
#if defined GL_PACK_SKIP_IMAGES
# if defined PackSkipImages
#  pragma push_macro("PackSkipImages")
#  undef PackSkipImages
	 , PackSkipImages(_base())
#  pragma pop_macro("PackSkipImages")
# else
	 , PackSkipImages(_base())
# endif
#endif
#if defined GL_PACK_COMPRESSED_BLOCK_WIDTH
# if defined PackCompressedBlockWidth
#  pragma push_macro("PackCompressedBlockWidth")
#  undef PackCompressedBlockWidth
	 , PackCompressedBlockWidth(_base())
#  pragma pop_macro("PackCompressedBlockWidth")
# else
	 , PackCompressedBlockWidth(_base())
# endif
#endif
#if defined GL_PACK_COMPRESSED_BLOCK_HEIGHT
# if defined PackCompressedBlockHeight
#  pragma push_macro("PackCompressedBlockHeight")
#  undef PackCompressedBlockHeight
	 , PackCompressedBlockHeight(_base())
#  pragma pop_macro("PackCompressedBlockHeight")
# else
	 , PackCompressedBlockHeight(_base())
# endif
#endif
#if defined GL_PACK_COMPRESSED_BLOCK_DEPTH
# if defined PackCompressedBlockDepth
#  pragma push_macro("PackCompressedBlockDepth")
#  undef PackCompressedBlockDepth
	 , PackCompressedBlockDepth(_base())
#  pragma pop_macro("PackCompressedBlockDepth")
# else
	 , PackCompressedBlockDepth(_base())
# endif
#endif
#if defined GL_PACK_COMPRESSED_BLOCK_SIZE
# if defined PackCompressedBlockSize
#  pragma push_macro("PackCompressedBlockSize")
#  undef PackCompressedBlockSize
	 , PackCompressedBlockSize(_base())
#  pragma pop_macro("PackCompressedBlockSize")
# else
	 , PackCompressedBlockSize(_base())
# endif
#endif
#if defined GL_UNPACK_SWAP_BYTES
# if defined UnpackSwapBytes
#  pragma push_macro("UnpackSwapBytes")
#  undef UnpackSwapBytes
	 , UnpackSwapBytes(_base())
#  pragma pop_macro("UnpackSwapBytes")
# else
	 , UnpackSwapBytes(_base())
# endif
#endif
#if defined GL_UNPACK_LSB_FIRST
# if defined UnpackLSBFirst
#  pragma push_macro("UnpackLSBFirst")
#  undef UnpackLSBFirst
	 , UnpackLSBFirst(_base())
#  pragma pop_macro("UnpackLSBFirst")
# else
	 , UnpackLSBFirst(_base())
# endif
#endif
#if defined GL_UNPACK_ROW_LENGTH
# if defined UnpackRowLength
#  pragma push_macro("UnpackRowLength")
#  undef UnpackRowLength
	 , UnpackRowLength(_base())
#  pragma pop_macro("UnpackRowLength")
# else
	 , UnpackRowLength(_base())
# endif
#endif
#if defined GL_UNPACK_SKIP_ROWS
# if defined UnpackSkipRows
#  pragma push_macro("UnpackSkipRows")
#  undef UnpackSkipRows
	 , UnpackSkipRows(_base())
#  pragma pop_macro("UnpackSkipRows")
# else
	 , UnpackSkipRows(_base())
# endif
#endif
#if defined GL_UNPACK_SKIP_PIXELS
# if defined UnpackSkipPixels
#  pragma push_macro("UnpackSkipPixels")
#  undef UnpackSkipPixels
	 , UnpackSkipPixels(_base())
#  pragma pop_macro("UnpackSkipPixels")
# else
	 , UnpackSkipPixels(_base())
# endif
#endif
#if defined GL_UNPACK_ALIGNMENT
# if defined UnpackAlignment
#  pragma push_macro("UnpackAlignment")
#  undef UnpackAlignment
	 , UnpackAlignment(_base())
#  pragma pop_macro("UnpackAlignment")
# else
	 , UnpackAlignment(_base())
# endif
#endif
#if defined GL_UNPACK_IMAGE_HEIGHT
# if defined UnpackImageHeight
#  pragma push_macro("UnpackImageHeight")
#  undef UnpackImageHeight
	 , UnpackImageHeight(_base())
#  pragma pop_macro("UnpackImageHeight")
# else
	 , UnpackImageHeight(_base())
# endif
#endif
#if defined GL_UNPACK_SKIP_IMAGES
# if defined UnpackSkipImages
#  pragma push_macro("UnpackSkipImages")
#  undef UnpackSkipImages
	 , UnpackSkipImages(_base())
#  pragma pop_macro("UnpackSkipImages")
# else
	 , UnpackSkipImages(_base())
# endif
#endif
#if defined GL_UNPACK_COMPRESSED_BLOCK_WIDTH
# if defined UnpackCompressedBlockWidth
#  pragma push_macro("UnpackCompressedBlockWidth")
#  undef UnpackCompressedBlockWidth
	 , UnpackCompressedBlockWidth(_base())
#  pragma pop_macro("UnpackCompressedBlockWidth")
# else
	 , UnpackCompressedBlockWidth(_base())
# endif
#endif
#if defined GL_UNPACK_COMPRESSED_BLOCK_HEIGHT
# if defined UnpackCompressedBlockHeight
#  pragma push_macro("UnpackCompressedBlockHeight")
#  undef UnpackCompressedBlockHeight
	 , UnpackCompressedBlockHeight(_base())
#  pragma pop_macro("UnpackCompressedBlockHeight")
# else
	 , UnpackCompressedBlockHeight(_base())
# endif
#endif
#if defined GL_UNPACK_COMPRESSED_BLOCK_DEPTH
# if defined UnpackCompressedBlockDepth
#  pragma push_macro("UnpackCompressedBlockDepth")
#  undef UnpackCompressedBlockDepth
	 , UnpackCompressedBlockDepth(_base())
#  pragma pop_macro("UnpackCompressedBlockDepth")
# else
	 , UnpackCompressedBlockDepth(_base())
# endif
#endif
#if defined GL_UNPACK_COMPRESSED_BLOCK_SIZE
# if defined UnpackCompressedBlockSize
#  pragma push_macro("UnpackCompressedBlockSize")
#  undef UnpackCompressedBlockSize
	 , UnpackCompressedBlockSize(_base())
#  pragma pop_macro("UnpackCompressedBlockSize")
# else
	 , UnpackCompressedBlockSize(_base())
# endif
#endif
	{ }
};

} // namespace enums


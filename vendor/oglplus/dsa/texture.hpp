/**
 *  @file oglplus/dsa/texture.hpp
 *  @brief Texture object wrappers with direct state access
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_DSA_TEXTURE_1107121519_HPP
#define OGLPLUS_DSA_TEXTURE_1107121519_HPP

#include <oglplus/texture.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_5 || GL_ARB_direct_state_access

template <>
struct ObjGenTag<tag::DirectState, tag::Texture>
{
	typedef tag::Create Type;
};

/// Class wrapping texture object functionality with direct state access
/** @note Do not use this class directly, use DSATexture instead.
 *
 */
template <>
class ObjectOps<tag::DirectState, tag::Texture>
 : public ObjZeroOps<tag::DirectState, tag::Texture>
{
protected:
	ObjectOps(TextureName name)
	OGLPLUS_NOEXCEPT(true)
	 : ObjZeroOps<tag::DirectState, tag::Texture>(name)
	{ }
public:
#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	ObjectOps(ObjectOps&&) = default;
	ObjectOps(const ObjectOps&) = default;
	ObjectOps& operator = (ObjectOps&&) = default;
	ObjectOps& operator = (const ObjectOps&) = default;
#else
	typedef ObjZeroOps<tag::DirectState, tag::Texture> _base;

	ObjectOps(ObjectOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	 : _base(static_cast<_base&&>(temp))
	{ }

	ObjectOps(const ObjectOps& that)
	OGLPLUS_NOEXCEPT(true)
	 : _base(static_cast<const _base&>(that))
	{ }

	ObjectOps& operator = (ObjectOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	{
		_base::operator = (static_cast<_base&&>(temp));
		return *this;
	}

	ObjectOps& operator = (const ObjectOps& that)
	OGLPLUS_NOEXCEPT(true)
	{
		_base::operator = (static_cast<const _base&>(that));
		return *this;
	}
#endif
	/// Types related to Texture
	typedef TextureOps::Property Property;

	using ObjCommonOps<tag::Texture>::Bind;

	GLint GetIntParam(GLenum query) const;
	GLfloat GetFloatParam(GLenum query) const;

	GLint GetIntParam(GLint level, GLenum query) const;
	GLfloat GetFloatParam(GLint level, GLenum query) const;

	/// Returns the width of the texture as it was specified by *Image*D
	/**
	 *  @see Height
	 *  @see Depth
	 *
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_WIDTH}
	 */
	SizeType Width(GLint level = 0) const
	{
		return MakeSizeType(
			GetIntParam(level, GL_TEXTURE_WIDTH),
			std::nothrow
		);
	}

	/// Returns the height of the texture as it was specified by *Image*D
	/**
	 *  @see Width
	 *  @see Depth
	 *
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_HEIGHT}
	 */
	SizeType Height(GLint level = 0) const
	{
		return MakeSizeType(
			GetIntParam(level, GL_TEXTURE_HEIGHT),
			std::nothrow
		);
	}

	/// Returns the depth of the texture as it was specified by *Image*D
	/**
	 *  @see Width
	 *  @see Height
	 *
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_DEPTH}
	 */
	SizeType Depth(GLint level = 0) const
	{
		return MakeSizeType(
			GetIntParam(level, GL_TEXTURE_DEPTH),
			std::nothrow
		);
	}

	/// Returns the data type used to store the RED component
	/**
	 *  @see GreenType
	 *  @see BlueType
	 *  @see AlphaType
	 *  @see DepthType
	 *
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_RED_TYPE}
	 */
	PixelDataType RedType(GLint level = 0) const
	{
		return PixelDataType(GetIntParam(
			level,
			GL_TEXTURE_RED_TYPE
		));
	}

	/// Returns the data type used to store the GREEN component
	/**
	 *  @see RedType
	 *  @see BlueType
	 *  @see AlphaType
	 *  @see DepthType
	 *
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_GREEN_TYPE}
	 */
	PixelDataType GreenType(GLint level = 0) const
	{
		return PixelDataType(GetIntParam(
			level,
			GL_TEXTURE_GREEN_TYPE
		));
	}

	/// Returns the data type used to store the BLUE component
	/**
	 *  @see RedType
	 *  @see GreenType
	 *  @see AlphaType
	 *  @see DepthType
	 *
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_BLUE_TYPE}
	 */
	PixelDataType BlueType(GLint level = 0) const
	{
		return PixelDataType(GetIntParam(
			level,
			GL_TEXTURE_BLUE_TYPE
		));
	}

	/// Returns the data type used to store the ALPHA component
	/**
	 *  @see RedType
	 *  @see GreenType
	 *  @see BlueType
	 *  @see DepthType
	 *
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_ALPHA_TYPE}
	 */
	PixelDataType AlphaType(GLint level = 0) const
	{
		return PixelDataType(GetIntParam(
			level,
			GL_TEXTURE_ALPHA_TYPE
		));
	}

	/// Returns the data type used to store the DEPTH component
	/**
	 *  @see RedType
	 *  @see GreenType
	 *  @see BlueType
	 *  @see AlphaType
	 *
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_DEPTH_TYPE}
	 */
	PixelDataType DepthType(GLint level = 0) const
	{
		return PixelDataType(GetIntParam(
			level,
			GL_TEXTURE_DEPTH_TYPE
		));
	}

	/// Returns the actual resolution of the RED component
	/**
	 *  @see GreenSize
	 *  @see BlueSize
	 *  @see AlphaSize
	 *  @see DepthSize
	 *  @see StencilSize
	 *
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_RED_SIZE}
	 */
	SizeType RedSize(GLint level = 0) const
	{
		return MakeSizeType(
			GetIntParam(level, GL_TEXTURE_RED_SIZE),
			std::nothrow
		);
	}

	/// Returns the actual resolution of the GREEN component
	/**
	 *  @see RedSize
	 *  @see BlueSize
	 *  @see AlphaSize
	 *  @see DepthSize
	 *  @see StencilSize
	 *
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_GREEN_SIZE}
	 */
	SizeType GreenSize(GLint level = 0) const
	{
		return MakeSizeType(
			GetIntParam(level, GL_TEXTURE_GREEN_SIZE),
			std::nothrow
		);
	}

	/// Returns the actual resolution of the BLUE component
	/**
	 *  @see RedSize
	 *  @see GreenSize
	 *  @see AlphaSize
	 *  @see DepthSize
	 *  @see StencilSize
	 *
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_BLUE_SIZE}
	 */
	SizeType BlueSize(GLint level = 0) const
	{
		return MakeSizeType(
			GetIntParam(level, GL_TEXTURE_BLUE_SIZE),
			std::nothrow
		);
	}

	/// Returns the actual resolution of the ALPHA component
	/**
	 *  @see RedSize
	 *  @see GreenSize
	 *  @see BlueSize
	 *  @see DepthSize
	 *  @see StencilSize
	 *
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_ALPHA_SIZE}
	 */
	SizeType AlphaSize(GLint level = 0) const
	{
		return MakeSizeType(
			GetIntParam(level, GL_TEXTURE_ALPHA_SIZE),
			std::nothrow
		);
	}

	/// Returns the actual resolution of the DEPTH component
	/**
	 *  @see RedSize
	 *  @see GreenSize
	 *  @see BlueSize
	 *  @see AlphaSize
	 *  @see StencilSize
	 *
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_DEPTH_SIZE}
	 */
	SizeType DepthSize(GLint level = 0) const
	{
		return MakeSizeType(
			GetIntParam(level, GL_TEXTURE_DEPTH_SIZE),
			std::nothrow
		);
	}

	/// Returns the actual resolution of the STENCIL component
	/**
	 *  @see RedSize
	 *  @see GreenSize
	 *  @see BlueSize
	 *  @see AlphaSize
	 *  @see DepthSize
	 *
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_STENCIL_SIZE}
	 */
	SizeType StencilSize(GLint level = 0) const
	{
		return MakeSizeType(
			GetIntParam(level, GL_TEXTURE_STENCIL_SIZE),
			std::nothrow
		);
	}

	/// Returns the size of all texture components
	/**
	 *  @see RedSize
	 *  @see GreenSize
	 *  @see BlueSize
	 *  @see AlphaSize
	 *  @see DepthSize
	 *  @see StencilSize
	 *
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_SHARED_SIZE}
	 */
	SizeType SharedSize(GLint level = 0) const
	{
		return MakeSizeType(
			GetIntParam(level, GL_TEXTURE_SHARED_SIZE),
			std::nothrow
		);
	}

	/// Returns the size (in bytes) of the image array if it is compressed
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_COMPRESSED_IMAGE_SIZE}
	 */
	SizeType CompressedImageSize(GLint level = 0) const
	{
		return MakeSizeType(
			GetIntParam(level, GL_TEXTURE_COMPRESSED_IMAGE_SIZE),
			std::nothrow
		);
	}

	/// Returns the internal data format of the image array
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_INTERNAL_FORMAT}
	 */
	PixelDataInternalFormat InternalFormat(GLint level = 0) const
	{
		return PixelDataInternalFormat(GetIntParam(
			level,
			GL_TEXTURE_INTERNAL_FORMAT
		));
	}

	/// Allows to obtain the texture image in uncompressed form
	/** This function stores the image of the texture bound to
	 *  the specified texture @p target with the specified @p level
	 *  of detail in uncompressed form into the @p dest buffer.
	 *
	 *  @note This function, unlike @c GetCompressedImage, does NOT
	 *  automatically resize the destination buffer so that
	 *  it can accomodate the texture data. The caller is responsible
	 *  for keeping track or querying the type of the texture, its
	 *  dimensions and current pixel transfer settings and resize
	 *  the @c dest buffer accordingly.
	 *
	 *  @glsymbols
	 *  @glfunref{GetTexImage}
	 */
	void GetImage(
		GLint level,
		PixelDataFormat format,
		const OutputData& dest
	) const;

	/// Allows to obtain the texture image in uncompressed form
	/** This function stores the image of the texture bound to
	 *  the specified texture @p target with the specified @p level
	 *  of detail in uncompressed form into the @p dest buffer.
	 *
	 *  @note This function, unlike @c GetCompressedImage, does NOT
	 *  automatically resize the destination buffer so that
	 *  it can accomodate the texture data. The caller is responsible
	 *  for keeping track or querying the type of the texture, its
	 *  dimensions and current pixel transfer settings and resize
	 *  the @c dest buffer accordingly.
	 *
	 *  @glsymbols
	 *  @glfunref{GetTexImage}
	 */
	void GetImage(
		GLint level,
		PixelDataFormat format,
		Property::PixDataType type,
		SizeType size,
		GLvoid* buffer
	) const
	{
		GetImage(level, format, OutputData(type, size, buffer));
	}

	void GetCompressedImage(
		GLint level,
		const OutputData& dest
	) const;

	/// Allows to obtain the texture image in compressed form
	/** This function stores the image of the texture bound to
	 *  the specified texture @p target with the specified @p level
	 *  of detail in compressed form into the @p dest buffer.
	 *  This function automatically resizes the buffer so that
	 *  it can accomodate the texture data.
	 *
	 *  @glsymbols
	 *  @glfunref{GetCompressedTexImage}
	 */
	void GetCompressedImage(
		GLint level,
		SizeType size,
		GLubyte* buffer
	) const
	{
		GetCompressedImage(level, OutputData(size, buffer));
	}

	/// Allows to obtain the texture image in compressed form
	/** This function stores the image of the texture bound to
	 *  the specified texture @p target with the specified @p level
	 *  of detail in compressed form into the @p dest buffer.
	 *  This function automatically resizes the buffer so that
	 *  it can accomodate the texture data.
	 *
	 *  @glsymbols
	 *  @glfunref{GetCompressedTexImage}
	 */
	void GetCompressedImage(
		GLint level,
		std::vector<GLubyte>& dest
	) const;

	/// Specifies all levels of 1D texture at the same time
	/**
	 *  @glsymbols
	 *  @glfunref{TextureStorage1D}
	 */
	ObjectOps& Storage1D(
		SizeType levels,
		PixelDataInternalFormat internal_format,
		SizeType width
	)
	{
		OGLPLUS_GLFUNC(TextureStorage1D)(
			_obj_name(),
			levels,
			GLenum(internal_format),
			width
		);
		OGLPLUS_CHECK(
			TextureStorage1D,
			ObjectError,
			Object(*this).
			EnumParam(internal_format)
		);
		return *this;
	}

	ObjectOps& Storage1D(
		SizeType levels,
		const images::ImageSpec& image_spec
	);

	/// Specifies all levels of 2D texture at the same time
	/**
	 *  @glsymbols
	 *  @glfunref{TextureStorage2D}
	 */
	ObjectOps& Storage2D(
		SizeType levels,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height
	)
	{
		OGLPLUS_GLFUNC(TextureStorage2D)(
			_obj_name(),
			levels,
			GLenum(internal_format),
			width,
			height
		);
		OGLPLUS_CHECK(
			TextureStorage2D,
			ObjectError,
			Object(*this).
			EnumParam(internal_format)
		);
		return *this;
	}

	ObjectOps& Storage2D(
		SizeType levels,
		const images::ImageSpec& image_spec
	);

	/// Specifies all levels of 3D texture at the same time
	/**
	 *  @glsymbols
	 *  @glfunref{TextureStorage3D}
	 */
	ObjectOps& Storage3D(
		SizeType levels,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height,
		SizeType depth
	)
	{
		OGLPLUS_GLFUNC(TextureStorage3D)(
			_obj_name(),
			levels,
			GLenum(internal_format),
			width,
			height,
			depth
		);
		OGLPLUS_CHECK(
			TextureStorage3D,
			ObjectError,
			Object(*this).
			EnumParam(internal_format)
		);
		return *this;
	}

	ObjectOps& Storage3D(
		SizeType levels,
		const images::ImageSpec& image_spec
	);

	ObjectOps& Storage(
		SizeType levels,
		const images::ImageSpec& image_spec
	);

	/// Specifies a three dimensional texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{TexSubImage3D}
	 */
	ObjectOps& SubImage3D(
		GLint level,
		GLint xoffs,
		GLint yoffs,
		GLint zoffs,
		SizeType width,
		SizeType height,
		SizeType depth,
		PixelDataFormat format,
		Property::PixDataType type,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(TextureSubImage3D)(
			_obj_name(),
			level,
			xoffs,
			yoffs,
			zoffs,
			width,
			height,
			depth,
			GLenum(format),
			GLenum(type),
			data
		);
		OGLPLUS_CHECK(
			TextureSubImage3D,
			ObjectError,
			Object(*this).
			EnumParam(format).
			Index(level)
		);
		return *this;
	}

	/// Specifies a three dimensional texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{TexSubImage3D}
	 */
	ObjectOps& SubImage3D(
		const images::Image& image,
		GLint xoffs = 0,
		GLint yoffs = 0,
		GLint zoffs = 0,
		GLint level = 0
	);

	/// Specifies a two dimensional texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{TexSubImage2D}
	 */
	ObjectOps& SubImage2D(
		GLint level,
		GLint xoffs,
		GLint yoffs,
		SizeType width,
		SizeType height,
		PixelDataFormat format,
		Property::PixDataType type,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(TextureSubImage2D)(
			_obj_name(),
			level,
			xoffs,
			yoffs,
			width,
			height,
			GLenum(format),
			GLenum(type),
			data
		);
		OGLPLUS_CHECK(
			TextureSubImage2D,
			ObjectError,
			Object(*this).
			EnumParam(format).
			Index(level)
		);
		return *this;
	}

	/// Specifies a two dimensional texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{TexSubImage2D}
	 */
	ObjectOps& SubImage2D(
		const images::Image& image,
		GLint xoffs = 0,
		GLint yoffs = 0,
		GLint level = 0
	);

	/// Specifies a one dimensional texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{TexSubImage1D}
	 */
	ObjectOps& SubImage1D(
		GLint level,
		GLint xoffs,
		SizeType width,
		PixelDataFormat format,
		Property::PixDataType type,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(TextureSubImage1D)(
			_obj_name(),
			level,
			xoffs,
			width,
			GLenum(format),
			GLenum(type),
			data
		);
		OGLPLUS_CHECK(
			TextureSubImage1D,
			ObjectError,
			Object(*this).
			EnumParam(format).
			Index(level)
		);
		return *this;
	}

	/// Specifies a two dimensional texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{TexSubImage1D}
	 */
	ObjectOps& SubImage1D(
		const images::Image& image,
		GLint xoffs = 0,
		GLint level = 0
	);

	/// Copies a three dimensional texture sub image from the framebuffer
	/**
	 *  @glsymbols
	 *  @glfunref{CopyTexSubImage3D}
	 */
	ObjectOps& CopySubImage3D(
		GLint level,
		GLint xoffs,
		GLint yoffs,
		GLint zoffs,
		GLint x,
		GLint y,
		SizeType width,
		SizeType height
	)
	{
		OGLPLUS_GLFUNC(CopyTextureSubImage3D)(
			_obj_name(),
			level,
			xoffs,
			yoffs,
			zoffs,
			x,
			y,
			width,
			height
		);
		OGLPLUS_CHECK(
			CopyTextureSubImage3D,
			ObjectError,
			Object(*this).
			Index(level)
		);
		return *this;
	}

	/// Copies a two dimensional texture sub image from the framebuffer
	/**
	 *  @glsymbols
	 *  @glfunref{CopyTexSubImage2D}
	 */
	ObjectOps& CopySubImage2D(
		GLint level,
		GLint xoffs,
		GLint yoffs,
		GLint x,
		GLint y,
		SizeType width,
		SizeType height
	)
	{
		OGLPLUS_GLFUNC(CopyTextureSubImage2D)(
			_obj_name(),
			level,
			xoffs,
			yoffs,
			x,
			y,
			width,
			height
		);
		OGLPLUS_CHECK(
			CopyTextureSubImage2D,
			ObjectError,
			Object(*this).
			Index(level)
		);
		return *this;
	}

	/// Copies a one dimensional texture sub image from the framebuffer
	/**
	 *  @glsymbols
	 *  @glfunref{CopyTexSubImage1D}
	 */
	ObjectOps& CopySubImage1D(
		GLint level,
		GLint xoffs,
		GLint x,
		GLint y,
		SizeType width
	)
	{
		OGLPLUS_GLFUNC(CopyTextureSubImage1D)(
			_obj_name(),
			level,
			xoffs,
			x,
			y,
			width
		);
		OGLPLUS_CHECK(
			CopyTextureSubImage1D,
			ObjectError,
			Object(*this).
			Index(level)
		);
		return *this;
	}

	/// Specifies a three dimensional compressed texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{CompressedTexSubImage3D}
	 */
	ObjectOps& CompressedSubImage3D(
		GLint level,
		GLint xoffs,
		GLint yoffs,
		GLint zoffs,
		SizeType width,
		SizeType height,
		SizeType depth,
		PixelDataFormat format,
		SizeType image_size,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(CompressedTextureSubImage3D)(
			_obj_name(),
			level,
			xoffs,
			yoffs,
			zoffs,
			width,
			height,
			depth,
			GLint(format),
			image_size,
			data
		);
		OGLPLUS_CHECK(
			CompressedTextureSubImage3D,
			ObjectError,
			Object(*this).
			EnumParam(format).
			Index(level)
		);
		return *this;
	}

	/// Specifies a two dimensional compressed texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{CompressedTexSubImage2D}
	 */
	ObjectOps& CompressedSubImage2D(
		GLint level,
		GLint xoffs,
		GLint yoffs,
		SizeType width,
		SizeType height,
		PixelDataFormat format,
		SizeType image_size,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(CompressedTextureSubImage2D)(
			_obj_name(),
			level,
			xoffs,
			yoffs,
			width,
			height,
			GLint(format),
			image_size,
			data
		);
		OGLPLUS_CHECK(
			CompressedTextureSubImage2D,
			ObjectError,
			Object(*this).
			EnumParam(format).
			Index(level)
		);
		return *this;
	}

	/// Specifies a one dimensional compressed texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{CompressedTexSubImage1D}
	 */
	ObjectOps& CompressedSubImage1D(
		GLint level,
		GLint xoffs,
		SizeType width,
		PixelDataFormat format,
		SizeType image_size,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(CompressedTextureSubImage1D)(
			_obj_name(),
			level,
			xoffs,
			width,
			GLint(format),
			image_size,
			data
		);
		OGLPLUS_CHECK(
			CompressedTextureSubImage1D,
			ObjectError,
			Object(*this).
			EnumParam(format).
			Index(level)
		);
		return *this;
	}

	/// Assigns a buffer storing the texel data to the texture
	/**
	 *  @glverreq{3,1}
	 *  @glsymbols
	 *  @glfunref{TexBuffer}
	 */
	ObjectOps& Buffer(
		PixelDataInternalFormat internal_format,
		BufferName buffer
	)
	{
		OGLPLUS_GLFUNC(TextureBuffer)(
			_obj_name(),
			GLenum(internal_format),
			GetGLName(buffer)
		);
		OGLPLUS_CHECK(
			TextureBuffer,
			ObjectPairError,
			Subject(buffer).
			Object(*this).
			EnumParam(internal_format)
		);
		return *this;
	}

	/// Returns the texture base level (TEXTURE_BASE_LEVEL)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_BASE_LEVEL}
	 */
	GLuint BaseLevel(void) const
	{
		return GetIntParam(GL_TEXTURE_BASE_LEVEL);
	}

	/// Sets the texture base level (TEXTURE_BASE_LEVEL)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_BASE_LEVEL}
	 */
	ObjectOps& BaseLevel(GLuint level)
	{
		OGLPLUS_GLFUNC(TextureParameteri)(
			_obj_name(),
			GL_TEXTURE_BASE_LEVEL,
			level
		);
		OGLPLUS_CHECK(
			TextureParameteri,
			ObjectError,
			Object(*this).
			Index(level)
		);
		return *this;
	}

	/// Gets the texture border color (TEXTURE_BORDER_COLOR)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_BORDER_COLOR}
	 */
	Vector<GLfloat, 4> BorderColor(TypeTag<GLfloat>) const
	{
		GLfloat result[4];
		OGLPLUS_GLFUNC(GetTextureParameterfv)(
			_obj_name(),
			GL_TEXTURE_BORDER_COLOR,
			result
		);
		OGLPLUS_CHECK(
			GetTextureParameterfv,
			ObjectError,
			Object(*this)
		);
		return Vector<GLfloat, 4>(result, 4);
	}

	/// Sets the texture border color (TEXTURE_BORDER_COLOR)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_BORDER_COLOR}
	 */
	ObjectOps& BorderColor(Vector<GLfloat, 4> color)
	{
		OGLPLUS_GLFUNC(TextureParameterfv)(
			_obj_name(),
			GL_TEXTURE_BORDER_COLOR,
			Data(color)
		);
		OGLPLUS_CHECK(
			TextureParameterfv,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

	/// Gets the texture border color (TEXTURE_BORDER_COLOR)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_BORDER_COLOR}
	 */
	Vector<GLint, 4> BorderColor(TypeTag<GLint>) const
	{
		GLint result[4];
		OGLPLUS_GLFUNC(GetTextureParameterIiv)(
			_obj_name(),
			GL_TEXTURE_BORDER_COLOR,
			result
		);
		OGLPLUS_CHECK(
			GetTextureParameterIiv,
			ObjectError,
			Object(*this)
		);
		return Vector<GLint, 4>(result, 4);
	}

	/// Sets the texture border color (TEXTURE_BORDER_COLOR)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_BORDER_COLOR}
	 */
	ObjectOps& BorderColor(Vector<GLint, 4> color)
	{
		OGLPLUS_GLFUNC(TextureParameterIiv)(
			_obj_name(),
			GL_TEXTURE_BORDER_COLOR,
			Data(color)
		);
		OGLPLUS_CHECK(
			TextureParameterIiv,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

	/// Gets the texture border color (TEXTURE_BORDER_COLOR)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_BORDER_COLOR}
	 */
	Vector<GLuint, 4> BorderColor(TypeTag<GLuint>) const
	{
		GLuint result[4];
		OGLPLUS_GLFUNC(GetTextureParameterIuiv)(
			_obj_name(),
			GL_TEXTURE_BORDER_COLOR,
			result
		);
		OGLPLUS_CHECK(
			GetTextureParameterIuiv,
			ObjectError,
			Object(*this)
		);
		return Vector<GLuint, 4>(result, 4);
	}

	/// Sets the texture border color (TEXTURE_BORDER_COLOR)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_BORDER_COLOR}
	 */
	ObjectOps& BorderColor(Vector<GLuint, 4> color)
	{
		OGLPLUS_GLFUNC(TextureParameterIuiv)(
			_obj_name(),
			GL_TEXTURE_BORDER_COLOR,
			Data(color)
		);
		OGLPLUS_CHECK(
			TextureParameterIuiv,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

	/// Gets the compare mode (TEXTURE_COMPARE_MODE)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_COMPARE_MODE}
	 */
	TextureCompareMode CompareMode(void) const
	{
		return TextureCompareMode(GetIntParam(
			GL_TEXTURE_COMPARE_MODE
		));
	}

	/// Sets the compare mode (TEXTURE_COMPARE_MODE)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_COMPARE_MODE}
	 */
	ObjectOps& CompareMode(TextureCompareMode mode)
	{
		OGLPLUS_GLFUNC(TextureParameteri)(
			_obj_name(),
			GL_TEXTURE_COMPARE_MODE,
			GLenum(mode)
		);
		OGLPLUS_CHECK(
			TextureParameteri,
			ObjectError,
			Object(*this).
			EnumParam(mode)
		);
		return *this;
	}

	/// Sets the compare function (TEXTURE_COMPARE_FUNC)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_COMPARE_FUNC}
	 */
	CompareFunction CompareFunc(void) const
	{
		return CompareFunction(GetIntParam(
			GL_TEXTURE_COMPARE_FUNC
		));
	}

	/// Sets the compare function (TEXTURE_COMPARE_FUNC)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_COMPARE_FUNC}
	 */
	ObjectOps& CompareFunc(CompareFunction func)
	{
		OGLPLUS_GLFUNC(TextureParameteri)(
			_obj_name(),
			GL_TEXTURE_COMPARE_FUNC,
			GLenum(func)
		);
		OGLPLUS_CHECK(
			TextureParameteri,
			ObjectError,
			Object(*this).
			EnumParam(func)
		);
		return *this;
	}

	/// Gets the LOD bias value (TEXTURE_LOD_BIAS)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_LOD_BIAS}
	 */
	GLfloat LODBias(void) const
	{
		return GetFloatParam(GL_TEXTURE_LOD_BIAS);
	}

	/// Sets the LOD bias value (TEXTURE_LOD_BIAS)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_LOD_BIAS}
	 */
	ObjectOps& LODBias(GLfloat value)
	{
		OGLPLUS_GLFUNC(TextureParameterf)(
			_obj_name(),
			GL_TEXTURE_LOD_BIAS,
			value
		);
		OGLPLUS_CHECK(
			TextureParameterf,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

	/// Sets both the minification and maginification filter
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_MIN_FILTER}
	 *  @gldefref{TEXTURE_MAG_FILTER}
	 */
	ObjectOps& Filter(TextureFilter filter)
	{
		OGLPLUS_GLFUNC(TextureParameteri)(
			_obj_name(),
			GL_TEXTURE_MIN_FILTER,
			GLenum(filter)
		);
		OGLPLUS_CHECK(
			TextureParameteri,
			ObjectError,
			Object(*this).
			EnumParam(filter)
		);
		OGLPLUS_GLFUNC(TextureParameteri)(
			_obj_name(),
			GL_TEXTURE_MAG_FILTER,
			GLenum(filter)
		);
		OGLPLUS_CHECK(
			TextureParameteri,
			ObjectError,
			Object(*this).
			EnumParam(filter)
		);
		return *this;
	}

	/// Gets the magnification filter (TEXTURE_MAG_FILTER)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_MAG_FILTER}
	 */
	TextureMagFilter MagFilter(void) const
	{
		return TextureMagFilter(GetIntParam(
			GL_TEXTURE_MAG_FILTER
		));
	}

	/// Sets the magnification filter (TEXTURE_MAG_FILTER)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_MAG_FILTER}
	 */
	ObjectOps& MagFilter(TextureMagFilter filter)
	{
		OGLPLUS_GLFUNC(TextureParameteri)(
			_obj_name(),
			GL_TEXTURE_MAG_FILTER,
			GLenum(filter)
		);
		OGLPLUS_CHECK(
			TextureParameteri,
			ObjectError,
			Object(*this).
			EnumParam(filter)
		);
		return *this;
	}

	/// Gets the minification filter (TEXTURE_MIN_FILTER)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_MIN_FILTER}
	 */
	TextureMinFilter MinFilter(void) const
	{
		return TextureMinFilter(GetIntParam(
			GL_TEXTURE_MIN_FILTER
		));
	}

	/// Sets the minification filter (TEXTURE_MIN_FILTER)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_MIN_FILTER}
	 */
	ObjectOps& MinFilter(TextureMinFilter filter)
	{
		OGLPLUS_GLFUNC(TextureParameteri)(
			_obj_name(),
			GL_TEXTURE_MIN_FILTER,
			GLenum(filter)
		);
		OGLPLUS_CHECK(
			TextureParameteri,
			ObjectError,
			Object(*this).
			EnumParam(filter)
		);
		return *this;
	}

	/// Gets minimal LOD value (TEXTURE_MIN_LOD)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_MIN_LOD}
	 */
	GLfloat MinLOD(void) const
	{
		return GetFloatParam(GL_TEXTURE_MIN_LOD);
	}

	/// Sets minimal LOD value (TEXTURE_MIN_LOD)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_MIN_LOD}
	 */
	ObjectOps& MinLOD(GLfloat value)
	{
		OGLPLUS_GLFUNC(TextureParameterf)(
			_obj_name(),
			GL_TEXTURE_MIN_LOD,
			value
		);
		OGLPLUS_CHECK(
			TextureParameterf,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

	/// Gets maximum LOD value (TEXTURE_MAX_LOD)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_MAX_LOD}
	 */
	GLfloat MaxLOD(void) const
	{
		return GetFloatParam(GL_TEXTURE_MAX_LOD);
	}

	/// Sets maximum LOD value (TEXTURE_MAX_LOD)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_MAX_LOD}
	 */
	ObjectOps& MaxLOD(GLfloat value)
	{
		OGLPLUS_GLFUNC(TextureParameterf)(
			_obj_name(),
			GL_TEXTURE_MAX_LOD,
			value
		);
		OGLPLUS_CHECK(
			TextureParameterf,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

	/// Gets maximum level value (TEXTURE_MAX_LEVEL)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_MAX_LEVEL}
	 */
	GLint MaxLevel(void) const
	{
		return GetIntParam(GL_TEXTURE_MAX_LEVEL);
	}

	/// Sets maximum level value (TEXTURE_MAX_LEVEL)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_MAX_LEVEL}
	 */
	ObjectOps& MaxLevel(GLint value)
	{
		OGLPLUS_GLFUNC(TextureParameteri)(
			_obj_name(),
			GL_TEXTURE_MAX_LEVEL,
			value
		);
		OGLPLUS_CHECK(
			TextureParameteri,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

	/// Gets the maximum anisotropy level
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{MAX_TEXTURE_MAX_ANISOTROPY_EXT}
	 */
	GLfloat MaxAnisotropy(void) const
	{
#ifdef  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT
		return GetFloatParam(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT);
#else
		return GLfloat(1);
#endif
	}

	/// Gets the current anisotropy level
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_MAX_ANISOTROPY_EXT}
	 */
	GLfloat Anisotropy(void) const
	{
#ifdef  GL_TEXTURE_MAX_ANISOTROPY_EXT
		return GetFloatParam(GL_TEXTURE_MAX_ANISOTROPY_EXT);
#else
		return GLfloat(1);
#endif
	}

	/// Sets the anisotropy level
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_MAX_ANISOTROPY_EXT}
	 */
	ObjectOps& Anisotropy(GLfloat value)
	{
#ifdef  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT
		OGLPLUS_GLFUNC(TextureParameterf)(
			_obj_name(),
			GL_TEXTURE_MAX_ANISOTROPY_EXT,
			value
		);
		OGLPLUS_CHECK(
			TextureParameterf,
			ObjectError,
			Object(*this)
		);
#else
		OGLPLUS_FAKE_USE(value);
#endif
		return *this;
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle
	/// Gets the swizzle parameter (TEXTURE_SWIZZLE_*)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 */
	TextureSwizzle Swizzle(TextureSwizzleCoord coord) const
	{
		return TextureSwizzle(GetIntParam(
			GLenum(coord)
		));
	}

	/// Sets the swizzle parameter (TEXTURE_SWIZZLE_*)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 */
	ObjectOps& Swizzle(
		TextureSwizzleCoord coord,
		TextureSwizzle mode
	)
	{
		OGLPLUS_GLFUNC(TextureParameteri)(
			_obj_name(),
			GLenum(coord),
			GLenum(mode)
		);
		OGLPLUS_CHECK(
			TextureParameteri,
			ObjectError,
			Object(*this).
			EnumParam(mode)
		);
		return *this;
	}

	/// Gets the swizzle parameter (TEXTURE_SWIZZLE_R)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_R}
	 */
	TextureSwizzle SwizzleR(void) const
	{
		return Swizzle(TextureSwizzleCoord::R);
	}

	/// Sets the swizzle parameter (TEXTURE_SWIZZLE_R)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_R}
	 */
	ObjectOps& SwizzleR(TextureSwizzle mode)
	{
		return Swizzle(TextureSwizzleCoord::R, mode);
	}

	/// Gets the swizzle parameter (TEXTURE_SWIZZLE_G)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_G}
	 */
	TextureSwizzle SwizzleG(void) const
	{
		return Swizzle(TextureSwizzleCoord::G);
	}

	/// Sets the swizzle parameter (TEXTURE_SWIZZLE_G)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_G}
	 */
	ObjectOps& SwizzleG(TextureSwizzle mode)
	{
		return Swizzle(TextureSwizzleCoord::G, mode);
	}

	/// Gets the swizzle parameter (TEXTURE_SWIZZLE_B)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_B}
	 */
	TextureSwizzle SwizzleB(void) const
	{
		return Swizzle(TextureSwizzleCoord::B);
	}

	/// Sets the swizzle parameter (TEXTURE_SWIZZLE_B)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_B}
	 */
	ObjectOps& SwizzleB(TextureSwizzle mode)
	{
		return Swizzle(TextureSwizzleCoord::B, mode);
	}

	/// Gets the swizzle parameter (TEXTURE_SWIZZLE_A)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_A}
	 */
	TextureSwizzle SwizzleA(void) const
	{
		return Swizzle(TextureSwizzleCoord::A);
	}

	/// Sets the swizzle parameter (TEXTURE_SWIZZLE_A)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_A}
	 */
	ObjectOps& SwizzleA(TextureSwizzle mode)
	{
		return Swizzle(TextureSwizzleCoord::A, mode);
	}

	/// Gets the swizzle parameter (TEXTURE_SWIZZLE_RGBA)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_RGBA}
	 */
	TextureSwizzleTuple SwizzleRGBA(void) const
	{
		TextureSwizzleTuple result;
		OGLPLUS_GLFUNC(GetTextureParameteriv)(
			_obj_name(),
			GL_TEXTURE_SWIZZLE_RGBA,
			result.Values()
		);
		OGLPLUS_CHECK(
			GetTextureParameteriv,
			ObjectError,
			Object(*this)
		);
		return result;
	}

	/// Sets the swizzle parameter (TEXTURE_SWIZZLE_RGBA)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_RGBA}
	 */
	ObjectOps& SwizzleRGBA(TextureSwizzle mode)
	{
		GLint m = GLint(GLenum(mode));
		GLint params[4] = {m, m, m, m};
		OGLPLUS_GLFUNC(TextureParameteriv)(
			_obj_name(),
			GL_TEXTURE_SWIZZLE_RGBA,
			params
		);
		OGLPLUS_CHECK(
			TextureParameteriv,
			ObjectError,
			Object(*this).
			EnumParam(mode)
		);
		return *this;
	}

	/// Sets the swizzle parameter (TEXTURE_SWIZZLE_RGBA)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_RGBA}
	 */
	ObjectOps& SwizzleRGBA(
		TextureSwizzle mode_r,
		TextureSwizzle mode_g,
		TextureSwizzle mode_b,
		TextureSwizzle mode_a
	)
	{
		GLint params[4] = {
			GLint(GLenum(mode_r)),
			GLint(GLenum(mode_g)),
			GLint(GLenum(mode_b)),
			GLint(GLenum(mode_a))
		};
		OGLPLUS_GLFUNC(TextureParameteriv)(
			_obj_name(),
			GL_TEXTURE_SWIZZLE_RGBA,
			params
		);
		OGLPLUS_CHECK(
			TextureParameteriv,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

	/// Sets the swizzle parameter (TEXTURE_SWIZZLE_RGBA)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_RGBA}
	 */
	ObjectOps& SwizzleRGBA(const TextureSwizzleTuple& modes)
	{
		OGLPLUS_GLFUNC(TextureParameteriv)(
			_obj_name(),
			GL_TEXTURE_SWIZZLE_RGBA,
			modes.Values()
		);
		OGLPLUS_CHECK(
			TextureParameteriv,
			ObjectError,
			Object(*this)
		);
		return *this;
	}
#endif // texture swizzle

	/// Gets the wrap parameter (TEXTURE_WRAP_*)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 */
	TextureWrap Wrap(TextureWrapCoord coord) const
	{
		return TextureWrap(GetIntParam(GLenum(coord)));
	}

	/// Sets the wrap parameter (TEXTURE_WRAP_*)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 */
	ObjectOps& Wrap(
		TextureWrapCoord coord,
		TextureWrap mode
	)
	{
		OGLPLUS_GLFUNC(TextureParameteri)(
			_obj_name(),
			GLenum(coord),
			GLenum(mode)
		);
		OGLPLUS_CHECK(
			TextureParameteri,
			ObjectError,
			Object(*this).
			EnumParam(mode)
		);
		return *this;
	}

	/// Gets the wrap parameter (TEXTURE_WRAP_S)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_WRAP_S}
	 */
	TextureWrap WrapS(void) const
	{
		return Wrap(TextureWrapCoord::S);
	}

	/// Sets the wrap parameter (TEXTURE_WRAP_S)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_WRAP_S}
	 */
	ObjectOps& WrapS(TextureWrap mode)
	{
		return Wrap(TextureWrapCoord::S, mode);
	}

	/// Gets the wrap parameter (TEXTURE_WRAP_T)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_WRAP_T}
	 */
	TextureWrap WrapT(void) const
	{
		return Wrap(TextureWrapCoord::T);
	}

	/// Sets the wrap parameter (TEXTURE_WRAP_T)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_WRAP_T}
	 */
	ObjectOps& WrapT(TextureWrap mode)
	{
		return Wrap(TextureWrapCoord::T, mode);
	}

	/// Gets the wrap parameter (TEXTURE_WRAP_R)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_WRAP_R}
	 */
	TextureWrap WrapR(void) const
	{
		return Wrap(TextureWrapCoord::R);
	}

	/// Sets the wrap parameter (TEXTURE_WRAP_R)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_WRAP_R}
	 */
	ObjectOps& WrapR(TextureWrap mode)
	{
		return Wrap(TextureWrapCoord::R, mode);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3
	/// Gets the depth stencil mode parameter (DEPTH_STENCIL_TEXTURE_MODE)
	/**
	 *  @glverreq{4,3}
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{DEPTH_STENCIL_TEXTURE_MODE}
	 */
	PixelDataFormat DepthStencilMode(void) const
	{
		return PixelDataFormat(GetIntParam(
			GL_DEPTH_STENCIL_TEXTURE_MODE
		));
	}

	/// Sets the swizzle parameter (DEPTH_STENCIL_TEXTURE_MODE)
	/**
	 *  @glverreq{4,3}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{DEPTH_STENCIL_TEXTURE_MODE}
	 */
	ObjectOps& DepthStencilMode(PixelDataFormat mode)
	{
		OGLPLUS_GLFUNC(TextureParameteri)(
			_obj_name(),
			GL_DEPTH_STENCIL_TEXTURE_MODE,
			GLenum(mode)
		);
		OGLPLUS_CHECK(
			TextureParameteri,
			ObjectError,
			Object(*this).
			EnumParam(mode)
		);
		return *this;
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_ARB_seamless_cubemap_per_texture
	/// Gets the seamless cubemap setting value
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_CUBE_MAP_SEAMLESS}
	 */
	Boolean Seamless(void) const
	{
		return Boolean(
			GetIntParam(GL_TEXTURE_CUBE_MAP_SEAMLESS),
			std::nothrow
		);
	}

	/// Sets the seamless cubemap setting
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_CUBE_MAP_SEAMLESS}
	 */
	ObjectOps& Seamless(Boolean enable)
	{
		OGLPLUS_GLFUNC(TextureParameteri)(
			_obj_name(),
			GL_TEXTURE_CUBE_MAP_SEAMLESS,
			enable._get()
		);
		OGLPLUS_CHECK(
			TextureParameteri,
			ObjectError,
			Object(*this)
		);
		return *this;
	}
#endif

	/// Generate mipmap for the texture bound to the specified target
	/**
	 *  @glsymbols
	 *  @glfunref{GenerateMipmap}
	 */
	ObjectOps& GenerateMipmap(void)
	{
		OGLPLUS_GLFUNC(GenerateTextureMipmap)(_obj_name());
		OGLPLUS_CHECK(
			GenerateTextureMipmap,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

};

/// Texture operations with direct state access
typedef ObjectOps<tag::DirectState, tag::Texture>
	DSATextureOps;

// Helper class for syntax-sugar operators
struct DSATextureOpsAndSlot
{
	DSATextureOps& tex;
	GLint slot;

	DSATextureOpsAndSlot(DSATextureOps& t, GLint s)
	 : tex(t)
	 , slot(s)
	{ }
};

// syntax sugar operators
inline DSATextureOpsAndSlot operator | (
	DSATextureOps& tex,
	GLuint slot
)
{
	return DSATextureOpsAndSlot(tex, slot);
}

// Bind
inline DSATextureOps& operator << (
	DSATextureOps& tex,
	TextureTarget target
)
{
	tex.Bind(target);
	return tex;
}

// Filter
inline DSATextureOps& operator << (
	DSATextureOps& tex,
	TextureFilter filter
)
{
	tex.Filter(filter);
	return tex;
}

// MinFilter
inline DSATextureOps& operator << (
	DSATextureOps& tex,
	TextureMinFilter filter
)
{
	tex.MinFilter(filter);
	return tex;
}

// MagFilter
inline DSATextureOps& operator << (
	DSATextureOps& tex,
	TextureMagFilter filter
)
{
	tex.MagFilter(filter);
	return tex;
}

// CompareMode
inline DSATextureOps& operator << (
	DSATextureOps& tex,
	TextureCompareMode mode
)
{
	tex.CompareMode(mode);
	return tex;
}

// CompareFunc
inline DSATextureOps& operator << (
	DSATextureOps& tex,
	CompareFunction func
)
{
	tex.CompareFunc(func);
	return tex;
}

// Wrap
inline DSATextureOps& operator << (
	DSATextureOpsAndSlot tas,
	TextureWrap wrap
)
{
	switch(tas.slot)
	{
		case 0: tas.tex.WrapS(wrap); break;
		case 1: tas.tex.WrapT(wrap); break;
		case 2: tas.tex.WrapR(wrap); break;
		default: OGLPLUS_ABORT("Invalid texture wrap slot");
	}
	return tas.tex;
}

// Swizzle
inline DSATextureOps& operator << (
	DSATextureOps& tex,
	TextureSwizzle swizzle
)
{
	tex.SwizzleRGBA(swizzle);
	return tex;
}

// Swizzle
inline DSATextureOps& operator << (
	DSATextureOpsAndSlot tas,
	TextureSwizzle swizzle
)
{
	switch(tas.slot)
	{
		case 0: tas.tex.SwizzleR(swizzle); break;
		case 1: tas.tex.SwizzleG(swizzle); break;
		case 2: tas.tex.SwizzleB(swizzle); break;
		case 3: tas.tex.SwizzleA(swizzle); break;
		default: OGLPLUS_ABORT("Invalid texture swizzle slot");
	}
	return tas.tex;
}

// BorderColor
template <typename T>
inline DSATextureOps& operator << (
	DSATextureOps& tex,
	const Vector<T, 4>& col
)
{
	tex.BorderColor(col);
	return tex;
}

/*
// Image
inline DSATextureOps& operator << (
	DSATextureOps& tex,
	const images::Image& image
)
{
	tex.Image(image);
	return tex;
}

// Image
inline DSATextureOps& operator << (
	DSATextureOps& tex,
	const images::ImageSpec& image_spec
)
{
	tex.Image(image_spec);
	return tex;
}

// Image + Level
inline DSATextureOps& operator << (
	DSATextureOpsAndSlot tas,
	const images::Image& image
)
{
	tas.tex.Image(image, tas.slot);
	return tas.tex;
}

// Image + Level
inline DSATextureOps& operator << (
	DSATextureOpsAndSlot tas,
	const images::ImageSpec& image_spec
)
{
	tas.tex.Image(image_spec, tas.slot);
	return tas.tex;
}
*/

// GenerateMipmaps
inline DSATextureOps& operator << (
	DSATextureOps& tex,
	TextureMipmap
)
{
	tex.GenerateMipmap();
	return tex;
}

/// An @ref oglplus_object encapsulating the DSA texture object functionality
/**
 *  @ingroup oglplus_objects
 */
typedef Object<DSATextureOps> DSATexture;

#endif // GL_ARB_direct_state_access

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/dsa/texture.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

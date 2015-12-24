/**
 *  @file oglplus/dsa/ext/texture.hpp
 *  @brief Texture object wrappers with direct state access
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_DSA_EXT_TEXTURE_1107121519_HPP
#define OGLPLUS_DSA_EXT_TEXTURE_1107121519_HPP

#include <oglplus/texture.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_EXT_direct_state_access

/// Class wrapping default texture functionality with direct state access
/** @note Do not use this class directly, use DSATextureEXT instead.
 *
 */
template <>
class ObjZeroOps<tag::DirectStateEXT, tag::Texture>
 : public ObjCommonOps<tag::Texture>
{
protected:
	ObjZeroOps(TextureName name)
	OGLPLUS_NOEXCEPT(true)
	 : ObjCommonOps<tag::Texture>(name)
	{ }
public:
#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	ObjZeroOps(ObjZeroOps&&) = default;
	ObjZeroOps(const ObjZeroOps&) = default;
	ObjZeroOps& operator = (ObjZeroOps&&) = default;
	ObjZeroOps& operator = (const ObjZeroOps&) = default;
#else
	typedef ObjCommonOps<tag::Texture> _base;

	ObjZeroOps(ObjZeroOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	 : _base(static_cast<_base&&>(temp))
	 , target(temp.target)
	{ }

	ObjZeroOps(const ObjZeroOps& that)
	OGLPLUS_NOEXCEPT(true)
	 : _base(static_cast<const _base&>(that))
	 , target(that.target)
	{ }

	ObjZeroOps& operator = (ObjZeroOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	{
		_base::operator = (static_cast<_base&&>(temp));
		target = temp.target;
		return *this;
	}

	ObjZeroOps& operator = (const ObjZeroOps& that)
	OGLPLUS_NOEXCEPT(true)
	{
		_base::operator = (static_cast<const _base&>(that));
		target = that.target;
		return *this;
	}
#endif
	Target target;

	/// Types related to Texture
	typedef TextureOps::Property Property;

	using ObjCommonOps<tag::Texture>::Bind;
	void Bind(void)
	{
		ObjCommonOps<tag::Texture>::Bind(target);
	}

	/// Bind this texture to target on the specified texture unit
	void BindMulti(TextureUnitSelector index, Target tex_target)
	{
		OGLPLUS_GLFUNC(BindMultiTextureEXT)(
			GLenum(GL_TEXTURE0 + GLuint(index)),
			GLenum(tex_target),
			_obj_name()
		);
		OGLPLUS_VERIFY(
			BindMultiTextureEXT,
			ObjectError,
			Object(*this).
			BindTarget(tex_target).
			Index(GLuint(index))
		);
	}

	void BindMulti(TextureUnitSelector index)
	{
		BindMulti(index, target);
	}

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

	/// Specifies a three dimensional texture image
	/**
	 *  @glsymbols
	 *  @glfunref{TexImage3D}
	 */
	ObjZeroOps& Image3D(
		GLint level,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height,
		SizeType depth,
		GLint border,
		PixelDataFormat format,
		Property::PixDataType type,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(TextureImage3DEXT)(
			_obj_name(),
			GLenum(target),
			level,
			GLint(internal_format),
			width,
			height,
			depth,
			border,
			GLenum(format),
			GLenum(type),
			data
		);
		OGLPLUS_CHECK(
			TextureImage3DEXT,
			ObjectError,
			Object(*this).
			EnumParam(internal_format).
			Index(level)
		);
		return *this;
	}

	/// Specifies a three dimensional texture image
	/**
	 *  @glsymbols
	 *  @glfunref{TexImage3D}
	 */
	ObjZeroOps& Image3D(
		const images::Image& image,
		GLint level = 0,
		GLint border = 0
	);

	/// Specifies a three dimensional texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{TexSubImage3D}
	 */
	ObjZeroOps& SubImage3D(
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
		OGLPLUS_GLFUNC(TextureSubImage3DEXT)(
			_obj_name(),
			GLenum(target),
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
			TextureSubImage3DEXT,
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
	ObjZeroOps& SubImage3D(
		const images::Image& image,
		GLint xoffs,
		GLint yoffs,
		GLint zoffs,
		GLint level = 0
	);

	/// Specifies a two dimensional texture image
	/**
	 *  @glsymbols
	 *  @glfunref{TexImage2D}
	 */
	ObjZeroOps& Image2D(
		TextureTarget tex_target,
		GLint level,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height,
		GLint border,
		PixelDataFormat format,
		Property::PixDataType type,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(TextureImage2DEXT)(
			_obj_name(),
			GLenum(tex_target),
			level,
			GLint(internal_format),
			width,
			height,
			border,
			GLenum(format),
			GLenum(type),
			data
		);
		OGLPLUS_CHECK(
			TextureImage2DEXT,
			ObjectError,
			Object(*this).
			BindTarget(tex_target).
			EnumParam(internal_format).
			Index(level)
		);
		return *this;
	}

	ObjZeroOps& Image2D(
		GLint level,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height,
		GLint border,
		PixelDataFormat format,
		Property::PixDataType type,
		const void* data
	)
	{
		return Image2D(
			target,
			level,
			internal_format,
			width,
			height,
			border,
			format,
			type,
			data
		);
	}

	/// Specifies a two dimensional texture image
	/**
	 *  @glsymbols
	 *  @glfunref{TexImage2D}
	 */
	ObjZeroOps& Image2D(
		TextureTarget tex_target,
		const images::Image& image,
		GLint level = 0,
		GLint border = 0
	);

	/// Specifies a two dimensional texture image
	/**
	 *  @glsymbols
	 *  @glfunref{TexImage2D}
	 */
	ObjZeroOps& Image2D(
		const images::Image& image,
		GLint level = 0,
		GLint border = 0
	)
	{
		return Image2D(target, image, level, border);
	}

	/// Specifies the image of the specified cube-map face
	/**
	 *  @pre (face >= 0) && (face <= 5)
	 *
	 *  @glsymbols
	 *  @glfunref{TexImage2D}
	 *  @gldefref{TEXTURE_CUBE_MAP_POSITIVE_X}
	 *  @gldefref{TEXTURE_CUBE_MAP_NEGATIVE_X}
	 *  @gldefref{TEXTURE_CUBE_MAP_POSITIVE_Y}
	 *  @gldefref{TEXTURE_CUBE_MAP_NEGATIVE_Y}
	 *  @gldefref{TEXTURE_CUBE_MAP_POSITIVE_Z}
	 *  @gldefref{TEXTURE_CUBE_MAP_NEGATIVE_Z}
	 */
	ObjZeroOps& ImageCM(
		GLuint face,
		GLint level,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height,
		GLint border,
		PixelDataFormat format,
		Property::PixDataType type,
		const void* data
	)
	{
		assert(face <= 5);
		OGLPLUS_GLFUNC(TextureImage2DEXT)(
			_obj_name(),
			GLenum(GL_TEXTURE_CUBE_MAP_POSITIVE_X+face),
			level,
			GLint(internal_format),
			width,
			height,
			border,
			GLenum(format),
			GLenum(type),
			data
		);
		OGLPLUS_CHECK(
			TextureImage2DEXT,
			ObjectError,
			Object(*this).
			BindTarget(TextureTarget(GL_TEXTURE_CUBE_MAP_POSITIVE_X+face)).
			EnumParam(internal_format).
			Index(level)
		);
		return *this;
	}

	/// Specifies the image of the specified cube-map face
	/**
	 *  @pre (face >= 0) && (face <= 5)
	 *
	 *  @glsymbols
	 *  @glfunref{TexImage2D}
	 *  @gldefref{TEXTURE_CUBE_MAP_POSITIVE_X}
	 *  @gldefref{TEXTURE_CUBE_MAP_NEGATIVE_X}
	 *  @gldefref{TEXTURE_CUBE_MAP_POSITIVE_Y}
	 *  @gldefref{TEXTURE_CUBE_MAP_NEGATIVE_Y}
	 *  @gldefref{TEXTURE_CUBE_MAP_POSITIVE_Z}
	 *  @gldefref{TEXTURE_CUBE_MAP_NEGATIVE_Z}
	 */
	ObjZeroOps& ImageCM(
		GLuint face,
		const images::Image& image,
		GLint level = 0,
		GLint border = 0
	)
	{
		assert(face <= 5);
		return Image2D(
			Target(GL_TEXTURE_CUBE_MAP_POSITIVE_X+face),
			image,
			level,
			border
		);
	}

	/// Specifies a two dimensional texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{TexSubImage2D}
	 */
	ObjZeroOps& SubImage2D(
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
		OGLPLUS_GLFUNC(TextureSubImage2DEXT)(
			_obj_name(),
			GLenum(target),
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
			TextureSubImage2DEXT,
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
	ObjZeroOps& SubImage2D(
		const images::Image& image,
		GLint xoffs,
		GLint yoffs,
		GLint level = 0
	);

	/// Specifies a one dimensional texture image
	/**
	 *  @glsymbols
	 *  @glfunref{TexImage1D}
	 */
	ObjZeroOps& Image1D(
		GLint level,
		PixelDataInternalFormat internal_format,
		SizeType width,
		GLint border,
		PixelDataFormat format,
		Property::PixDataType type,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(TextureImage1DEXT)(
			_obj_name(),
			GLenum(target),
			level,
			GLint(internal_format),
			width,
			border,
			GLenum(format),
			GLenum(type),
			data
		);
		OGLPLUS_CHECK(
			TextureImage1DEXT,
			ObjectError,
			Object(*this).
			EnumParam(internal_format).
			Index(level)
		);
		return *this;
	}

	/// Specifies a one dimensional texture image
	/**
	 *  @glsymbols
	 *  @glfunref{TexImage1D}
	 */
	ObjZeroOps& Image1D(
		const images::Image& image,
		GLint level = 0,
		GLint border = 0
	);

	/// Specifies a one dimensional texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{TexSubImage1D}
	 */
	ObjZeroOps& SubImage1D(
		GLint level,
		GLint xoffs,
		SizeType width,
		PixelDataFormat format,
		Property::PixDataType type,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(TextureSubImage1DEXT)(
			_obj_name(),
			GLenum(target),
			level,
			xoffs,
			width,
			GLenum(format),
			GLenum(type),
			data
		);
		OGLPLUS_CHECK(
			TextureSubImage1DEXT,
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
	ObjZeroOps& SubImage1D(
		const images::Image& image,
		GLint xoffs,
		GLint level = 0
	);

	/// Specifies a texture image
	/**
	 *  @glsymbols
	 *  @glfunref{TexImage3D}
	 *  @glfunref{TexImage2D}
	 *  @glfunref{TexImage1D}
	 */
	ObjZeroOps& Image(
		Target tex_target,
		const images::Image& image,
		GLint level = 0,
		GLint border = 0
	);

	/// Specifies a texture image
	/**
	 *  @glsymbols
	 *  @glfunref{TexImage3D}
	 *  @glfunref{TexImage2D}
	 *  @glfunref{TexImage1D}
	 */
	ObjZeroOps& Image(
		const images::Image& image,
		GLint level = 0,
		GLint border = 0
	)
	{
		return Image(target, image, level, border);
	}

	/// Specifies a texture image
	/**
	 *  @glsymbols
	 *  @glfunref{TexImage3D}
	 *  @glfunref{TexImage2D}
	 *  @glfunref{TexImage1D}
	 */
	ObjZeroOps& Image(
		Target tex_target,
		const images::ImageSpec& image_spec,
		GLint level = 0,
		GLint border = 0
	);

	/// Specifies a texture image
	/**
	 *  @glsymbols
	 *  @glfunref{TexImage3D}
	 *  @glfunref{TexImage2D}
	 *  @glfunref{TexImage1D}
	 */
	ObjZeroOps& Image(
		const images::ImageSpec& image_spec,
		GLint level = 0,
		GLint border = 0
	)
	{
		return Image(target, image_spec, level, border);
	}

	/// Copies a two dimensional texture image from the framebuffer
	/**
	 *  @glsymbols
	 *  @glfunref{CopyTexImage2D}
	 */
	ObjZeroOps& CopyImage2D(
		GLint level,
		PixelDataInternalFormat internal_format,
		GLint x,
		GLint y,
		SizeType width,
		SizeType height,
		GLint border
	)
	{
		OGLPLUS_GLFUNC(CopyTextureImage2DEXT)(
			_obj_name(),
			GLenum(target),
			level,
			GLenum(internal_format),
			x,
			y,
			width,
			height,
			border
		);
		OGLPLUS_CHECK(
			CopyTextureImage2DEXT,
			ObjectError,
			Object(*this).
			EnumParam(internal_format).
			Index(level)
		);
		return *this;
	}

	/// Copies a one dimensional texture image from the framebuffer
	/**
	 *  @glsymbols
	 *  @glfunref{CopyTexImage1D}
	 */
	ObjZeroOps& CopyImage1D(
		GLint level,
		PixelDataInternalFormat internal_format,
		GLint x,
		GLint y,
		SizeType width,
		GLint border
	)
	{
		OGLPLUS_GLFUNC(CopyTextureImage1DEXT)(
			_obj_name(),
			GLenum(target),
			level,
			GLenum(internal_format),
			x,
			y,
			width,
			border
		);
		OGLPLUS_CHECK(
			CopyTextureImage1DEXT,
			ObjectError,
			Object(*this).
			EnumParam(internal_format).
			Index(level)
		);
		return *this;
	}

	/// Copies a three dimensional texture sub image from the framebuffer
	/**
	 *  @glsymbols
	 *  @glfunref{CopyTexSubImage3D}
	 */
	ObjZeroOps& CopySubImage3D(
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
		OGLPLUS_GLFUNC(CopyTextureSubImage3DEXT)(
			_obj_name(),
			GLenum(target),
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
			CopyTextureSubImage3DEXT,
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
	ObjZeroOps& CopySubImage2D(
		GLint level,
		GLint xoffs,
		GLint yoffs,
		GLint x,
		GLint y,
		SizeType width,
		SizeType height
	)
	{
		OGLPLUS_GLFUNC(CopyTextureSubImage2DEXT)(
			_obj_name(),
			GLenum(target),
			level,
			xoffs,
			yoffs,
			x,
			y,
			width,
			height
		);
		OGLPLUS_CHECK(
			CopyTextureSubImage2DEXT,
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
	ObjZeroOps& CopySubImage1D(
		GLint level,
		GLint xoffs,
		GLint x,
		GLint y,
		SizeType width
	)
	{
		OGLPLUS_GLFUNC(CopyTextureSubImage1DEXT)(
			_obj_name(),
			GLenum(target),
			level,
			xoffs,
			x,
			y,
			width
		);
		OGLPLUS_CHECK(
			CopyTextureSubImage1DEXT,
			ObjectError,
			Object(*this).
			Index(level)
		);
		return *this;
	}

	/// Specifies a three dimensional compressed texture image
	/**
	 *  @glsymbols
	 *  @glfunref{CompressedTexImage3D}
	 */
	ObjZeroOps& CompressedImage3D(
		GLint level,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height,
		SizeType depth,
		GLint border,
		SizeType image_size,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(CompressedTextureImage3DEXT)(
			_obj_name(),
			GLenum(target),
			level,
			GLenum(internal_format),
			width,
			height,
			depth,
			border,
			image_size,
			data
		);
		OGLPLUS_CHECK(
			CompressedTextureImage3DEXT,
			ObjectError,
			Object(*this).
			EnumParam(internal_format).
			Index(level)
		);
		return *this;
	}

	/// Specifies a two dimensional compressed texture image
	/**
	 *  @glsymbols
	 *  @glfunref{CompressedTexImage2D}
	 */
	ObjZeroOps& CompressedImage2D(
		GLint level,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height,
		GLint border,
		SizeType image_size,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(CompressedTextureImage2DEXT)(
			_obj_name(),
			GLenum(target),
			level,
			GLenum(internal_format),
			width,
			height,
			border,
			image_size,
			data
		);
		OGLPLUS_CHECK(
			CompressedTextureImage2DEXT,
			ObjectError,
			Object(*this).
			EnumParam(internal_format).
			Index(level)
		);
		return *this;
	}

	/// Specifies a one dimensional compressed texture image
	/**
	 *  @glsymbols
	 *  @glfunref{CompressedTexImage1D}
	 */
	ObjZeroOps& CompressedImage1D(
		GLint level,
		PixelDataInternalFormat internal_format,
		SizeType width,
		GLint border,
		SizeType image_size,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(CompressedTextureImage1DEXT)(
			_obj_name(),
			GLenum(target),
			level,
			GLenum(internal_format),
			width,
			border,
			image_size,
			data
		);
		OGLPLUS_CHECK(
			CompressedTextureImage1DEXT,
			ObjectError,
			Object(*this).
			EnumParam(internal_format).
			Index(level)
		);
		return *this;
	}

	/// Specifies a three dimensional compressed texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{CompressedTexSubImage3D}
	 */
	ObjZeroOps& CompressedSubImage3D(
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
		OGLPLUS_GLFUNC(CompressedTextureSubImage3DEXT)(
			_obj_name(),
			GLenum(target),
			level,
			xoffs,
			yoffs,
			zoffs,
			width,
			height,
			depth,
			GLenum(format),
			image_size,
			data
		);
		OGLPLUS_CHECK(
			CompressedTextureSubImage3DEXT,
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
	ObjZeroOps& CompressedSubImage2D(
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
		OGLPLUS_GLFUNC(CompressedTextureSubImage2DEXT)(
			_obj_name(),
			GLenum(target),
			level,
			xoffs,
			yoffs,
			width,
			height,
			GLenum(format),
			image_size,
			data
		);
		OGLPLUS_CHECK(
			CompressedTextureSubImage2DEXT,
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
	ObjZeroOps& CompressedSubImage1D(
		GLint level,
		GLint xoffs,
		SizeType width,
		PixelDataFormat format,
		SizeType image_size,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(CompressedTextureSubImage1DEXT)(
			_obj_name(),
			GLenum(target),
			level,
			xoffs,
			width,
			GLenum(format),
			image_size,
			data
		);
		OGLPLUS_CHECK(
			CompressedTextureSubImage1DEXT,
			ObjectError,
			Object(*this).
			EnumParam(format).
			Index(level)
		);
		return *this;
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_1
	/// Assigns a buffer storing the texel data to the texture
	/**
	 *  @glverreq{3,1}
	 *  @glsymbols
	 *  @glfunref{TexBuffer}
	 */
	ObjZeroOps& Buffer(
		PixelDataInternalFormat internal_format,
		BufferName buffer
	)
	{
		OGLPLUS_GLFUNC(TextureBufferEXT)(
			_obj_name(),
			GLenum(target),
			GLenum(internal_format),
			GetGLName(buffer)
		);
		OGLPLUS_CHECK(
			TextureBufferEXT,
			ObjectPairError,
			Subject(buffer).
			Object(*this).
			EnumParam(internal_format)
		);
		return *this;
	}
#endif

	/// Returns the texture base level (TEXTURE_BASE_LEVEL)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
	 *  @gldefref{TEXTURE_BASE_LEVEL}
	 */
	GLuint BaseLevel(void) const
	{
		return GLuint(GetIntParam(GL_TEXTURE_BASE_LEVEL));
	}

	/// Sets the texture base level (TEXTURE_BASE_LEVEL)
	/**
	 *  @glsymbols
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_BASE_LEVEL}
	 */
	ObjZeroOps& BaseLevel(GLuint level)
	{
		OGLPLUS_GLFUNC(TextureParameteriEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_BASE_LEVEL,
			GLint(level)
		);
		OGLPLUS_CHECK(
			TextureParameteriEXT,
			ObjectError,
			Object(*this).
			Index(level)
		);
		return *this;
	}

	/// Gets the texture border color (TEXTURE_BORDER_COLOR)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
	 *  @gldefref{TEXTURE_BORDER_COLOR}
	 */
	Vector<GLfloat, 4> BorderColor(TypeTag<GLfloat>) const
	{
		GLfloat result[4];
		OGLPLUS_GLFUNC(GetTextureParameterfvEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_BORDER_COLOR,
			result
		);
		OGLPLUS_CHECK(
			GetTextureParameterfvEXT,
			ObjectError,
			Object(*this)
		);
		return Vector<GLfloat, 4>(result, 4);
	}

	/// Sets the texture border color (TEXTURE_BORDER_COLOR)
	/**
	 *  @glsymbols
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_BORDER_COLOR}
	 */
	ObjZeroOps& BorderColor(Vector<GLfloat, 4> color)
	{
		OGLPLUS_GLFUNC(TextureParameterfvEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_BORDER_COLOR,
			Data(color)
		);
		OGLPLUS_CHECK(
			TextureParameterfvEXT,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

	/// Gets the texture border color (TEXTURE_BORDER_COLOR)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
	 *  @gldefref{TEXTURE_BORDER_COLOR}
	 */
	Vector<GLint, 4> BorderColor(TypeTag<GLint>) const
	{
		GLint result[4];
		OGLPLUS_GLFUNC(GetTextureParameterIivEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_BORDER_COLOR,
			result
		);
		OGLPLUS_CHECK(
			GetTextureParameterIivEXT,
			ObjectError,
			Object(*this)
		);
		return Vector<GLint, 4>(result, 4);
	}

	/// Sets the texture border color (TEXTURE_BORDER_COLOR)
	/**
	 *  @glsymbols
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_BORDER_COLOR}
	 */
	ObjZeroOps& BorderColor(Vector<GLint, 4> color)
	{
		OGLPLUS_GLFUNC(TextureParameterIivEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_BORDER_COLOR,
			Data(color)
		);
		OGLPLUS_CHECK(
			TextureParameterIivEXT,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

	/// Gets the texture border color (TEXTURE_BORDER_COLOR)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
	 *  @gldefref{TEXTURE_BORDER_COLOR}
	 */
	Vector<GLuint, 4> BorderColor(TypeTag<GLuint>) const
	{
		GLuint result[4];
		OGLPLUS_GLFUNC(GetTextureParameterIuivEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_BORDER_COLOR,
			result
		);
		OGLPLUS_CHECK(
			GetTextureParameterIuivEXT,
			ObjectError,
			Object(*this)
		);
		return Vector<GLuint, 4>(result, 4);
	}

	/// Sets the texture border color (TEXTURE_BORDER_COLOR)
	/**
	 *  @glsymbols
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_BORDER_COLOR}
	 */
	ObjZeroOps& BorderColor(Vector<GLuint, 4> color)
	{
		OGLPLUS_GLFUNC(TextureParameterIuivEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_BORDER_COLOR,
			Data(color)
		);
		OGLPLUS_CHECK(
			TextureParameterIuivEXT,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

	/// Gets the compare mode (TEXTURE_COMPARE_MODE)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
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
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_COMPARE_MODE}
	 */
	ObjZeroOps& CompareMode(TextureCompareMode mode)
	{
		OGLPLUS_GLFUNC(TextureParameteriEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_COMPARE_MODE,
			GLint(mode)
		);
		OGLPLUS_CHECK(
			TextureParameteriEXT,
			ObjectError,
			Object(*this).
			EnumParam(mode)
		);
		return *this;
	}

	/// Sets the compare function (TEXTURE_COMPARE_FUNC)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
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
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_COMPARE_FUNC}
	 */
	ObjZeroOps& CompareFunc(CompareFunction func)
	{
		OGLPLUS_GLFUNC(TextureParameteriEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_COMPARE_FUNC,
			GLint(func)
		);
		OGLPLUS_CHECK(
			TextureParameteriEXT,
			ObjectError,
			Object(*this).
			EnumParam(func)
		);
		return *this;
	}

	/// Gets the LOD bias value (TEXTURE_LOD_BIAS)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
	 *  @gldefref{TEXTURE_LOD_BIAS}
	 */
	GLfloat LODBias(void) const
	{
		return GetFloatParam(GL_TEXTURE_LOD_BIAS);
	}

	/// Sets the LOD bias value (TEXTURE_LOD_BIAS)
	/**
	 *  @glsymbols
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_LOD_BIAS}
	 */
	ObjZeroOps& LODBias(GLfloat value)
	{
		OGLPLUS_GLFUNC(TextureParameterfEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_LOD_BIAS,
			value
		);
		OGLPLUS_CHECK(
			TextureParameterfEXT,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

	/// Sets both the minification and maginification filter
	/**
	 *  @glsymbols
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_MIN_FILTER}
	 *  @gldefref{TEXTURE_MAG_FILTER}
	 */
	ObjZeroOps& Filter(TextureFilter filter)
	{
		OGLPLUS_GLFUNC(TextureParameteriEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_MIN_FILTER,
			GLint(filter)
		);
		OGLPLUS_CHECK(
			TextureParameteriEXT,
			ObjectError,
			Object(*this).
			EnumParam(filter)
		);
		OGLPLUS_GLFUNC(TextureParameteriEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_MAG_FILTER,
			GLint(filter)
		);
		OGLPLUS_CHECK(
			TextureParameteriEXT,
			ObjectError,
			Object(*this).
			EnumParam(filter)
		);
		return *this;
	}

	/// Gets the magnification filter (TEXTURE_MAG_FILTER)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
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
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_MAG_FILTER}
	 */
	ObjZeroOps& MagFilter(TextureMagFilter filter)
	{
		OGLPLUS_GLFUNC(TextureParameteriEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_MAG_FILTER,
			GLint(filter)
		);
		OGLPLUS_CHECK(
			TextureParameteriEXT,
			ObjectError,
			Object(*this).
			EnumParam(filter)
		);
		return *this;
	}

	/// Gets the minification filter (TEXTURE_MIN_FILTER)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
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
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_MIN_FILTER}
	 */
	ObjZeroOps& MinFilter(TextureMinFilter filter)
	{
		OGLPLUS_GLFUNC(TextureParameteriEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_MIN_FILTER,
			GLint(filter)
		);
		OGLPLUS_CHECK(
			TextureParameteriEXT,
			ObjectError,
			Object(*this).
			EnumParam(filter)
		);
		return *this;
	}

	/// Gets minimal LOD value (TEXTURE_MIN_LOD)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
	 *  @gldefref{TEXTURE_MIN_LOD}
	 */
	GLfloat MinLOD(void) const
	{
		return GetFloatParam(GL_TEXTURE_MIN_LOD);
	}

	/// Sets minimal LOD value (TEXTURE_MIN_LOD)
	/**
	 *  @glsymbols
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_MIN_LOD}
	 */
	ObjZeroOps& MinLOD(GLfloat value)
	{
		OGLPLUS_GLFUNC(TextureParameterfEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_MIN_LOD,
			value
		);
		OGLPLUS_CHECK(
			TextureParameterfEXT,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

	/// Gets maximum LOD value (TEXTURE_MAX_LOD)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
	 *  @gldefref{TEXTURE_MAX_LOD}
	 */
	GLfloat MaxLOD(void) const
	{
		return GetFloatParam(GL_TEXTURE_MAX_LOD);
	}

	/// Sets maximum LOD value (TEXTURE_MAX_LOD)
	/**
	 *  @glsymbols
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_MAX_LOD}
	 */
	ObjZeroOps& MaxLOD(GLfloat value)
	{
		OGLPLUS_GLFUNC(TextureParameterfEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_MAX_LOD,
			value
		);
		OGLPLUS_CHECK(
			TextureParameterfEXT,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

	/// Gets maximum level value (TEXTURE_MAX_LEVEL)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
	 *  @gldefref{TEXTURE_MAX_LEVEL}
	 */
	GLint MaxLevel(void) const
	{
		return GetIntParam(GL_TEXTURE_MAX_LEVEL);
	}

	/// Sets maximum level value (TEXTURE_MAX_LEVEL)
	/**
	 *  @glsymbols
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_MAX_LEVEL}
	 */
	ObjZeroOps& MaxLevel(GLint value)
	{
		OGLPLUS_GLFUNC(TextureParameteriEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_MAX_LEVEL,
			value
		);
		OGLPLUS_CHECK(
			TextureParameteriEXT,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

	/// Gets the maximum anisotropy level
	/**
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
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
	 *  @glfunref{GetTextureParameter}
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
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_MAX_ANISOTROPY_EXT}
	 */
	ObjZeroOps& Anisotropy(GLfloat value)
	{
#ifdef  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT
		OGLPLUS_GLFUNC(TextureParameterfEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_MAX_ANISOTROPY_EXT,
			value
		);
		OGLPLUS_CHECK(
			TextureParameterfEXT,
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
	 *  @glfunref{GetTextureParameter}
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
	 *  @glfunref{TextureParameter}
	 */
	ObjZeroOps& Swizzle(
		TextureSwizzleCoord coord,
		TextureSwizzle mode
	)
	{
		OGLPLUS_GLFUNC(TextureParameteriEXT)(
			_obj_name(),
			GLenum(target),
			GLenum(coord),
			GLint(mode)
		);
		OGLPLUS_CHECK(
			TextureParameteriEXT,
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
	 *  @glfunref{GetTextureParameter}
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
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_R}
	 */
	ObjZeroOps& SwizzleR(TextureSwizzle mode)
	{
		return Swizzle(TextureSwizzleCoord::R, mode);
	}

	/// Gets the swizzle parameter (TEXTURE_SWIZZLE_G)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
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
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_G}
	 */
	ObjZeroOps& SwizzleG(TextureSwizzle mode)
	{
		return Swizzle(TextureSwizzleCoord::G, mode);
	}

	/// Gets the swizzle parameter (TEXTURE_SWIZZLE_B)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
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
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_B}
	 */
	ObjZeroOps& SwizzleB(TextureSwizzle mode)
	{
		return Swizzle(TextureSwizzleCoord::B, mode);
	}

	/// Gets the swizzle parameter (TEXTURE_SWIZZLE_A)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
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
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_A}
	 */
	ObjZeroOps& SwizzleA(TextureSwizzle mode)
	{
		return Swizzle(TextureSwizzleCoord::A, mode);
	}

	/// Gets the swizzle parameter (TEXTURE_SWIZZLE_RGBA)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_RGBA}
	 */
	TextureSwizzleTuple SwizzleRGBA(void) const
	{
		TextureSwizzleTuple result;
		OGLPLUS_GLFUNC(GetTextureParameterivEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_SWIZZLE_RGBA,
			result.Values()
		);
		OGLPLUS_CHECK(
			GetTextureParameterivEXT,
			ObjectError,
			Object(*this)
		);
		return result;
	}

	/// Sets the swizzle parameter (TEXTURE_SWIZZLE_RGBA)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_RGBA}
	 */
	ObjZeroOps& SwizzleRGBA(TextureSwizzle mode)
	{
		GLint m = GLint(GLenum(mode));
		GLint params[4] = {m, m, m, m};
		OGLPLUS_GLFUNC(TextureParameterivEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_SWIZZLE_RGBA,
			params
		);
		OGLPLUS_CHECK(
			TextureParameterivEXT,
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
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_RGBA}
	 */
	ObjZeroOps& SwizzleRGBA(
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
		OGLPLUS_GLFUNC(TextureParameterivEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_SWIZZLE_RGBA,
			params
		);
		OGLPLUS_CHECK(
			TextureParameterivEXT,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

	/// Sets the swizzle parameter (TEXTURE_SWIZZLE_RGBA)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_RGBA}
	 */
	ObjZeroOps& SwizzleRGBA(const TextureSwizzleTuple& modes)
	{
		OGLPLUS_GLFUNC(TextureParameterivEXT)(
			_obj_name(),
			GLenum(target),
			GL_TEXTURE_SWIZZLE_RGBA,
			modes.Values()
		);
		OGLPLUS_CHECK(
			TextureParameterivEXT,
			ObjectError,
			Object(*this)
		);
		return *this;
	}
#endif // texture swizzle

	/// Gets the wrap parameter (TEXTURE_WRAP_*)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
	 */
	TextureWrap Wrap(TextureWrapCoord coord) const
	{
		return TextureWrap(GetIntParam(GLenum(coord)));
	}

	/// Sets the wrap parameter (TEXTURE_WRAP_*)
	/**
	 *  @glsymbols
	 *  @glfunref{TextureParameter}
	 */
	ObjZeroOps& Wrap(
		TextureWrapCoord coord,
		TextureWrap mode
	)
	{
		OGLPLUS_GLFUNC(TextureParameteriEXT)(
			_obj_name(),
			GLenum(target),
			GLenum(coord),
			GLint(mode)
		);
		OGLPLUS_CHECK(
			TextureParameteriEXT,
			ObjectError,
			Object(*this).
			EnumParam(mode)
		);
		return *this;
	}

	/// Gets the wrap parameter (TEXTURE_WRAP_S)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
	 *  @gldefref{TEXTURE_WRAP_S}
	 */
	TextureWrap WrapS(void) const
	{
		return Wrap(TextureWrapCoord::S);
	}

	/// Sets the wrap parameter (TEXTURE_WRAP_S)
	/**
	 *  @glsymbols
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_WRAP_S}
	 */
	ObjZeroOps& WrapS(TextureWrap mode)
	{
		return Wrap(TextureWrapCoord::S, mode);
	}

	/// Gets the wrap parameter (TEXTURE_WRAP_T)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
	 *  @gldefref{TEXTURE_WRAP_T}
	 */
	TextureWrap WrapT(void) const
	{
		return Wrap(TextureWrapCoord::T);
	}

	/// Sets the wrap parameter (TEXTURE_WRAP_T)
	/**
	 *  @glsymbols
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_WRAP_T}
	 */
	ObjZeroOps& WrapT(TextureWrap mode)
	{
		return Wrap(TextureWrapCoord::T, mode);
	}

	/// Gets the wrap parameter (TEXTURE_WRAP_R)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
	 *  @gldefref{TEXTURE_WRAP_R}
	 */
	TextureWrap WrapR(void) const
	{
		return Wrap(TextureWrapCoord::R);
	}

	/// Sets the wrap parameter (TEXTURE_WRAP_R)
	/**
	 *  @glsymbols
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_WRAP_R}
	 */
	ObjZeroOps& WrapR(TextureWrap mode)
	{
		return Wrap(TextureWrapCoord::R, mode);
	}

	/// Sets the wrap parameter (TEXTURE_WRAP_*)
	/**
	 *  @glsymbols
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_WRAP_R}
	 *  @gldefref{TEXTURE_WRAP_T}
	 *  @gldefref{TEXTURE_WRAP_S}
	 */
	ObjZeroOps& Wrap(TextureWrap mode)
	{
		switch(TextureTargetDimensions(target))
		{
			case 3: WrapR(mode);
				OGLPLUS_FALLTHROUGH
			case 2: WrapT(mode);
				OGLPLUS_FALLTHROUGH
			case 1: WrapS(mode);
				OGLPLUS_FALLTHROUGH
			case 0: break;
			default: OGLPLUS_ABORT("Invalid texture wrap dimension");
		}
		return *this;
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3
	/// Gets the depth stencil mode parameter (DEPTH_STENCIL_TEXTURE_MODE)
	/**
	 *  @glverreq{4,3}
	 *  @glsymbols
	 *  @glfunref{GetTextureParameter}
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
	 *  @glfunref{TextureParameter}
	 *  @gldefref{DEPTH_STENCIL_TEXTURE_MODE}
	 */
	ObjZeroOps& DepthStencilMode(PixelDataFormat mode)
	{
		OGLPLUS_GLFUNC(TextureParameteriEXT)(
			_obj_name(),
			GLenum(target),
			GL_DEPTH_STENCIL_TEXTURE_MODE,
			GLint(mode)
		);
		OGLPLUS_CHECK(
			TextureParameteriEXT,
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
	 *  @glfunref{GetTextureParameter}
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
	 *  @glfunref{TextureParameter}
	 *  @gldefref{TEXTURE_CUBE_MAP_SEAMLESS}
	 */
	ObjZeroOps& Seamless(Boolean enable)
	{
		OGLPLUS_GLFUNC(TextureParameteriEXT)(
			GLenum(target),
			_obj_name(),
			GL_TEXTURE_CUBE_MAP_SEAMLESS,
			enable._get()
		);
		OGLPLUS_CHECK(
			TextureParameteriEXT,
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
	ObjZeroOps& GenerateMipmap(void)
	{
		OGLPLUS_GLFUNC(GenerateTextureMipmapEXT)(
			_obj_name(),
			GLenum(target)
		);
		OGLPLUS_CHECK(
			GenerateTextureMipmapEXT,
			ObjectError,
			Object(*this)
		);
		return *this;
	}

};

/// Default Texture operations with direct state access
typedef ObjZeroOps<tag::DirectStateEXT, tag::Texture>
	DSADefaultTextureOpsEXT;

/// Texture operations with direct state access
typedef ObjectOps<tag::DirectStateEXT, tag::Texture>
	DSATextureOpsEXT;

// Helper class for syntax-sugar operators
struct DSATextureOpsAndSlotEXT
{
	DSATextureOpsEXT& tex;
	GLint slot;

	DSATextureOpsAndSlotEXT(DSATextureOpsEXT& t, GLint s)
	 : tex(t)
	 , slot(s)
	{ }
};

// syntax sugar operators
inline DSATextureOpsAndSlotEXT operator | (
	DSATextureOpsEXT& tex,
	GLint slot
)
{
	return DSATextureOpsAndSlotEXT(tex, slot);
}

// Bind
inline DSATextureOpsEXT& operator << (
	DSATextureOpsEXT& tex,
	TextureTarget target
)
{
	tex.target = target;
	tex.Bind();
	return tex;
}

// BindMulti
inline DSATextureOpsEXT& operator << (
	DSATextureOpsEXT& tex,
	TextureUnitAndTarget uat
)
{
	tex.target = uat.tgt;
	tex.BindMulti(uat.unit);
	return tex;
}

// Filter
inline DSATextureOpsEXT& operator << (
	DSATextureOpsEXT& tex,
	TextureFilter filter
)
{
	tex.Filter(filter);
	return tex;
}

// MinFilter
inline DSATextureOpsEXT& operator << (
	DSATextureOpsEXT& tex,
	TextureMinFilter filter
)
{
	tex.MinFilter(filter);
	return tex;
}

// MagFilter
inline DSATextureOpsEXT& operator << (
	DSATextureOpsEXT& tex,
	TextureMagFilter filter
)
{
	tex.MagFilter(filter);
	return tex;
}

// CompareMode
inline DSATextureOpsEXT& operator << (
	DSATextureOpsEXT& tex,
	TextureCompareMode mode
)
{
	tex.CompareMode(mode);
	return tex;
}

// CompareFunc
inline DSATextureOpsEXT& operator << (
	DSATextureOpsEXT& tex,
	CompareFunction func
)
{
	tex.CompareFunc(func);
	return tex;
}

// Wrap
inline DSATextureOpsEXT& operator << (
	DSATextureOpsEXT& tex,
	TextureWrap wrap
)
{
	switch(TextureTargetDimensions(tex.target))
	{
		case 3: tex.WrapR(wrap); OGLPLUS_FALLTHROUGH
		case 2: tex.WrapT(wrap); OGLPLUS_FALLTHROUGH
		case 1: tex.WrapS(wrap); OGLPLUS_FALLTHROUGH
		case 0: break;
		default: OGLPLUS_ABORT("Invalid texture wrap dimension");
	}
	return tex;
}

// Wrap
inline DSATextureOpsEXT& operator << (
	DSATextureOpsAndSlotEXT tas,
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
inline DSATextureOpsEXT& operator << (
	DSATextureOpsEXT& tex,
	TextureSwizzle swizzle
)
{
	tex.SwizzleRGBA(swizzle);
	return tex;
}

// Swizzle
inline DSATextureOpsEXT& operator << (
	DSATextureOpsAndSlotEXT tas,
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
inline DSATextureOpsEXT& operator << (
	DSATextureOpsEXT& tex,
	const Vector<T, 4>& col
)
{
	tex.BorderColor(col);
	return tex;
}

// Image
inline DSATextureOpsEXT& operator << (
	DSATextureOpsEXT& tex,
	const images::Image& image
)
{
	tex.Image(image);
	return tex;
}

// Image
inline DSATextureOpsEXT& operator << (
	DSATextureOpsEXT& tex,
	const images::ImageSpec& image_spec
)
{
	tex.Image(image_spec);
	return tex;
}

// Image + Level
inline DSATextureOpsEXT& operator << (
	DSATextureOpsAndSlotEXT tas,
	const images::Image& image
)
{
	tas.tex.Image(image, tas.slot);
	return tas.tex;
}

// Image + Level
inline DSATextureOpsEXT& operator << (
	DSATextureOpsAndSlotEXT tas,
	const images::ImageSpec& image_spec
)
{
	tas.tex.Image(image_spec, tas.slot);
	return tas.tex;
}

// GenerateMipmaps
inline DSATextureOpsEXT& operator << (
	DSATextureOpsEXT& tex,
	TextureMipmap
)
{
	tex.GenerateMipmap();
	return tex;
}

/// An @ref oglplus_object encapsulating the DSA default texture functionality
/**
 *  @ingroup oglplus_objects
 */
typedef ObjectZero<DSADefaultTextureOpsEXT> DSADefaultTextureEXT;

/// An @ref oglplus_object encapsulating the DSA texture object functionality
/**
 *  @ingroup oglplus_objects
 */
typedef Object<DSATextureOpsEXT> DSATextureEXT;

#else
#error Direct State Access Textures not available
#endif // GL_EXT_direct_state_access

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/dsa/ext/texture.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

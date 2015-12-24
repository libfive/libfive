
/**
 *  @file oglplus/bound/texture.hpp
 *  @brief Specialization of ObjectOps for Texture.
 *
 *  Automatically generated file, do not edit manually!
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */


#ifndef OGLPLUS_BOUND_TEXTURE_1107121519_HPP
#define OGLPLUS_BOUND_TEXTURE_1107121519_HPP

#include <oglplus/object/bound.hpp>
#include <oglplus/texture.hpp>
#include <utility>

namespace oglplus {

/// Specialization of the BoundObjOps for Texture  >.
/** This template implements wrappers around the member functions
 *  of Texture, which have
 *  a TextureTarget parameter
 *  specifying the binding point on which they should operate.
 *
 *  @note Do not use this template class directly use
 *  Bound < Texture > or the Context::Current()
 *  function instead.
 *
 *  @ingroup utility_classes
 */
template <>
class BoundObjOps<tag::Texture>
{
private:
	typedef ObjectOps<tag::ExplicitSel, tag::Texture> ExplicitOps;
public:
	typedef ExplicitOps::Target Target;
	Target target;

	BoundObjOps(void)
	{ }

	BoundObjOps(Target init_tgt)
	 : target(init_tgt)
	{ }

	/** Wrapper for Texture::GetIntParam()
	 *  @see Texture::GetIntParam()
	 */
	GLint GetIntParam(
		GLenum query
	) const
	{
		return ExplicitOps::GetIntParam(
			this->target,
			query
		);
	}


	/** Wrapper for Texture::GetFloatParam()
	 *  @see Texture::GetFloatParam()
	 */
	GLfloat GetFloatParam(
		GLenum query
	) const
	{
		return ExplicitOps::GetFloatParam(
			this->target,
			query
		);
	}


	/** Wrapper for Texture::GetIntParam()
	 *  @see Texture::GetIntParam()
	 */
	GLint GetIntParam(
		GLint level,
		GLenum query
	) const
	{
		return ExplicitOps::GetIntParam(
			this->target,
			level,
			query
		);
	}


	/** Wrapper for Texture::GetFloatParam()
	 *  @see Texture::GetFloatParam()
	 */
	GLfloat GetFloatParam(
		GLint level,
		GLenum query
	) const
	{
		return ExplicitOps::GetFloatParam(
			this->target,
			level,
			query
		);
	}


	/** Wrapper for Texture::Width()
	 *  @see Texture::Width()
	 */
	SizeType Width(
		GLint level = 0
	) const
	{
		return ExplicitOps::Width(
			this->target,
			level
		);
	}


	/** Wrapper for Texture::Height()
	 *  @see Texture::Height()
	 */
	SizeType Height(
		GLint level = 0
	) const
	{
		return ExplicitOps::Height(
			this->target,
			level
		);
	}


	/** Wrapper for Texture::Depth()
	 *  @see Texture::Depth()
	 */
	SizeType Depth(
		GLint level = 0
	) const
	{
		return ExplicitOps::Depth(
			this->target,
			level
		);
	}


	/** Wrapper for Texture::RedType()
	 *  @see Texture::RedType()
	 */
	PixelDataType RedType(
		GLint level = 0
	) const
	{
		return ExplicitOps::RedType(
			this->target,
			level
		);
	}


	/** Wrapper for Texture::GreenType()
	 *  @see Texture::GreenType()
	 */
	PixelDataType GreenType(
		GLint level = 0
	) const
	{
		return ExplicitOps::GreenType(
			this->target,
			level
		);
	}


	/** Wrapper for Texture::BlueType()
	 *  @see Texture::BlueType()
	 */
	PixelDataType BlueType(
		GLint level = 0
	) const
	{
		return ExplicitOps::BlueType(
			this->target,
			level
		);
	}


	/** Wrapper for Texture::AlphaType()
	 *  @see Texture::AlphaType()
	 */
	PixelDataType AlphaType(
		GLint level = 0
	) const
	{
		return ExplicitOps::AlphaType(
			this->target,
			level
		);
	}


	/** Wrapper for Texture::DepthType()
	 *  @see Texture::DepthType()
	 */
	PixelDataType DepthType(
		GLint level = 0
	) const
	{
		return ExplicitOps::DepthType(
			this->target,
			level
		);
	}


	/** Wrapper for Texture::RedSize()
	 *  @see Texture::RedSize()
	 */
	SizeType RedSize(
		GLint level = 0
	) const
	{
		return ExplicitOps::RedSize(
			this->target,
			level
		);
	}


	/** Wrapper for Texture::GreenSize()
	 *  @see Texture::GreenSize()
	 */
	SizeType GreenSize(
		GLint level = 0
	) const
	{
		return ExplicitOps::GreenSize(
			this->target,
			level
		);
	}


	/** Wrapper for Texture::BlueSize()
	 *  @see Texture::BlueSize()
	 */
	SizeType BlueSize(
		GLint level = 0
	) const
	{
		return ExplicitOps::BlueSize(
			this->target,
			level
		);
	}


	/** Wrapper for Texture::AlphaSize()
	 *  @see Texture::AlphaSize()
	 */
	SizeType AlphaSize(
		GLint level = 0
	) const
	{
		return ExplicitOps::AlphaSize(
			this->target,
			level
		);
	}


	/** Wrapper for Texture::DepthSize()
	 *  @see Texture::DepthSize()
	 */
	SizeType DepthSize(
		GLint level = 0
	) const
	{
		return ExplicitOps::DepthSize(
			this->target,
			level
		);
	}


	/** Wrapper for Texture::StencilSize()
	 *  @see Texture::StencilSize()
	 */
	SizeType StencilSize(
		GLint level = 0
	) const
	{
		return ExplicitOps::StencilSize(
			this->target,
			level
		);
	}


	/** Wrapper for Texture::SharedSize()
	 *  @see Texture::SharedSize()
	 */
	SizeType SharedSize(
		GLint level = 0
	) const
	{
		return ExplicitOps::SharedSize(
			this->target,
			level
		);
	}


	/** Wrapper for Texture::CompressedImageSize()
	 *  @see Texture::CompressedImageSize()
	 */
	SizeType CompressedImageSize(
		GLint level = 0
	) const
	{
		return ExplicitOps::CompressedImageSize(
			this->target,
			level
		);
	}


	/** Wrapper for Texture::InternalFormat()
	 *  @see Texture::InternalFormat()
	 */
	PixelDataInternalFormat InternalFormat(
		GLint level = 0
	) const
	{
		return ExplicitOps::InternalFormat(
			this->target,
			level
		);
	}


	/** Wrapper for Texture::GetImage()
	 *  @see Texture::GetImage()
	 */
	const BoundObjOps& GetImage(
		GLint level,
		PixelDataFormat format,
		const OutputData & dest
	) const
	{
		ExplicitOps::GetImage(
			this->target,
			level,
			format,
			dest
		);
		return *this;
	}


	/** Wrapper for Texture::GetImage()
	 *  @see Texture::GetImage()
	 */
	const BoundObjOps& GetImage(
		GLint level,
		PixelDataFormat format,
		ExplicitOps::Property::PixDataType type,
		SizeType size,
		GLvoid * buffer
	) const
	{
		ExplicitOps::GetImage(
			this->target,
			level,
			format,
			type,
			size,
			buffer
		);
		return *this;
	}


	/** Wrapper for Texture::GetCompressedImage()
	 *  @see Texture::GetCompressedImage()
	 */
	const BoundObjOps& GetCompressedImage(
		GLint level,
		const OutputData & dest
	) const
	{
		ExplicitOps::GetCompressedImage(
			this->target,
			level,
			dest
		);
		return *this;
	}


	/** Wrapper for Texture::GetCompressedImage()
	 *  @see Texture::GetCompressedImage()
	 */
	const BoundObjOps& GetCompressedImage(
		GLint level,
		SizeType size,
		GLubyte * buffer
	) const
	{
		ExplicitOps::GetCompressedImage(
			this->target,
			level,
			size,
			buffer
		);
		return *this;
	}


	/** Wrapper for Texture::GetCompressedImage()
	 *  @see Texture::GetCompressedImage()
	 */
	const BoundObjOps& GetCompressedImage(
		GLint level,
		std::vector< GLubyte > & dest
	) const
	{
		ExplicitOps::GetCompressedImage(
			this->target,
			level,
			dest
		);
		return *this;
	}


	/** Wrapper for Texture::Image3D()
	 *  @see Texture::Image3D()
	 */
	const BoundObjOps& Image3D(
		GLint level,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height,
		SizeType depth,
		GLint border,
		PixelDataFormat format,
		ExplicitOps::Property::PixDataType type,
		const void * data
	) const
	{
		ExplicitOps::Image3D(
			this->target,
			level,
			internal_format,
			width,
			height,
			depth,
			border,
			format,
			type,
			data
		);
		return *this;
	}


	/** Wrapper for Texture::Image3D()
	 *  @see Texture::Image3D()
	 */
	const BoundObjOps& Image3D(
		const images::Image & image,
		GLint level = 0,
		GLint border = 0
	) const
	{
		ExplicitOps::Image3D(
			this->target,
			image,
			level,
			border
		);
		return *this;
	}


	/** Wrapper for Texture::SubImage3D()
	 *  @see Texture::SubImage3D()
	 */
	const BoundObjOps& SubImage3D(
		GLint level,
		GLint xoffs,
		GLint yoffs,
		GLint zoffs,
		SizeType width,
		SizeType height,
		SizeType depth,
		PixelDataFormat format,
		ExplicitOps::Property::PixDataType type,
		const void * data
	) const
	{
		ExplicitOps::SubImage3D(
			this->target,
			level,
			xoffs,
			yoffs,
			zoffs,
			width,
			height,
			depth,
			format,
			type,
			data
		);
		return *this;
	}


	/** Wrapper for Texture::SubImage3D()
	 *  @see Texture::SubImage3D()
	 */
	const BoundObjOps& SubImage3D(
		const images::Image & image,
		GLint xoffs,
		GLint yoffs,
		GLint zoffs,
		GLint level = 0
	) const
	{
		ExplicitOps::SubImage3D(
			this->target,
			image,
			xoffs,
			yoffs,
			zoffs,
			level
		);
		return *this;
	}


	/** Wrapper for Texture::Image2D()
	 *  @see Texture::Image2D()
	 */
	const BoundObjOps& Image2D(
		GLint level,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height,
		GLint border,
		PixelDataFormat format,
		ExplicitOps::Property::PixDataType type,
		const void * data
	) const
	{
		ExplicitOps::Image2D(
			this->target,
			level,
			internal_format,
			width,
			height,
			border,
			format,
			type,
			data
		);
		return *this;
	}


	/** Wrapper for Texture::Image2D()
	 *  @see Texture::Image2D()
	 */
	const BoundObjOps& Image2D(
		const images::Image & image,
		GLint level = 0,
		GLint border = 0
	) const
	{
		ExplicitOps::Image2D(
			this->target,
			image,
			level,
			border
		);
		return *this;
	}


	/** Wrapper for Texture::SubImage2D()
	 *  @see Texture::SubImage2D()
	 */
	const BoundObjOps& SubImage2D(
		GLint level,
		GLint xoffs,
		GLint yoffs,
		SizeType width,
		SizeType height,
		PixelDataFormat format,
		ExplicitOps::Property::PixDataType type,
		const void * data
	) const
	{
		ExplicitOps::SubImage2D(
			this->target,
			level,
			xoffs,
			yoffs,
			width,
			height,
			format,
			type,
			data
		);
		return *this;
	}


	/** Wrapper for Texture::SubImage2D()
	 *  @see Texture::SubImage2D()
	 */
	const BoundObjOps& SubImage2D(
		const images::Image & image,
		GLint xoffs,
		GLint yoffs,
		GLint level = 0
	) const
	{
		ExplicitOps::SubImage2D(
			this->target,
			image,
			xoffs,
			yoffs,
			level
		);
		return *this;
	}


	/** Wrapper for Texture::Image1D()
	 *  @see Texture::Image1D()
	 */
	const BoundObjOps& Image1D(
		GLint level,
		PixelDataInternalFormat internal_format,
		SizeType width,
		GLint border,
		PixelDataFormat format,
		ExplicitOps::Property::PixDataType type,
		const void * data
	) const
	{
		ExplicitOps::Image1D(
			this->target,
			level,
			internal_format,
			width,
			border,
			format,
			type,
			data
		);
		return *this;
	}


	/** Wrapper for Texture::Image1D()
	 *  @see Texture::Image1D()
	 */
	const BoundObjOps& Image1D(
		const images::Image & image,
		GLint level = 0,
		GLint border = 0
	) const
	{
		ExplicitOps::Image1D(
			this->target,
			image,
			level,
			border
		);
		return *this;
	}


	/** Wrapper for Texture::SubImage1D()
	 *  @see Texture::SubImage1D()
	 */
	const BoundObjOps& SubImage1D(
		GLint level,
		GLint xoffs,
		SizeType width,
		PixelDataFormat format,
		ExplicitOps::Property::PixDataType type,
		const void * data
	) const
	{
		ExplicitOps::SubImage1D(
			this->target,
			level,
			xoffs,
			width,
			format,
			type,
			data
		);
		return *this;
	}


	/** Wrapper for Texture::SubImage1D()
	 *  @see Texture::SubImage1D()
	 */
	const BoundObjOps& SubImage1D(
		const images::Image & image,
		GLint xoffs,
		GLint level = 0
	) const
	{
		ExplicitOps::SubImage1D(
			this->target,
			image,
			xoffs,
			level
		);
		return *this;
	}


	/** Wrapper for Texture::Image()
	 *  @see Texture::Image()
	 */
	const BoundObjOps& Image(
		const images::Image & image,
		GLint level = 0,
		GLint border = 0
	) const
	{
		ExplicitOps::Image(
			this->target,
			image,
			level,
			border
		);
		return *this;
	}


	/** Wrapper for Texture::Image()
	 *  @see Texture::Image()
	 */
	const BoundObjOps& Image(
		const images::ImageSpec & image_spec,
		GLint level = 0,
		GLint border = 0
	) const
	{
		ExplicitOps::Image(
			this->target,
			image_spec,
			level,
			border
		);
		return *this;
	}


	/** Wrapper for Texture::CopyImage2D()
	 *  @see Texture::CopyImage2D()
	 */
	const BoundObjOps& CopyImage2D(
		GLint level,
		PixelDataInternalFormat internal_format,
		GLint x,
		GLint y,
		SizeType width,
		SizeType height,
		GLint border
	) const
	{
		ExplicitOps::CopyImage2D(
			this->target,
			level,
			internal_format,
			x,
			y,
			width,
			height,
			border
		);
		return *this;
	}


	/** Wrapper for Texture::CopyImage1D()
	 *  @see Texture::CopyImage1D()
	 */
	const BoundObjOps& CopyImage1D(
		GLint level,
		PixelDataInternalFormat internal_format,
		GLint x,
		GLint y,
		SizeType width,
		GLint border
	) const
	{
		ExplicitOps::CopyImage1D(
			this->target,
			level,
			internal_format,
			x,
			y,
			width,
			border
		);
		return *this;
	}


	/** Wrapper for Texture::CopySubImage3D()
	 *  @see Texture::CopySubImage3D()
	 */
	const BoundObjOps& CopySubImage3D(
		GLint level,
		GLint xoffs,
		GLint yoffs,
		GLint zoffs,
		GLint x,
		GLint y,
		SizeType width,
		SizeType height
	) const
	{
		ExplicitOps::CopySubImage3D(
			this->target,
			level,
			xoffs,
			yoffs,
			zoffs,
			x,
			y,
			width,
			height
		);
		return *this;
	}


	/** Wrapper for Texture::CopySubImage2D()
	 *  @see Texture::CopySubImage2D()
	 */
	const BoundObjOps& CopySubImage2D(
		GLint level,
		GLint xoffs,
		GLint yoffs,
		GLint x,
		GLint y,
		SizeType width,
		SizeType height
	) const
	{
		ExplicitOps::CopySubImage2D(
			this->target,
			level,
			xoffs,
			yoffs,
			x,
			y,
			width,
			height
		);
		return *this;
	}


	/** Wrapper for Texture::CopySubImage1D()
	 *  @see Texture::CopySubImage1D()
	 */
	const BoundObjOps& CopySubImage1D(
		GLint level,
		GLint xoffs,
		GLint x,
		GLint y,
		SizeType width
	) const
	{
		ExplicitOps::CopySubImage1D(
			this->target,
			level,
			xoffs,
			x,
			y,
			width
		);
		return *this;
	}


	/** Wrapper for Texture::CompressedImage3D()
	 *  @see Texture::CompressedImage3D()
	 */
	const BoundObjOps& CompressedImage3D(
		GLint level,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height,
		SizeType depth,
		GLint border,
		SizeType image_size,
		const void * data
	) const
	{
		ExplicitOps::CompressedImage3D(
			this->target,
			level,
			internal_format,
			width,
			height,
			depth,
			border,
			image_size,
			data
		);
		return *this;
	}


	/** Wrapper for Texture::CompressedImage2D()
	 *  @see Texture::CompressedImage2D()
	 */
	const BoundObjOps& CompressedImage2D(
		GLint level,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height,
		GLint border,
		SizeType image_size,
		const void * data
	) const
	{
		ExplicitOps::CompressedImage2D(
			this->target,
			level,
			internal_format,
			width,
			height,
			border,
			image_size,
			data
		);
		return *this;
	}


	/** Wrapper for Texture::CompressedImage1D()
	 *  @see Texture::CompressedImage1D()
	 */
	const BoundObjOps& CompressedImage1D(
		GLint level,
		PixelDataInternalFormat internal_format,
		SizeType width,
		GLint border,
		SizeType image_size,
		const void * data
	) const
	{
		ExplicitOps::CompressedImage1D(
			this->target,
			level,
			internal_format,
			width,
			border,
			image_size,
			data
		);
		return *this;
	}


	/** Wrapper for Texture::CompressedSubImage3D()
	 *  @see Texture::CompressedSubImage3D()
	 */
	const BoundObjOps& CompressedSubImage3D(
		GLint level,
		GLint xoffs,
		GLint yoffs,
		GLint zoffs,
		SizeType width,
		SizeType height,
		SizeType depth,
		PixelDataFormat format,
		SizeType image_size,
		const void * data
	) const
	{
		ExplicitOps::CompressedSubImage3D(
			this->target,
			level,
			xoffs,
			yoffs,
			zoffs,
			width,
			height,
			depth,
			format,
			image_size,
			data
		);
		return *this;
	}


	/** Wrapper for Texture::CompressedSubImage2D()
	 *  @see Texture::CompressedSubImage2D()
	 */
	const BoundObjOps& CompressedSubImage2D(
		GLint level,
		GLint xoffs,
		GLint yoffs,
		SizeType width,
		SizeType height,
		PixelDataFormat format,
		SizeType image_size,
		const void * data
	) const
	{
		ExplicitOps::CompressedSubImage2D(
			this->target,
			level,
			xoffs,
			yoffs,
			width,
			height,
			format,
			image_size,
			data
		);
		return *this;
	}


	/** Wrapper for Texture::CompressedSubImage1D()
	 *  @see Texture::CompressedSubImage1D()
	 */
	const BoundObjOps& CompressedSubImage1D(
		GLint level,
		GLint xoffs,
		SizeType width,
		PixelDataFormat format,
		SizeType image_size,
		const void * data
	) const
	{
		ExplicitOps::CompressedSubImage1D(
			this->target,
			level,
			xoffs,
			width,
			format,
			image_size,
			data
		);
		return *this;
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_2 || GL_ARB_texture_multisample

	/** Wrapper for Texture::Image3DMultisample()
	 *  @see Texture::Image3DMultisample()
	 */
	const BoundObjOps& Image3DMultisample(
		SizeType samples,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height,
		SizeType depth,
		Boolean fixed_sample_locations
	) const
	{
		ExplicitOps::Image3DMultisample(
			this->target,
			samples,
			internal_format,
			width,
			height,
			depth,
			fixed_sample_locations
		);
		return *this;
	}
#endif // GL_VERSION_3_2 GL_ARB_texture_multisample

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_2 || GL_ARB_texture_multisample

	/** Wrapper for Texture::Image2DMultisample()
	 *  @see Texture::Image2DMultisample()
	 */
	const BoundObjOps& Image2DMultisample(
		SizeType samples,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height,
		Boolean fixed_sample_locations
	) const
	{
		ExplicitOps::Image2DMultisample(
			this->target,
			samples,
			internal_format,
			width,
			height,
			fixed_sample_locations
		);
		return *this;
	}
#endif // GL_VERSION_3_2 GL_ARB_texture_multisample

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_1

	/** Wrapper for Texture::Buffer()
	 *  @see Texture::Buffer()
	 */
	const BoundObjOps& Buffer(
		PixelDataInternalFormat internal_format,
		BufferName buffer
	) const
	{
		ExplicitOps::Buffer(
			this->target,
			internal_format,
			buffer
		);
		return *this;
	}
#endif // GL_VERSION_3_1

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3

	/** Wrapper for Texture::BufferRange()
	 *  @see Texture::BufferRange()
	 */
	const BoundObjOps& BufferRange(
		PixelDataInternalFormat internal_format,
		BufferName buffer,
		GLintptr offset,
		BigSizeType size
	) const
	{
		ExplicitOps::BufferRange(
			this->target,
			internal_format,
			buffer,
			offset,
			size
		);
		return *this;
	}
#endif // GL_VERSION_4_3

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_2 || GL_ARB_texture_storage

	/** Wrapper for Texture::Storage1D()
	 *  @see Texture::Storage1D()
	 */
	const BoundObjOps& Storage1D(
		SizeType levels,
		PixelDataInternalFormat internal_format,
		SizeType width
	) const
	{
		ExplicitOps::Storage1D(
			this->target,
			levels,
			internal_format,
			width
		);
		return *this;
	}
#endif // GL_VERSION_4_2 GL_ARB_texture_storage

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_2 || GL_ARB_texture_storage

	/** Wrapper for Texture::Storage2D()
	 *  @see Texture::Storage2D()
	 */
	const BoundObjOps& Storage2D(
		SizeType levels,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height
	) const
	{
		ExplicitOps::Storage2D(
			this->target,
			levels,
			internal_format,
			width,
			height
		);
		return *this;
	}
#endif // GL_VERSION_4_2 GL_ARB_texture_storage

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_2 || GL_ARB_texture_storage

	/** Wrapper for Texture::Storage3D()
	 *  @see Texture::Storage3D()
	 */
	const BoundObjOps& Storage3D(
		SizeType levels,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height,
		SizeType depth
	) const
	{
		ExplicitOps::Storage3D(
			this->target,
			levels,
			internal_format,
			width,
			height,
			depth
		);
		return *this;
	}
#endif // GL_VERSION_4_2 GL_ARB_texture_storage


	/** Wrapper for Texture::BaseLevel()
	 *  @see Texture::BaseLevel()
	 */
	GLuint BaseLevel(void) const
	{
		return ExplicitOps::BaseLevel(
			this->target
		);
	}


	/** Wrapper for Texture::BaseLevel()
	 *  @see Texture::BaseLevel()
	 */
	const BoundObjOps& BaseLevel(
		GLint level
	) const
	{
		ExplicitOps::BaseLevel(
			this->target,
			level
		);
		return *this;
	}


	/** Wrapper for Texture::BorderColor()
	 *  @see Texture::BorderColor()
	 */
	Vector< GLfloat, 4 > BorderColor(
		TypeTag< GLfloat > _auto_param_2
	) const
	{
		return ExplicitOps::BorderColor(
			this->target,
			_auto_param_2
		);
	}


	/** Wrapper for Texture::BorderColor()
	 *  @see Texture::BorderColor()
	 */
	const BoundObjOps& BorderColor(
		Vector< GLfloat, 4 > color
	) const
	{
		ExplicitOps::BorderColor(
			this->target,
			color
		);
		return *this;
	}


	/** Wrapper for Texture::BorderColor()
	 *  @see Texture::BorderColor()
	 */
	Vector< GLint, 4 > BorderColor(
		TypeTag< GLint > _auto_param_2
	) const
	{
		return ExplicitOps::BorderColor(
			this->target,
			_auto_param_2
		);
	}


	/** Wrapper for Texture::BorderColor()
	 *  @see Texture::BorderColor()
	 */
	const BoundObjOps& BorderColor(
		Vector< GLint, 4 > color
	) const
	{
		ExplicitOps::BorderColor(
			this->target,
			color
		);
		return *this;
	}


	/** Wrapper for Texture::BorderColor()
	 *  @see Texture::BorderColor()
	 */
	Vector< GLuint, 4 > BorderColor(
		TypeTag< GLuint > _auto_param_2
	) const
	{
		return ExplicitOps::BorderColor(
			this->target,
			_auto_param_2
		);
	}


	/** Wrapper for Texture::BorderColor()
	 *  @see Texture::BorderColor()
	 */
	const BoundObjOps& BorderColor(
		Vector< GLuint, 4 > color
	) const
	{
		ExplicitOps::BorderColor(
			this->target,
			color
		);
		return *this;
	}


	/** Wrapper for Texture::CompareMode()
	 *  @see Texture::CompareMode()
	 */
	TextureCompareMode CompareMode(void) const
	{
		return ExplicitOps::CompareMode(
			this->target
		);
	}


	/** Wrapper for Texture::CompareMode()
	 *  @see Texture::CompareMode()
	 */
	const BoundObjOps& CompareMode(
		TextureCompareMode mode
	) const
	{
		ExplicitOps::CompareMode(
			this->target,
			mode
		);
		return *this;
	}


	/** Wrapper for Texture::CompareFunc()
	 *  @see Texture::CompareFunc()
	 */
	CompareFunction CompareFunc(void) const
	{
		return ExplicitOps::CompareFunc(
			this->target
		);
	}


	/** Wrapper for Texture::CompareFunc()
	 *  @see Texture::CompareFunc()
	 */
	const BoundObjOps& CompareFunc(
		CompareFunction func
	) const
	{
		ExplicitOps::CompareFunc(
			this->target,
			func
		);
		return *this;
	}


	/** Wrapper for Texture::LODBias()
	 *  @see Texture::LODBias()
	 */
	GLfloat LODBias(void) const
	{
		return ExplicitOps::LODBias(
			this->target
		);
	}


	/** Wrapper for Texture::LODBias()
	 *  @see Texture::LODBias()
	 */
	const BoundObjOps& LODBias(
		GLfloat value
	) const
	{
		ExplicitOps::LODBias(
			this->target,
			value
		);
		return *this;
	}


	/** Wrapper for Texture::Filter()
	 *  @see Texture::Filter()
	 */
	const BoundObjOps& Filter(
		TextureFilter filter
	) const
	{
		ExplicitOps::Filter(
			this->target,
			filter
		);
		return *this;
	}


	/** Wrapper for Texture::MagFilter()
	 *  @see Texture::MagFilter()
	 */
	TextureMagFilter MagFilter(void) const
	{
		return ExplicitOps::MagFilter(
			this->target
		);
	}


	/** Wrapper for Texture::MagFilter()
	 *  @see Texture::MagFilter()
	 */
	const BoundObjOps& MagFilter(
		TextureMagFilter filter
	) const
	{
		ExplicitOps::MagFilter(
			this->target,
			filter
		);
		return *this;
	}


	/** Wrapper for Texture::MinFilter()
	 *  @see Texture::MinFilter()
	 */
	TextureMinFilter MinFilter(void) const
	{
		return ExplicitOps::MinFilter(
			this->target
		);
	}


	/** Wrapper for Texture::MinFilter()
	 *  @see Texture::MinFilter()
	 */
	const BoundObjOps& MinFilter(
		TextureMinFilter filter
	) const
	{
		ExplicitOps::MinFilter(
			this->target,
			filter
		);
		return *this;
	}


	/** Wrapper for Texture::MinLOD()
	 *  @see Texture::MinLOD()
	 */
	GLfloat MinLOD(void) const
	{
		return ExplicitOps::MinLOD(
			this->target
		);
	}


	/** Wrapper for Texture::MinLOD()
	 *  @see Texture::MinLOD()
	 */
	const BoundObjOps& MinLOD(
		GLfloat value
	) const
	{
		ExplicitOps::MinLOD(
			this->target,
			value
		);
		return *this;
	}


	/** Wrapper for Texture::MaxLOD()
	 *  @see Texture::MaxLOD()
	 */
	GLfloat MaxLOD(void) const
	{
		return ExplicitOps::MaxLOD(
			this->target
		);
	}


	/** Wrapper for Texture::MaxLOD()
	 *  @see Texture::MaxLOD()
	 */
	const BoundObjOps& MaxLOD(
		GLfloat value
	) const
	{
		ExplicitOps::MaxLOD(
			this->target,
			value
		);
		return *this;
	}


	/** Wrapper for Texture::MaxLevel()
	 *  @see Texture::MaxLevel()
	 */
	GLint MaxLevel(void) const
	{
		return ExplicitOps::MaxLevel(
			this->target
		);
	}


	/** Wrapper for Texture::MaxLevel()
	 *  @see Texture::MaxLevel()
	 */
	const BoundObjOps& MaxLevel(
		GLint value
	) const
	{
		ExplicitOps::MaxLevel(
			this->target,
			value
		);
		return *this;
	}


	/** Wrapper for Texture::MaxAnisotropy()
	 *  @see Texture::MaxAnisotropy()
	 */
	GLfloat MaxAnisotropy(void) const
	{
		return ExplicitOps::MaxAnisotropy(
			this->target
		);
	}


	/** Wrapper for Texture::Anisotropy()
	 *  @see Texture::Anisotropy()
	 */
	GLfloat Anisotropy(void) const
	{
		return ExplicitOps::Anisotropy(
			this->target
		);
	}


	/** Wrapper for Texture::Anisotropy()
	 *  @see Texture::Anisotropy()
	 */
	const BoundObjOps& Anisotropy(
		GLfloat value
	) const
	{
		ExplicitOps::Anisotropy(
			this->target,
			value
		);
		return *this;
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle

	/** Wrapper for Texture::Swizzle()
	 *  @see Texture::Swizzle()
	 */
	TextureSwizzle Swizzle(
		TextureSwizzleCoord coord
	) const
	{
		return ExplicitOps::Swizzle(
			this->target,
			coord
		);
	}
#endif // GL_VERSION_3_3 GL_ARB_texture_swizzle

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle

	/** Wrapper for Texture::Swizzle()
	 *  @see Texture::Swizzle()
	 */
	const BoundObjOps& Swizzle(
		TextureSwizzleCoord coord,
		TextureSwizzle mode
	) const
	{
		ExplicitOps::Swizzle(
			this->target,
			coord,
			mode
		);
		return *this;
	}
#endif // GL_VERSION_3_3 GL_ARB_texture_swizzle

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle

	/** Wrapper for Texture::SwizzleR()
	 *  @see Texture::SwizzleR()
	 */
	TextureSwizzle SwizzleR(void) const
	{
		return ExplicitOps::SwizzleR(
			this->target
		);
	}
#endif // GL_VERSION_3_3 GL_ARB_texture_swizzle

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle

	/** Wrapper for Texture::SwizzleR()
	 *  @see Texture::SwizzleR()
	 */
	const BoundObjOps& SwizzleR(
		TextureSwizzle mode
	) const
	{
		ExplicitOps::SwizzleR(
			this->target,
			mode
		);
		return *this;
	}
#endif // GL_VERSION_3_3 GL_ARB_texture_swizzle

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle

	/** Wrapper for Texture::SwizzleG()
	 *  @see Texture::SwizzleG()
	 */
	TextureSwizzle SwizzleG(void) const
	{
		return ExplicitOps::SwizzleG(
			this->target
		);
	}
#endif // GL_VERSION_3_3 GL_ARB_texture_swizzle

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle

	/** Wrapper for Texture::SwizzleG()
	 *  @see Texture::SwizzleG()
	 */
	const BoundObjOps& SwizzleG(
		TextureSwizzle mode
	) const
	{
		ExplicitOps::SwizzleG(
			this->target,
			mode
		);
		return *this;
	}
#endif // GL_VERSION_3_3 GL_ARB_texture_swizzle

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle

	/** Wrapper for Texture::SwizzleB()
	 *  @see Texture::SwizzleB()
	 */
	TextureSwizzle SwizzleB(void) const
	{
		return ExplicitOps::SwizzleB(
			this->target
		);
	}
#endif // GL_VERSION_3_3 GL_ARB_texture_swizzle

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle

	/** Wrapper for Texture::SwizzleB()
	 *  @see Texture::SwizzleB()
	 */
	const BoundObjOps& SwizzleB(
		TextureSwizzle mode
	) const
	{
		ExplicitOps::SwizzleB(
			this->target,
			mode
		);
		return *this;
	}
#endif // GL_VERSION_3_3 GL_ARB_texture_swizzle

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle

	/** Wrapper for Texture::SwizzleA()
	 *  @see Texture::SwizzleA()
	 */
	TextureSwizzle SwizzleA(void) const
	{
		return ExplicitOps::SwizzleA(
			this->target
		);
	}
#endif // GL_VERSION_3_3 GL_ARB_texture_swizzle

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle

	/** Wrapper for Texture::SwizzleA()
	 *  @see Texture::SwizzleA()
	 */
	const BoundObjOps& SwizzleA(
		TextureSwizzle mode
	) const
	{
		ExplicitOps::SwizzleA(
			this->target,
			mode
		);
		return *this;
	}
#endif // GL_VERSION_3_3 GL_ARB_texture_swizzle

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle

	/** Wrapper for Texture::SwizzleRGBA()
	 *  @see Texture::SwizzleRGBA()
	 */
	TextureSwizzleTuple SwizzleRGBA(void) const
	{
		return ExplicitOps::SwizzleRGBA(
			this->target
		);
	}
#endif // GL_VERSION_3_3 GL_ARB_texture_swizzle

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle

	/** Wrapper for Texture::SwizzleRGBA()
	 *  @see Texture::SwizzleRGBA()
	 */
	const BoundObjOps& SwizzleRGBA(
		TextureSwizzle mode
	) const
	{
		ExplicitOps::SwizzleRGBA(
			this->target,
			mode
		);
		return *this;
	}
#endif // GL_VERSION_3_3 GL_ARB_texture_swizzle

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle

	/** Wrapper for Texture::SwizzleRGBA()
	 *  @see Texture::SwizzleRGBA()
	 */
	const BoundObjOps& SwizzleRGBA(
		TextureSwizzle mode_r,
		TextureSwizzle mode_g,
		TextureSwizzle mode_b,
		TextureSwizzle mode_a
	) const
	{
		ExplicitOps::SwizzleRGBA(
			this->target,
			mode_r,
			mode_g,
			mode_b,
			mode_a
		);
		return *this;
	}
#endif // GL_VERSION_3_3 GL_ARB_texture_swizzle

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle

	/** Wrapper for Texture::SwizzleRGBA()
	 *  @see Texture::SwizzleRGBA()
	 */
	const BoundObjOps& SwizzleRGBA(
		const TextureSwizzleTuple & modes
	) const
	{
		ExplicitOps::SwizzleRGBA(
			this->target,
			modes
		);
		return *this;
	}
#endif // GL_VERSION_3_3 GL_ARB_texture_swizzle


	/** Wrapper for Texture::Wrap()
	 *  @see Texture::Wrap()
	 */
	TextureWrap Wrap(
		TextureWrapCoord coord
	) const
	{
		return ExplicitOps::Wrap(
			this->target,
			coord
		);
	}


	/** Wrapper for Texture::Wrap()
	 *  @see Texture::Wrap()
	 */
	const BoundObjOps& Wrap(
		TextureWrapCoord coord,
		TextureWrap mode
	) const
	{
		ExplicitOps::Wrap(
			this->target,
			coord,
			mode
		);
		return *this;
	}


	/** Wrapper for Texture::WrapS()
	 *  @see Texture::WrapS()
	 */
	TextureWrap WrapS(void) const
	{
		return ExplicitOps::WrapS(
			this->target
		);
	}


	/** Wrapper for Texture::WrapS()
	 *  @see Texture::WrapS()
	 */
	const BoundObjOps& WrapS(
		TextureWrap mode
	) const
	{
		ExplicitOps::WrapS(
			this->target,
			mode
		);
		return *this;
	}


	/** Wrapper for Texture::WrapT()
	 *  @see Texture::WrapT()
	 */
	TextureWrap WrapT(void) const
	{
		return ExplicitOps::WrapT(
			this->target
		);
	}


	/** Wrapper for Texture::WrapT()
	 *  @see Texture::WrapT()
	 */
	const BoundObjOps& WrapT(
		TextureWrap mode
	) const
	{
		ExplicitOps::WrapT(
			this->target,
			mode
		);
		return *this;
	}


	/** Wrapper for Texture::WrapR()
	 *  @see Texture::WrapR()
	 */
	TextureWrap WrapR(void) const
	{
		return ExplicitOps::WrapR(
			this->target
		);
	}


	/** Wrapper for Texture::WrapR()
	 *  @see Texture::WrapR()
	 */
	const BoundObjOps& WrapR(
		TextureWrap mode
	) const
	{
		ExplicitOps::WrapR(
			this->target,
			mode
		);
		return *this;
	}


	/** Wrapper for Texture::Wrap()
	 *  @see Texture::Wrap()
	 */
	const BoundObjOps& Wrap(
		TextureWrap mode
	) const
	{
		ExplicitOps::Wrap(
			this->target,
			mode
		);
		return *this;
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3

	/** Wrapper for Texture::DepthStencilMode()
	 *  @see Texture::DepthStencilMode()
	 */
	PixelDataFormat DepthStencilMode(void) const
	{
		return ExplicitOps::DepthStencilMode(
			this->target
		);
	}
#endif // GL_VERSION_4_3

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3

	/** Wrapper for Texture::DepthStencilMode()
	 *  @see Texture::DepthStencilMode()
	 */
	const BoundObjOps& DepthStencilMode(
		PixelDataFormat mode
	) const
	{
		ExplicitOps::DepthStencilMode(
			this->target,
			mode
		);
		return *this;
	}
#endif // GL_VERSION_4_3

#if OGLPLUS_DOCUMENTATION_ONLY || GL_ARB_seamless_cubemap_per_texture

	/** Wrapper for Texture::Seamless()
	 *  @see Texture::Seamless()
	 */
	Boolean Seamless(void) const
	{
		return ExplicitOps::Seamless(
			this->target
		);
	}
#endif // GL_ARB_seamless_cubemap_per_texture

#if OGLPLUS_DOCUMENTATION_ONLY || GL_ARB_seamless_cubemap_per_texture

	/** Wrapper for Texture::Seamless()
	 *  @see Texture::Seamless()
	 */
	const BoundObjOps& Seamless(
		Boolean enable
	) const
	{
		ExplicitOps::Seamless(
			this->target,
			enable
		);
		return *this;
	}
#endif // GL_ARB_seamless_cubemap_per_texture


	/** Wrapper for Texture::GenerateMipmap()
	 *  @see Texture::GenerateMipmap()
	 */
	const BoundObjOps& GenerateMipmap(void) const
	{
		ExplicitOps::GenerateMipmap(
			this->target
		);
		return *this;
	}


}; // class BoundObjOps

} // namespace oglplus

#endif // include guard

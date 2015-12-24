/**
 *  @file oglplus/dsa/ext/texture.ipp
 *  @brief Implementation of DSA Texture functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/images/image_spec.hpp>
#include <oglplus/images/image.hpp>
#include <oglplus/lib/incl_end.ipp>

namespace oglplus {

#if GL_EXT_direct_state_access
OGLPLUS_LIB_FUNC
GLint ObjZeroOps<tag::DirectStateEXT, tag::Texture>::
GetIntParam(GLenum query) const
{
	GLint result = 0;
	OGLPLUS_GLFUNC(GetTextureParameterivEXT)(
		_obj_name(),
		GLenum(target),
		query,
		&result
	);
	OGLPLUS_CHECK(
		GetTextureParameterivEXT,
		ObjectError,
		Object(*this).
		EnumParam(query)
	);
	return result;
}

OGLPLUS_LIB_FUNC
GLfloat ObjZeroOps<tag::DirectStateEXT, tag::Texture>::
GetFloatParam(GLenum query) const
{
	GLfloat result = 0;
	OGLPLUS_GLFUNC(GetTextureParameterfvEXT)(
		_obj_name(),
		GLenum(target),
		query,
		&result
	);
	OGLPLUS_CHECK(
		GetTextureParameterfvEXT,
		ObjectError,
		Object(*this).
		EnumParam(query)
	);
	return result;
}

OGLPLUS_LIB_FUNC
GLint ObjZeroOps<tag::DirectStateEXT, tag::Texture>::
GetIntParam(GLint level, GLenum query) const
{
	GLint result = 0;
	OGLPLUS_GLFUNC(GetTextureLevelParameterivEXT)(
		_obj_name(),
		GLenum(target),
		level,
		query,
		&result
	);
	OGLPLUS_CHECK(
		GetTextureLevelParameterivEXT,
		ObjectError,
		Object(*this).
		EnumParam(query).
		Index(level)
	);
	return result;
}

OGLPLUS_LIB_FUNC
GLfloat ObjZeroOps<tag::DirectStateEXT, tag::Texture>::
GetFloatParam(GLint level, GLenum query) const
{
	GLfloat result = 0;
	OGLPLUS_GLFUNC(GetTextureLevelParameterfvEXT)(
		_obj_name(),
		GLenum(target),
		level,
		query,
		&result
	);
	OGLPLUS_CHECK(
		GetTextureLevelParameterfvEXT,
		ObjectError,
		Object(*this).
		EnumParam(query).
		Index(level)
	);
	return result;
}

OGLPLUS_LIB_FUNC
void ObjZeroOps<tag::DirectStateEXT, tag::Texture>::
GetImage(
	GLint level,
	PixelDataFormat format,
	const OutputData& dest
) const
{
	OGLPLUS_GLFUNC(GetTextureImageEXT)(
		_obj_name(),
		GLenum(target),
		level,
		GLenum(format),
		GLenum(dest.Type()),
		dest.Addr()
	);
	OGLPLUS_CHECK(
		GetTextureImageEXT,
		ObjectError,
		Object(*this).
		BindTarget(target).
		EnumParam(format).
		Index(level)
	);
}

OGLPLUS_LIB_FUNC
void ObjZeroOps<tag::DirectStateEXT, tag::Texture>::
GetCompressedImage(
	GLint level,
	const OutputData& dest
) const
{
	OGLPLUS_GLFUNC(GetCompressedTextureImageEXT)(
		_obj_name(),
		GLenum(target),
		level,
		dest.Addr()
	);
	OGLPLUS_CHECK(
		GetCompressedTextureImageEXT,
		ObjectError,
		Object(*this).
		BindTarget(target).
		Index(level)
	);
}

OGLPLUS_LIB_FUNC
void ObjZeroOps<tag::DirectStateEXT, tag::Texture>::
GetCompressedImage(
	GLint level,
	std::vector<GLubyte>& dest
) const
{
	dest.resize(std::size_t(CompressedImageSize(level)));
	GetCompressedImage(
		level,
		OutputData(dest)
	);
}

OGLPLUS_LIB_FUNC
ObjZeroOps<tag::DirectStateEXT, tag::Texture>&
ObjZeroOps<tag::DirectStateEXT, tag::Texture>::
Image3D(
	const images::Image& image,
	GLint level,
	GLint border
)
{
	OGLPLUS_GLFUNC(TextureImage3DEXT)(
		_obj_name(),
		GLenum(target),
		level,
		GLint(image.InternalFormat()),
		image.Width(),
		image.Height(),
		image.Depth(),
		border,
		GLenum(image.Format()),
		GLenum(image.Type()),
		image.RawData()
	);
	OGLPLUS_CHECK(
		TextureImage3DEXT,
		ObjectError,
		Object(*this).
		BindTarget(target).
		Index(level)
	);
	return *this;
}

OGLPLUS_LIB_FUNC
ObjZeroOps<tag::DirectStateEXT, tag::Texture>&
ObjZeroOps<tag::DirectStateEXT, tag::Texture>::
SubImage3D(
	const images::Image& image,
	GLint xoffs,
	GLint yoffs,
	GLint zoffs,
	GLint level
)
{
	OGLPLUS_GLFUNC(TextureSubImage3DEXT)(
		_obj_name(),
		GLenum(target),
		level,
		xoffs,
		yoffs,
		zoffs,
		image.Width(),
		image.Height(),
		image.Depth(),
		GLenum(image.Format()),
		GLenum(image.Type()),
		image.RawData()
	);
	OGLPLUS_CHECK(
		TextureSubImage3DEXT,
		ObjectError,
		Object(*this).
		BindTarget(target).
		Index(level)
	);
	return *this;
}

OGLPLUS_LIB_FUNC
ObjZeroOps<tag::DirectStateEXT, tag::Texture>&
ObjZeroOps<tag::DirectStateEXT, tag::Texture>::
Image2D(
	TextureTarget tex_target,
	const images::Image& image,
	GLint level,
	GLint border
)
{
	OGLPLUS_GLFUNC(TextureImage2DEXT)(
		_obj_name(),
		GLenum(tex_target),
		level,
		GLint(image.InternalFormat()),
		image.Width(),
		image.Height(),
		border,
		GLenum(image.Format()),
		GLenum(image.Type()),
		image.RawData()
	);
	OGLPLUS_CHECK(
		TextureImage2DEXT,
		ObjectError,
		Object(*this).
		BindTarget(target).
		Index(level)
	);
	return *this;
}

OGLPLUS_LIB_FUNC
ObjZeroOps<tag::DirectStateEXT, tag::Texture>&
ObjZeroOps<tag::DirectStateEXT, tag::Texture>::
SubImage2D(
	const images::Image& image,
	GLint xoffs,
	GLint yoffs,
	GLint level
)
{
	OGLPLUS_GLFUNC(TextureSubImage2DEXT)(
		_obj_name(),
		GLenum(target),
		level,
		xoffs,
		yoffs,
		image.Width(),
		image.Height(),
		GLenum(image.Format()),
		GLenum(image.Type()),
		image.RawData()
	);
	OGLPLUS_CHECK(
		TextureSubImage2DEXT,
		ObjectError,
		Object(*this).
		BindTarget(target).
		Index(level)
	);
	return *this;
}

OGLPLUS_LIB_FUNC
ObjZeroOps<tag::DirectStateEXT, tag::Texture>&
ObjZeroOps<tag::DirectStateEXT, tag::Texture>::
Image1D(
	const images::Image& image,
	GLint level,
	GLint border
)
{
	OGLPLUS_GLFUNC(TextureImage1DEXT)(
		_obj_name(),
		GLenum(target),
		level,
		GLint(image.InternalFormat()),
		image.Width(),
		border,
		GLenum(image.Format()),
		GLenum(image.Type()),
		image.RawData()
	);
	OGLPLUS_CHECK(
		TextureImage1DEXT,
		ObjectError,
		Object(*this).
		BindTarget(target).
		Index(level)
	);
	return *this;
}

OGLPLUS_LIB_FUNC
ObjZeroOps<tag::DirectStateEXT, tag::Texture>&
ObjZeroOps<tag::DirectStateEXT, tag::Texture>::
SubImage1D(
	const images::Image& image,
	GLint xoffs,
	GLint level
)
{
	OGLPLUS_GLFUNC(TextureSubImage1DEXT)(
		_obj_name(),
		GLenum(target),
		level,
		xoffs,
		image.Width(),
		GLenum(image.Format()),
		GLenum(image.Type()),
		image.RawData()
	);
	OGLPLUS_CHECK(
		TextureSubImage1DEXT,
		ObjectError,
		Object(*this).
		BindTarget(target).
		Index(level)
	);
	return *this;
}

OGLPLUS_LIB_FUNC
ObjZeroOps<tag::DirectStateEXT, tag::Texture>&
ObjZeroOps<tag::DirectStateEXT, tag::Texture>::
Image(
	Target tex_target,
	const images::Image& image,
	GLint level,
	GLint border
)
{
	switch(TextureTargetDimensions(tex_target))
	{
		case 3:
		{
			Image3D(image, level, border);
			break;
		}
		case 2:
		{
			Image2D(tex_target, image, level, border);
			break;
		}
		case 1:
		{
			Image1D(image, level, border);
			break;
		}
		default: OGLPLUS_ABORT("Invalid texture dimension");
	}
	return *this;
}

OGLPLUS_LIB_FUNC
ObjZeroOps<tag::DirectStateEXT, tag::Texture>&
ObjZeroOps<tag::DirectStateEXT, tag::Texture>::
Image(
	Target tex_target,
	const images::ImageSpec& image_spec,
	GLint level,
	GLint border
)
{
	switch(TextureTargetDimensions(tex_target))
	{
		case 3:
		{
			Image3D(
				level,
				image_spec.internal_format,
				image_spec.width,
				image_spec.height,
				image_spec.depth,
				border,
				image_spec.format,
				image_spec.data_type,
				image_spec.data_ptr
			);
			break;
		}
		case 2:
		{
			Image2D(
				target,
				level,
				image_spec.internal_format,
				image_spec.width,
				image_spec.height,
				border,
				image_spec.format,
				image_spec.data_type,
				image_spec.data_ptr
			);
			break;
		}
		case 1:
		{
			Image1D(
				level,
				image_spec.internal_format,
				image_spec.width,
				border,
				image_spec.format,
				image_spec.data_type,
				image_spec.data_ptr
			);
			break;
		}
		default: OGLPLUS_ABORT("Invalid texture dimension");
	}
	return *this;
}

#endif // GL_EXT_direct_state_access

} // namespace oglplus


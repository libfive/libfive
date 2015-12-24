/**
 *  @file oglplus/dsa/texture.ipp
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

#if GL_VERSION_4_5 || GL_ARB_direct_state_access
OGLPLUS_LIB_FUNC
GLint ObjectOps<tag::DirectState, tag::Texture>::
GetIntParam(GLenum query) const
{
	GLint result = 0;
	OGLPLUS_GLFUNC(GetTextureParameteriv)(
		_obj_name(),
		GLenum(target),
		query,
		&result
	);
	OGLPLUS_CHECK(
		GetTextureParameteriv,
		ObjectError,
		Object(*this).
		EnumParam(query)
	);
	return result;
}

OGLPLUS_LIB_FUNC
GLfloat ObjectOps<tag::DirectState, tag::Texture>::
GetFloatParam(GLenum query) const
{
	GLfloat result = 0;
	OGLPLUS_GLFUNC(GetTextureParameterfv)(
		_obj_name(),
		GLenum(target),
		query,
		&result
	);
	OGLPLUS_CHECK(
		GetTextureParameterfv,
		ObjectError,
		Object(*this).
		EnumParam(query)
	);
	return result;
}

OGLPLUS_LIB_FUNC
GLint ObjectOps<tag::DirectState, tag::Texture>::
GetIntParam(GLint level, GLenum query) const
{
	GLint result = 0;
	OGLPLUS_GLFUNC(GetTextureLevelParameteriv)(
		_obj_name(),
		GLenum(target),
		level,
		query,
		&result
	);
	OGLPLUS_CHECK(
		GetTextureLevelParameteriv,
		ObjectError,
		Object(*this).
		EnumParam(query).
		Index(level)
	);
	return result;
}

OGLPLUS_LIB_FUNC
GLfloat ObjectOps<tag::DirectState, tag::Texture>::
GetFloatParam(GLint level, GLenum query) const
{
	GLfloat result = 0;
	OGLPLUS_GLFUNC(GetTextureLevelParameterfv)(
		_obj_name(),
		GLenum(target),
		level,
		query,
		&result
	);
	OGLPLUS_CHECK(
		GetTextureLevelParameterfv,
		ObjectError,
		Object(*this).
		EnumParam(query).
		Index(level)
	);
	return result;
}

OGLPLUS_LIB_FUNC
void ObjectOps<tag::DirectState, tag::Texture>::
GetImage(
	GLint level,
	PixelDataFormat format,
	const OutputData& dest
) const
{
	OGLPLUS_FAKE_USE(size);
	OGLPLUS_GLFUNC(GetTextureImage)(
		_obj_name(),
		GLenum(target),
		level,
		GLenum(format),
		GLenum(dest.Type()),
		dest.Addr()
	);
	OGLPLUS_CHECK(
		GetTextureImage,
		ObjectError,
		Object(*this).
		BindTarget(target).
		EnumParam(format).
		Index(level)
	);
}

OGLPLUS_LIB_FUNC
void ObjectOps<tag::DirectState, tag::Texture>::
GetCompressedImage(
	GLint level,
	const OutputData& dest
) const
{
	OGLPLUS_FAKE_USE(size);
	OGLPLUS_GLFUNC(GetCompressedTextureImage)(
		_obj_name(),
		GLenum(target),
		level,
		dest.Addr()
	);
	OGLPLUS_CHECK(
		GetCompressedTextureImage,
		ObjectError,
		Object(*this).
		BindTarget(target).
		Index(level)
	);
}

OGLPLUS_LIB_FUNC
void ObjectOps<tag::DirectState, tag::Texture>::
GetCompressedImage(
	GLint level,
	std::vector<GLubyte>& dest
) const
{
	dest.resize(CompressedImageSize(level));
	GetCompressedImage(
		level,
		OutputData(dest)
	);
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Texture>&
ObjectOps<tag::DirectState, tag::Texture>::
SubImage3D(
	const images::Image& image,
	GLint xoffs,
	GLint yoffs,
	GLint zoffs,
	GLint level
)
{
	OGLPLUS_GLFUNC(TextureSubImage3D)(
		_obj_name(),
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
		TextureSubImage3D,
		ObjectError,
		Object(*this).
		Index(level)
	);
	return *this;
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Texture>&
ObjectOps<tag::DirectState, tag::Texture>::
SubImage2D(
	const images::Image& image,
	GLint xoffs,
	GLint yoffs,
	GLint level
)
{
	OGLPLUS_GLFUNC(TextureSubImage2D)(
		_obj_name(),
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
		TextureSubImage2D,
		ObjectError,
		Object(*this).
		Index(level)
	);
	return *this;
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Texture>&
ObjectOps<tag::DirectState, tag::Texture>::
SubImage1D(
	const images::Image& image,
	GLint xoffs,
	GLint level
)
{
	OGLPLUS_GLFUNC(TextureSubImage1D)(
		_obj_name(),
		level,
		xoffs,
		image.Width(),
		GLenum(image.Format()),
		GLenum(image.Type()),
		image.RawData()
	);
	OGLPLUS_CHECK(
		TextureSubImage1D,
		ObjectError,
		Object(*this).
		Index(level)
	);
	return *this;
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Texture>&
ObjectOps<tag::DirectState, tag::Texture>::
Storage1D(
	GLsizei levels,
	const images::ImageSpec& image_spec
)
{
	return Storage1D(
		levels,
		image_spec.internal_format,
		image_spec.width
	);
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Texture>&
ObjectOps<tag::DirectState, tag::Texture>::
Storage2D(
	GLsizei levels,
	const images::ImageSpec& image_spec
)
{
	return Storage2D(
		levels,
		image_spec.internal_format,
		image_spec.width,
		image_spec.height
	);
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Texture>&
ObjectOps<tag::DirectState, tag::Texture>::
Storage3D(
	GLsizei levels,
	const images::ImageSpec& image_spec
)
{
	return Storage3D(
		levels,
		image_spec.internal_format,
		image_spec.width,
		image_spec.height,
		image_spec.depth
	);
}

OGLPLUS_LIB_FUNC
ObjectOps<tag::DirectState, tag::Texture>&
ObjectOps<tag::DirectState, tag::Texture>::
Storage(
	GLsizei levels,
	const images::ImageSpec& image_spec
)
{
	if(image_spec.depth > 1)
	{
		return Storage3D(levels, image_spec);
	}
	if(image_spec.height > 1)
	{
		return Storage2D(levels, image_spec);
	}
	if(image_spec.width > 1)
	{
		return Storage1D(levels, image_spec);
	}
	return *this;
}

#endif // GL_ARB_direct_state_access

} // namespace oglplus


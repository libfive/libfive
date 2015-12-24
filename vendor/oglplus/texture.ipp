/**
 *  @file oglplus/texture.ipp
 *  @brief Implementation of Texture functions
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

OGLPLUS_LIB_FUNC
GLuint TextureTargetDimensions(TextureTarget target)
{
	switch(GLenum(target))
	{
#ifdef GL_TEXTURE_1D
		case GL_TEXTURE_1D:
#endif
#ifdef GL_TEXTURE_1D_ARRAY
		case GL_TEXTURE_1D_ARRAY:
#endif
#ifdef GL_TEXTURE_BUFFER
		case GL_TEXTURE_BUFFER:
#endif
			return 1;

#ifdef GL_TEXTURE_2D
		case GL_TEXTURE_2D:
#endif
#ifdef GL_TEXTURE_2D_MULTISAMPLE
		case GL_TEXTURE_2D_MULTISAMPLE:
#endif
#ifdef GL_TEXTURE_RECTANGLE
		case GL_TEXTURE_RECTANGLE:
#endif
#ifdef GL_TEXTURE_CUBE_MAP
		case GL_TEXTURE_CUBE_MAP:
#endif
#ifdef GL_TEXTURE_CUBE_MAP_POSITIVE_X
		case GL_TEXTURE_CUBE_MAP_POSITIVE_X:
#endif
#ifdef GL_TEXTURE_CUBE_MAP_NEGATIVE_X
		case GL_TEXTURE_CUBE_MAP_NEGATIVE_X:
#endif
#ifdef GL_TEXTURE_CUBE_MAP_POSITIVE_Y
		case GL_TEXTURE_CUBE_MAP_POSITIVE_Y:
#endif
#ifdef GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
		case GL_TEXTURE_CUBE_MAP_NEGATIVE_Y:
#endif
#ifdef GL_TEXTURE_CUBE_MAP_POSITIVE_Z
		case GL_TEXTURE_CUBE_MAP_POSITIVE_Z:
#endif
#ifdef GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
		case GL_TEXTURE_CUBE_MAP_NEGATIVE_Z:
#endif
			return 2;

#ifdef GL_TEXTURE_2D_ARRAY
		case GL_TEXTURE_2D_ARRAY:
#endif
#ifdef GL_TEXTURE_2D_MULTISAMPLE_ARRAY
		case GL_TEXTURE_2D_MULTISAMPLE_ARRAY:
#endif
#ifdef GL_TEXTURE_CUBE_MAP_ARRAY
		case GL_TEXTURE_CUBE_MAP_ARRAY:
#endif
#ifdef GL_TEXTURE_3D
		case GL_TEXTURE_3D:
#endif
			return 3;
		default:;
	}
	return 0;
}

OGLPLUS_LIB_FUNC
GLenum ObjBindingOps<tag::Texture>::
_binding_query(Target target)
{
	switch(GLenum(target))
	{
#include <oglplus/enums/texture_target_bq.ipp>
		default:;
	}
	return 0;
}

OGLPLUS_LIB_FUNC
GLuint ObjBindingOps<tag::Texture>::
_binding(Target target)
{
	GLint name = 0;
	OGLPLUS_GLFUNC(GetIntegerv)(_binding_query(target), &name);
	OGLPLUS_VERIFY(
		GetIntegerv,
		Error,
		EnumParam(_binding_query(target))
	);

	assert(!(name < 0));
	return GLuint(name);
}

OGLPLUS_LIB_FUNC
GLint ObjZeroOps<tag::ExplicitSel, tag::Texture>::
GetIntParam(Target target, GLenum query)
{
	GLint result = 0;
	OGLPLUS_GLFUNC(GetTexParameteriv)(
		GLenum(target),
		query,
		&result
	);
	OGLPLUS_CHECK(
		GetTexParameteriv,
		ObjectError,
		ObjectBinding(target).
		EnumParam(query)
	);
	return result;
}

OGLPLUS_LIB_FUNC
GLfloat ObjZeroOps<tag::ExplicitSel, tag::Texture>::
GetFloatParam(Target target, GLenum query)
{
	GLfloat result = 0;
	OGLPLUS_GLFUNC(GetTexParameterfv)(
		GLenum(target),
		query,
		&result
	);
	OGLPLUS_CHECK(
		GetTexParameterfv,
		ObjectError,
		ObjectBinding(target).
		EnumParam(query)
	);
	return result;
}

#if GL_VERSION_3_0
OGLPLUS_LIB_FUNC
GLint ObjZeroOps<tag::ExplicitSel, tag::Texture>::
GetIntParam(Target target, GLint level, GLenum query)
{
	GLint result = 0;
	OGLPLUS_GLFUNC(GetTexLevelParameteriv)(
		GLenum(target),
		level,
		query,
		&result
	);
	OGLPLUS_CHECK(
		GetTexLevelParameteriv,
		ObjectError,
		ObjectBinding(target).
		EnumParam(query).
		Index(level)
	);
	return result;
}

OGLPLUS_LIB_FUNC
GLfloat ObjZeroOps<tag::ExplicitSel, tag::Texture>::
GetFloatParam(Target target, GLint level, GLenum query)
{
	GLfloat result = 0;
	OGLPLUS_GLFUNC(GetTexLevelParameterfv)(
		GLenum(target),
		level,
		query,
		&result
	);
	OGLPLUS_CHECK(
		GetTexLevelParameterfv,
		ObjectError,
		ObjectBinding(target).
		EnumParam(query).
		Index(level)
	);
	return result;
}

OGLPLUS_LIB_FUNC
void ObjZeroOps<tag::ExplicitSel, tag::Texture>::
GetImage(
	Target target,
	GLint level,
	PixelDataFormat format,
	const OutputData& dest
)
{
#if GL_ARB_robustness
	OGLPLUS_GLFUNC(GetnTexImageARB)(
		GLenum(target),
		level,
		GLenum(format),
		GLenum(dest.Type()),
		GLsizei(dest.Size()),
		dest.Addr()
	);
	OGLPLUS_CHECK(
		GetnTexImageARB,
		ObjectError,
		ObjectBinding(target).
		EnumParam(format).
		Index(level)
	);
#else
	OGLPLUS_GLFUNC(GetTexImage)(
		GLenum(target),
		level,
		GLenum(format),
		GLenum(dest.Type()),
		dest.Addr()
	);
	OGLPLUS_CHECK(
		GetTexImage,
		ObjectError,
		ObjectBinding(target).
		EnumParam(format).
		Index(level)
	);
#endif
}

OGLPLUS_LIB_FUNC
void ObjZeroOps<tag::ExplicitSel, tag::Texture>::
GetCompressedImage(
	Target target,
	GLint level,
	const OutputData& dest
)
{
#if GL_ARB_robustness
	OGLPLUS_GLFUNC(GetnCompressedTexImageARB)(
		GLenum(target),
		level,
		GLsizei(dest.Size()),
		dest.Addr()
	);
	OGLPLUS_CHECK(
		GetnCompressedTexImageARB,
		ObjectError,
		ObjectBinding(target).
		Index(level)
	);
#else
	OGLPLUS_GLFUNC(GetCompressedTexImage)(
		GLenum(target),
		level,
		dest.Addr()
	);
	OGLPLUS_CHECK(
		GetCompressedTexImage,
		ObjectError,
		ObjectBinding(target).
		Index(level)
	);
#endif
}

OGLPLUS_LIB_FUNC
void ObjZeroOps<tag::ExplicitSel, tag::Texture>::
GetCompressedImage(
	Target target,
	GLint level,
	std::vector<GLubyte>& dest
)
{
	dest.resize(std::size_t(CompressedImageSize(target, level)));
	GetCompressedImage(
		target,
		level,
		OutputData(dest)
	);
}

#endif

OGLPLUS_LIB_FUNC
void ObjZeroOps<tag::ExplicitSel, tag::Texture>::
Image3D(
	Target target,
	const images::Image& image,
	GLint level,
	GLint border
)
{
	OGLPLUS_GLFUNC(TexImage3D)(
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
		TexImage3D,
		ObjectError,
		ObjectBinding(target).
		Index(level)
	);
}

OGLPLUS_LIB_FUNC
void ObjZeroOps<tag::ExplicitSel, tag::Texture>::
SubImage3D(
	Target target,
	const images::Image& image,
	GLint xoffs,
	GLint yoffs,
	GLint zoffs,
	GLint level
)
{
	OGLPLUS_GLFUNC(TexSubImage3D)(
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
		TexSubImage3D,
		ObjectError,
		ObjectBinding(target).
		Index(level)
	);
}

OGLPLUS_LIB_FUNC
void ObjZeroOps<tag::ExplicitSel, tag::Texture>::
Image2D(
	Target target,
	const images::Image& image,
	GLint level,
	GLint border
)
{
	OGLPLUS_GLFUNC(TexImage2D)(
		GLenum(target),
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
		TexImage2D,
		ObjectError,
		ObjectBinding(target).
		Index(level)
	);
}

OGLPLUS_LIB_FUNC
void ObjZeroOps<tag::ExplicitSel, tag::Texture>::
ImageCM(
	GLuint face,
	const images::Image& image,
	GLint level,
	GLint border
)
{
	assert(face <= 5);
	Target target = Target(GL_TEXTURE_CUBE_MAP_POSITIVE_X+face);
	OGLPLUS_GLFUNC(TexImage2D)(
		GLenum(target),
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
		TexImage2D,
		ObjectError,
		ObjectBinding(target).
		Index(level)
	);
}

OGLPLUS_LIB_FUNC
void ObjZeroOps<tag::ExplicitSel, tag::Texture>::
SubImage2D(
	Target target,
	const images::Image& image,
	GLint xoffs,
	GLint yoffs,
	GLint level
)
{
	OGLPLUS_GLFUNC(TexSubImage2D)(
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
		TexSubImage2D,
		ObjectError,
		ObjectBinding(target).
		Index(level)
	);
}

#if GL_VERSION_3_0

OGLPLUS_LIB_FUNC
void ObjZeroOps<tag::ExplicitSel, tag::Texture>::
Image1D(
	Target target,
	const images::Image& image,
	GLint level,
	GLint border
)
{
	OGLPLUS_GLFUNC(TexImage1D)(
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
		TexImage1D,
		ObjectError,
		ObjectBinding(target).
		Index(level)
	);
}

OGLPLUS_LIB_FUNC
void ObjZeroOps<tag::ExplicitSel, tag::Texture>::
SubImage1D(
	Target target,
	const images::Image& image,
	GLint xoffs,
	GLint level
)
{
	OGLPLUS_GLFUNC(TexSubImage1D)(
		GLenum(target),
		level,
		xoffs,
		image.Width(),
		GLenum(image.Format()),
		GLenum(image.Type()),
		image.RawData()
	);
	OGLPLUS_CHECK(
		TexSubImage1D,
		ObjectError,
		ObjectBinding(target).
		Index(level)
	);
}

#endif // GL_VERSION_3_0

OGLPLUS_LIB_FUNC
void ObjZeroOps<tag::ExplicitSel, tag::Texture>::
Image(
	Target target,
	const images::Image& image,
	GLint level,
	GLint border
)
{
	switch(TextureTargetDimensions(target))
	{
		case 3:
		{
			Image3D(target, image, level, border);
			break;
		}
		case 2:
		{
			Image2D(target, image, level, border);
			break;
		}
#if GL_VERSION_3_0
		case 1:
		{
			Image1D(target, image, level, border);
			break;
		}
#endif
		default: OGLPLUS_ABORT("Invalid texture dimension");
	}
}

OGLPLUS_LIB_FUNC
void ObjZeroOps<tag::ExplicitSel, tag::Texture>::
Image(
	Target target,
	const images::ImageSpec& image_spec,
	GLint level,
	GLint border
)
{
	switch(TextureTargetDimensions(target))
	{
		case 3:
		{
			Image3D(
				target,
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
#if GL_VERSION_3_0
		case 1:
		{
			Image1D(
				target,
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
#endif
		default: OGLPLUS_ABORT("Invalid texture dimension");
	}
}

} // namespace oglplus


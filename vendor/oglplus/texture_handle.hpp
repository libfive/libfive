/**
 *  @file oglplus/texture_handle.hpp
 *  @brief Bindless Texture handle wrapper.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXTURE_HANDLE_1309262134_HPP
#define OGLPLUS_TEXTURE_HANDLE_1309262134_HPP

#include <oglplus/fwd.hpp>
#include <oglplus/glfunc.hpp>
#include <oglplus/boolean.hpp>
#include <oglplus/object/tags.hpp>
#include <oglplus/object/name.hpp>
#include <oglplus/prog_var/type_ops.hpp>
#include <oglplus/pixel_data.hpp>
#include <oglplus/access_specifier.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_ARB_bindless_texture

/// A handle for a bindless texture
class TextureHandle
{
private:
	friend GLuint64 GetGLHandle(TextureHandle);
	GLuint64 _handle;
public:
	/// Construction from a texture
	TextureHandle(TextureName texture)
	 : _handle(OGLPLUS_GLFUNC(GetTextureHandleARB)(GetGLName(texture)))
	{
		OGLPLUS_CHECK_SIMPLE(GetTextureHandleARB);
	}

	/// Construction from a texture and a sampler
	TextureHandle(TextureName texture, SamplerName sampler)
	 : _handle(OGLPLUS_GLFUNC(GetTextureSamplerHandleARB)(
		GetGLName(texture),
		GetGLName(sampler)
	))
	{
		OGLPLUS_CHECK_SIMPLE(GetTextureSamplerHandleARB);
	}

	/// Make the texture resident
	void MakeResident(void)
	{
		OGLPLUS_GLFUNC(MakeTextureHandleResidentARB)(_handle);
		OGLPLUS_CHECK_SIMPLE(MakeTextureHandleResidentARB);
	}

	/// Make the texture non-resident
	void MakeNonResident(void)
	{
		OGLPLUS_GLFUNC(MakeTextureHandleNonResidentARB)(_handle);
		OGLPLUS_CHECK_SIMPLE(MakeTextureHandleNonResidentARB);
	}

	/// Make the texture non-resident
	Boolean IsResident(void) const
	{
		Boolean result(
			OGLPLUS_GLFUNC(IsTextureHandleResidentARB)(_handle),
			std::nothrow
		);
		OGLPLUS_VERIFY_SIMPLE(IsTextureHandleResidentARB);
		return result;
	}
};

/// Returns the GL handle value from TextureHandle
inline
GLuint64 GetGLHandle(TextureHandle th)
OGLPLUS_NOEXCEPT(true)
{
	return th._handle;
}

template <>
struct AdjustProgVar<TextureHandle>
{
	typedef GLuint64 BaseType;
	typedef TextureHandle ValueType;

	inline static BaseType Adjust(ValueType value)
	{
		return GetGLHandle(value);
	}
};

/// A handle for a bindless texture image
class ImageHandle
{
private:
	friend GLuint64 GetGLHandle(ImageHandle);
	GLuint64 _handle;
public:
	/// Construction from a texture and additional parameters
	ImageHandle(
		TextureName texture,
		GLint level,
		Boolean layered,
		GLint layer,
		ImageUnitFormat format
	): _handle(OGLPLUS_GLFUNC(GetImageHandleARB)(
		GetGLName(texture),
		level,
		layered._v,
		layer,
		GLenum(format)
	))
	{
		OGLPLUS_CHECK_SIMPLE(GetImageHandleARB);
	}

	/// Make the image resident
	void MakeResident(AccessSpecifier access)
	{
		OGLPLUS_GLFUNC(MakeImageHandleResidentARB)(
			_handle,
			GLenum(access)
		);
		OGLPLUS_CHECK_SIMPLE(MakeImageHandleResidentARB);
	}

	/// Make the image non-resident
	void MakeNonResident(void)
	{
		OGLPLUS_GLFUNC(MakeImageHandleNonResidentARB)(_handle);
		OGLPLUS_CHECK_SIMPLE(MakeImageHandleNonResidentARB);
	}

	/// Make the image non-resident
	Boolean IsResident(void) const
	{
		Boolean result(
			OGLPLUS_GLFUNC(IsImageHandleResidentARB)(_handle),
			std::nothrow
		);
		OGLPLUS_VERIFY_SIMPLE(IsImageHandleResidentARB);
		return result;
	}
};

/// Returns the GL handle value from ImageHandle
inline
GLuint64 GetGLHandle(ImageHandle ih)
OGLPLUS_NOEXCEPT(true)
{
	return ih._handle;
}

template <>
struct AdjustProgVar<ImageHandle>
{
	typedef GLuint64 BaseType;
	typedef ImageHandle ValueType;

	inline static BaseType Adjust(ValueType value)
	{
		return GetGLHandle(value);
	}
};

#endif // GL_ARB_bindless_texture

} // namespace oglplus

#endif // include guard

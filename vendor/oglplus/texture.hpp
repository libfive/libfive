/**
 *  @file oglplus/texture.hpp
 *  @brief Texture object wrappers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXTURE_1107121519_HPP
#define OGLPLUS_TEXTURE_1107121519_HPP

#include <oglplus/fwd.hpp>
#include <oglplus/glfunc.hpp>
#include <oglplus/size_type.hpp>
#include <oglplus/boolean.hpp>
#include <oglplus/error/object.hpp>
#include <oglplus/math/vector.hpp>
#include <oglplus/object/sequence.hpp>
#include <oglplus/object/wrapper.hpp>
#include <oglplus/compare_function.hpp>
#include <oglplus/data_type.hpp>
#include <oglplus/pixel_data.hpp>
#include <oglplus/access_specifier.hpp>
#include <oglplus/texture_target.hpp>
#include <oglplus/texture_compare.hpp>
#include <oglplus/texture_filter.hpp>
#include <oglplus/texture_swizzle.hpp>
#include <oglplus/texture_wrap.hpp>
#include <oglplus/texture_unit.hpp>
#include <oglplus/one_of.hpp>
#include <oglplus/output_data.hpp>
#include <oglplus/images/fwd.hpp>
#include <oglplus/assert.hpp>

namespace oglplus {

/// Function returning the number of texture dimensions for a texture target
GLuint TextureTargetDimensions(TextureTarget target);

/// Class wrapping texture construction/destruction functions
/** @note Do not use this class directly, use Texture instead.
 *
 *  @glsymbols
 *  @glfunref{GenTextures}
 *  @glfunref{DeleteTextures}
 *  @glfunref{IsTexture}
 */
template <>
class ObjGenDelOps<tag::Texture>
{
protected:
	static void Gen(tag::Generate, GLsizei count, GLuint* names)
	{
		assert(names != nullptr);
		OGLPLUS_GLFUNC(GenTextures)(count, names);
		OGLPLUS_CHECK_SIMPLE(GenTextures);
	}
#if GL_VERSION_4_5 || GL_ARB_direct_state_access
	static void Gen(
		tag::Create,
		GLenum target,
		GLsizei count,
		GLuint* names
	)
	{
		assert(names != nullptr);
		OGLPLUS_GLFUNC(CreateTextures)(target, count, names);
		OGLPLUS_CHECK_SIMPLE(CreateTextures);
	}

	GLenum _type;

	void Gen(tag::Create create, GLsizei count, GLuint* names)
	{
		Gen(create, _type, count, names);
	}
#endif

	static void Delete(GLsizei count, GLuint* names)
	{
		assert(names != nullptr);
		OGLPLUS_GLFUNC(DeleteTextures)(count, names);
		OGLPLUS_VERIFY_SIMPLE(DeleteTextures);
	}

	static Boolean IsA(GLuint name)
	{
		Boolean result(
			OGLPLUS_GLFUNC(IsTexture)(name),
			std::nothrow
		);
		OGLPLUS_VERIFY_SIMPLE(IsTexture);
		return result;
	}
};

template <>
struct ObjectSubtype<tag::Texture>
{
	typedef TextureTarget Type;
};

/// Texture binding operations
template <>
class ObjBindingOps<tag::Texture>
{
private:
	static GLenum _binding_query(TextureTarget target);
protected:
	static GLuint _binding(TextureTarget target);
public:
	/// Texture bind targets
	typedef TextureTarget Target;

	/// Returns the current Texture bound to specified @p target
	/**
	 *  @glsymbols
	 *  @glfunref{GetIntegerv}
	 */
	static TextureName Binding(Target target)
	{
		return TextureName(_binding(target));
	}

	/// Binds the specified @p texture to the specified @p target
	/**
	 *  @glsymbols
	 *  @glfunref{BindTexture}
	 */
	static void Bind(
		Target target,
		TextureName texture
	)
	{
		OGLPLUS_GLFUNC(BindTexture)(
			GLenum(target),
			GetGLName(texture)
		);
		OGLPLUS_VERIFY(
			BindTexture,
			ObjectError,
			Object(texture).
			BindTarget(target)
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY ||GL_VERSION_4_2 ||GL_ARB_shader_image_load_store
	/// Binds a @p level of @p texture to an image @p unit
	/**
	 *  @glvoereq{4,2,ARB,shader_image_load_store}
	 *  @glsymbols
	 *  @glfunref{BindImageTexture}
	 */
	static void BindImage(
		ImageUnitSelector unit,
		TextureName texture,
		GLint level,
		Boolean layered,
		GLint layer,
		AccessSpecifier access,
		ImageUnitFormat format
	)
	{
		OGLPLUS_GLFUNC(BindImageTexture)(
			GLuint(unit),
			GetGLName(texture),
			level,
			layered._get(),
			layer,
			GLenum(access),
			GLenum(format)
		);
		OGLPLUS_CHECK(
			BindImageTexture,
			ObjectError,
			Object(texture).
			EnumParam(format).
			Index(level)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_4 || GL_ARB_multi_bind
	static void Bind(
		GLuint first,
		SizeType count,
		const GLuint* names
	)
	{
		OGLPLUS_GLFUNC(BindTextures)(
			first,
			count,
			names
		);
		OGLPLUS_VERIFY_SIMPLE(BindTextures);
	}

	static void BindImage(
		GLuint first,
		SizeType count,
		const GLuint* names
	)
	{
		OGLPLUS_GLFUNC(BindImageTextures)(
			first,
			count,
			names
		);
		OGLPLUS_VERIFY_SIMPLE(BindImageTextures);
	}

	/// Sequentially bind @p textures to texture units starting with @p first
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{BindTextures}
	 *  @glvoereq{4,4,ARB,multi_bind}
	 */
	static void Bind(
		GLuint first,
		const Sequence<TextureName>& textures
	)
	{
		Bind(
			first,
			SizeType(textures.size()),
			GetGLNames(textures)
		);
	}

	/// Sequentially bind @p textures to image units starting with @p first
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{BindImageTextures}
	 *  @glvoereq{4,4,ARB,multi_bind}
	 */
	static void BindImage(
		GLuint first,
		const Sequence<TextureName>& textures
	)
	{
		BindImage(
			first,
			SizeType(textures.size()),
			GetGLNames(textures)
		);
	}
#endif
};

/// Common texture operations
/** @note Do not use this class directly, use Texture
 *  or DefaultTexture instead.
 */
template <>
class ObjCommonOps<tag::Texture>
 : public TextureName
 , public ObjBindingOps<tag::Texture>
{
protected:
	ObjCommonOps(TextureName name)
	OGLPLUS_NOEXCEPT(true)
	 : TextureName(name)
	{ }
public:
#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	ObjCommonOps(ObjCommonOps&&) = default;
	ObjCommonOps(const ObjCommonOps&) = default;
	ObjCommonOps& operator = (ObjCommonOps&&) = default;
	ObjCommonOps& operator = (const ObjCommonOps&) = default;
#else
	typedef TextureName _base1;
	typedef ObjBindingOps<tag::Texture> _base2;

	ObjCommonOps(ObjCommonOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	 : _base1(static_cast<_base1&&>(temp))
	 , _base2(static_cast<_base2&&>(temp))
	{ }

	ObjCommonOps(const ObjCommonOps& that)
	OGLPLUS_NOEXCEPT(true)
	 : _base1(static_cast<const _base1&>(that))
	 , _base2(static_cast<const _base2&>(that))
	{ }

	ObjCommonOps& operator = (ObjCommonOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	{
		_base1::operator = (static_cast<_base1&&>(temp));
		_base2::operator = (static_cast<_base2&&>(temp));
		return *this;
	}

	ObjCommonOps& operator = (const ObjCommonOps& that)
	OGLPLUS_NOEXCEPT(true)
	{
		_base1::operator = (static_cast<const _base1&>(that));
		_base2::operator = (static_cast<const _base2&>(that));
		return *this;
	}
#endif
	/// Specify active texture unit for subsequent commands
	/**
	 *  @throws Error
	 *
	 *  @see Bind
	 *
	 *  @glsymbols
	 *  @glfunref{ActiveTexture}
	 */
	static void Active(TextureUnitSelector index)
	{
		OGLPLUS_GLFUNC(ActiveTexture)(
			GLenum(GL_TEXTURE0 + GLuint(index))
		);
		OGLPLUS_VERIFY(
			ActiveTexture,
			Error,
			Index(GLuint(index))
		);
	}

	/// Returns active texture unit
	/**
	 *  @throws Error
	 *
	 *  @glsymbols
	 *  @glfunref{Get}
	 *  @gldefref{ACTIVE_TEXTURE}
	 */
	static GLint Active(void)
	{
		GLint result;
		OGLPLUS_GLFUNC(GetIntegerv)(GL_ACTIVE_TEXTURE, &result);
		OGLPLUS_VERIFY(
			GetIntegerv,
			Error,
			EnumParam(GLenum(GL_ACTIVE_TEXTURE))
		);
		return result - GL_TEXTURE0;
	}

	/// Returns the target for the i-th cube map @p face (0-5)
	/** This functions returns one of the values for cube map faces
	 *  from the Target enumeration. The value of @p face must
	 *  be between 0 and 5 with the following meaning:
	 *  0 = Target::CubeMapPositiveX,
	 *  1 = Target::CubeMapNegativeX,
	 *  2 = Target::CubeMapPositiveY,
	 *  3 = Target::CubeMapNegativeY,
	 *  4 = Target::CubeMapPositiveZ,
	 *  5 = Target::CubeMapNegativeZ.
	 */
	static Target CubeMapFace(GLuint face)
	{
		assert(face <= 5);
		return Target(GL_TEXTURE_CUBE_MAP_POSITIVE_X+face);
	}

	using ObjBindingOps<tag::Texture>::Bind;
#if OGLPLUS_DOCUMENTATION_ONLY ||GL_VERSION_4_2 ||GL_ARB_shader_image_load_store
	using ObjBindingOps<tag::Texture>::BindImage;
#endif

	/// Binds this texture to the specified @p target
	/**
	 *  @glsymbols
	 *  @glfunref{BindTexture}
	 */
	void Bind(Target target) const
	{
		Bind(target, *this);
	}

#if OGLPLUS_DOCUMENTATION_ONLY ||GL_VERSION_4_2 ||GL_ARB_shader_image_load_store
	/// Binds a @p level of this texture to an image @p unit
	/**
	 *  @glvoereq{4,2,ARB,shader_image_load_store}
	 *  @glsymbols
	 *  @glfunref{BindImageTexture}
	 */
	void BindImage(
		ImageUnitSelector unit,
		GLint level,
		bool layered,
		GLint layer,
		AccessSpecifier access,
		ImageUnitFormat format
	) const
	{
		BindImage(unit, *this, level, layered, layer, access, format);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3
	/// Invalidates the specified level of texture image
	/**
	 *  @glverreq{4,3}
	 *  @glsymbols
	 *  @glfunref{InvalidateTexImage}
	 */
	void InvalidateImage(GLint level)
	{
		OGLPLUS_GLFUNC(InvalidateTexImage)(_obj_name(), level);
		OGLPLUS_CHECK(
			InvalidateTexImage,
			ObjectError,
			Object(*this).
			Index(level)
		);
	}

	/// Invalidates the specified part of texture image
	/**
	 *  @glverreq{4,3}
	 *  @glsymbols
	 *  @glfunref{InvalidateTexSubImage}
	 */
	void InvalidateSubImage(
		GLint level,
		GLint xoffs,
		GLint yoffs,
		GLint zoffs,
		SizeType width,
		SizeType height,
		SizeType depth
	)
	{
		OGLPLUS_GLFUNC(InvalidateTexSubImage)(
			_obj_name(),
			level,
			xoffs,
			yoffs,
			zoffs,
			width,
			height,
			depth
		);
		OGLPLUS_CHECK(
			InvalidateTexSubImage,
			ObjectError,
			Object(*this).
			Index(level)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_4
	/// Clears the specified level of texture image
	/**
	 *  @glverreq{4,4}
	 *  @glsymbols
	 *  @glfunref{ClearTexImage}
	 */
	template <typename GLtype>
	void ClearImage(
		GLint level,
		PixelDataFormat format,
		const GLtype* data
	)
	{
		OGLPLUS_GLFUNC(ClearTexImage)(
			_obj_name(),
			level,
			GLenum(format),
			GLenum(GetDataType<GLtype>()),
			data
		);
		OGLPLUS_CHECK(
			ClearTexImage,
			ObjectError,
			Object(*this).
			Index(level)
		);
	}

	/// Clears the specified part of texture image
	/**
	 *  @glverreq{4,4}
	 *  @glsymbols
	 *  @glfunref{ClearTexSubImage}
	 */
	template <typename GLtype>
	void ClearSubImage(
		GLint level,
		GLint xoffs,
		GLint yoffs,
		GLint zoffs,
		SizeType width,
		SizeType height,
		SizeType depth,
		PixelDataFormat format,
		const GLtype* data
	)
	{
		OGLPLUS_GLFUNC(ClearTexImage)(
			_obj_name(),
			level,
			xoffs,
			yoffs,
			zoffs,
			width,
			height,
			depth,
			GLenum(format),
			GLenum(GetDataType<GLtype>()),
			data
		);
		OGLPLUS_CHECK(
			ClearTexImage,
			ObjectError,
			Object(*this).
			Index(level)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3 || GL_ARB_texture_view
	/// References and reinteprets a subset of the data of another texture
	/**
	 *  @glvoereq{4,3,ARB,texture_view}
	 *  @glsymbols
	 *  @glfunref{TextureView}
	 */
	void View(
		Target target,
		TextureName orig_texture,
		PixelDataInternalFormat internal_format,
		GLuint min_level,
		GLuint num_levels,
		GLuint min_layer,
		GLuint num_layers
	)
	{
		OGLPLUS_GLFUNC(TextureView)(
			_obj_name(),
			GLenum(target),
			GetGLName(orig_texture),
			GLenum(internal_format),
			min_level,
			num_levels,
			min_layer,
			num_layers
		);
		OGLPLUS_CHECK(
			TextureView,
			ObjectError,
			Object(*this).
			BindTarget(target)
		);
	}
#endif
};

/// Operations applicable to any texture including texture 0 (zero)
/** @note Do not use this class directly, use Texture
 *  or DefaultTexture instead.
 */
template <>
class ObjZeroOps<tag::ExplicitSel, tag::Texture>
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
	{ }

	ObjZeroOps(const ObjZeroOps& that)
	OGLPLUS_NOEXCEPT(true)
	 : _base(static_cast<const _base&>(that))
	{ }

	ObjZeroOps& operator = (ObjZeroOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	{
		_base::operator = (static_cast<_base&&>(temp));
		return *this;
	}

	ObjZeroOps& operator = (const ObjZeroOps& that)
	OGLPLUS_NOEXCEPT(true)
	{
		_base::operator = (static_cast<const _base&>(that));
		return *this;
	}
#endif

	/// Types related to Texture
	struct Property
	{
		/// Depth texture comparison mode
		typedef TextureCompareMode CompareMode;

		/// Filter
		typedef TextureFilter Filter;

		/// Magnification filter
		typedef TextureMagFilter MagFilter;

		/// Minification filter
		typedef TextureMinFilter MinFilter;

		/// Texture swizzle coordinate
		typedef TextureSwizzleCoord SwizzleCoord;

		/// Texture swizzle value
		typedef TextureSwizzle Swizzle;

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle
		/// Texture swizzle tuple
		typedef TextureSwizzleTuple SwizzleTuple;
#endif

		/// Texture wrap mode for coordinate
		typedef TextureWrapCoord WrapCoord;

		/// Texture wrap mode value
		typedef TextureWrap Wrap;

		/// The pixel data type
		typedef OneOf<
			GLenum,
			std::tuple<
				DataType,
				PixelDataType
			>
		> PixDataType;
	};

	static GLint GetIntParam(Target target, GLenum query);
	static GLfloat GetFloatParam(Target target, GLenum query);

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	static GLint GetIntParam(Target target, GLint level, GLenum query);
	static GLfloat GetFloatParam(Target target, GLint level, GLenum query);

	/// Returns the width of the texture as it was specified by *Image*D
	/**
	 *  @see Height
	 *  @see Depth
	 *
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_WIDTH}
	 */
	static SizeType Width(Target target, GLint level = 0)
	{
		return MakeSizeType(
			GetIntParam(target, level, GL_TEXTURE_WIDTH),
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
	static SizeType Height(Target target, GLint level = 0)
	{
		return MakeSizeType(
			GetIntParam(target, level, GL_TEXTURE_HEIGHT),
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
	static SizeType Depth(Target target, GLint level = 0)
	{
		return MakeSizeType(
			GetIntParam(target, level, GL_TEXTURE_DEPTH),
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
	static PixelDataType RedType(Target target, GLint level = 0)
	{
		return PixelDataType(GetIntParam(
			target,
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
	static PixelDataType GreenType(Target target, GLint level = 0)
	{
		return PixelDataType(GetIntParam(
			target,
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
	static PixelDataType BlueType(Target target, GLint level = 0)
	{
		return PixelDataType(GetIntParam(
			target,
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
	static PixelDataType AlphaType(Target target, GLint level = 0)
	{
		return PixelDataType(GetIntParam(
			target,
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
	static PixelDataType DepthType(Target target, GLint level = 0)
	{
		return PixelDataType(GetIntParam(
			target,
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
	static SizeType RedSize(Target target, GLint level = 0)
	{
		return MakeSizeType(
			GetIntParam(target, level, GL_TEXTURE_RED_SIZE),
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
	static SizeType GreenSize(Target target, GLint level = 0)
	{
		return MakeSizeType(
			GetIntParam(target, level, GL_TEXTURE_GREEN_SIZE),
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
	static SizeType BlueSize(Target target, GLint level = 0)
	{
		return MakeSizeType(
			GetIntParam(target, level, GL_TEXTURE_BLUE_SIZE),
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
	static SizeType AlphaSize(Target target, GLint level = 0)
	{
		return MakeSizeType(
			GetIntParam(target, level, GL_TEXTURE_ALPHA_SIZE),
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
	static SizeType DepthSize(Target target, GLint level = 0)
	{
		return MakeSizeType(
			GetIntParam(target, level, GL_TEXTURE_DEPTH_SIZE),
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
	static SizeType StencilSize(Target target, GLint level = 0)
	{
		return MakeSizeType(
			GetIntParam(target, level, GL_TEXTURE_STENCIL_SIZE),
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
	static SizeType SharedSize(Target target, GLint level = 0)
	{
		return MakeSizeType(
			GetIntParam(target, level, GL_TEXTURE_SHARED_SIZE),
			std::nothrow
		);
	}

	/// Returns the size (in bytes) of the image array if it is compressed
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_COMPRESSED_IMAGE_SIZE}
	 */
	static SizeType CompressedImageSize(Target target, GLint level = 0)
	{
		return MakeSizeType(
			GetIntParam(
				target,
				level,
				GL_TEXTURE_COMPRESSED_IMAGE_SIZE
			), std::nothrow
		);
	}

	/// Returns the internal data format of the image array
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexLevelParameter}
	 *  @gldefref{TEXTURE_INTERNAL_FORMAT}
	 */
	static PixelDataInternalFormat InternalFormat(
		Target target,
		GLint level = 0
	)
	{
		return PixelDataInternalFormat(GetIntParam(
			target,
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
	static void GetImage(
		Target target,
		GLint level,
		PixelDataFormat format,
		const OutputData& dest
	);

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
	static void GetImage(
		Target target,
		GLint level,
		PixelDataFormat format,
		Property::PixDataType type,
		SizeType size,
		GLvoid* buffer
	)
	{
		GetImage(target, level, format, OutputData(type, size, buffer));
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
	static void GetCompressedImage(
		Target target,
		GLint level,
		const OutputData& dest
	);

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
	static void GetCompressedImage(
		Target target,
		GLint level,
		SizeType size,
		GLubyte* buffer
	)
	{
		GetCompressedImage(target, level, OutputData(size, buffer));
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
	static void GetCompressedImage(
		Target target,
		GLint level,
		std::vector<GLubyte>& dest
	);
#endif // GL_VERSION_3_0

	/// Specifies a three dimensional texture image
	/**
	 *  @glsymbols
	 *  @glfunref{TexImage3D}
	 */
	static void Image3D(
		Target target,
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
		OGLPLUS_GLFUNC(TexImage3D)(
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
			TexImage3D,
			ObjectError,
			ObjectBinding(target).
			EnumParam(internal_format).
			Index(level)
		);
	}

	/// Specifies a three dimensional texture image
	/**
	 *  @glsymbols
	 *  @glfunref{TexImage3D}
	 */
	static void Image3D(
		Target target,
		const images::Image& image,
		GLint level = 0,
		GLint border = 0
	);

	/// Specifies a three dimensional texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{TexSubImage3D}
	 */
	static void SubImage3D(
		Target target,
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
		OGLPLUS_GLFUNC(TexSubImage3D)(
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
			TexSubImage3D,
			ObjectError,
			ObjectBinding(target).
			EnumParam(format).
			Index(level)
		);
	}

	/// Specifies a three dimensional texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{TexSubImage3D}
	 */
	static void SubImage3D(
		Target target,
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
	static void Image2D(
		Target target,
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
		OGLPLUS_GLFUNC(TexImage2D)(
			GLenum(target),
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
			TexImage2D,
			ObjectError,
			ObjectBinding(target).
			EnumParam(internal_format).
			Index(level)
		);
	}

	/// Specifies a two dimensional texture image
	/**
	 *  @glsymbols
	 *  @glfunref{TexImage2D}
	 */
	static void Image2D(
		Target target,
		const images::Image& image,
		GLint level = 0,
		GLint border = 0
	);

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
	static void ImageCM(
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
		Target target = Target(GL_TEXTURE_CUBE_MAP_POSITIVE_X+face);
		OGLPLUS_GLFUNC(TexImage2D)(
			GLenum(target),
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
			TexImage2D,
			ObjectError,
			ObjectBinding(target).
			EnumParam(format).
			Index(level)
		);
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
	static void ImageCM(
		GLuint face,
		const images::Image& image,
		GLint level = 0,
		GLint border = 0
	);

	/// Specifies a two dimensional texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{TexSubImage2D}
	 */
	static void SubImage2D(
		Target target,
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
		OGLPLUS_GLFUNC(TexSubImage2D)(
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
			TexSubImage2D,
			ObjectError,
			ObjectBinding(target).
			EnumParam(format).
			Index(level)
		);
	}

	/// Specifies a two dimensional texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{TexSubImage2D}
	 */
	static void SubImage2D(
		Target target,
		const images::Image& image,
		GLint xoffs,
		GLint yoffs,
		GLint level = 0
	);

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Specifies a one dimensional texture image
	/**
	 *  @glsymbols
	 *  @glfunref{TexImage1D}
	 */
	static void Image1D(
		Target target,
		GLint level,
		PixelDataInternalFormat internal_format,
		SizeType width,
		GLint border,
		PixelDataFormat format,
		Property::PixDataType type,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(TexImage1D)(
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
			TexImage1D,
			ObjectError,
			ObjectBinding(target).
			EnumParam(internal_format).
			Index(level)
		);
	}

	/// Specifies a one dimensional texture image
	/**
	 *  @glsymbols
	 *  @glfunref{TexImage1D}
	 */
	static void Image1D(
		Target target,
		const images::Image& image,
		GLint level = 0,
		GLint border = 0
	);

	/// Specifies a one dimensional texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{TexSubImage1D}
	 */
	static void SubImage1D(
		Target target,
		GLint level,
		GLint xoffs,
		SizeType width,
		PixelDataFormat format,
		Property::PixDataType type,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(TexSubImage1D)(
			GLenum(target),
			level,
			xoffs,
			width,
			GLenum(format),
			GLenum(type),
			data
		);
		OGLPLUS_CHECK(
			TexSubImage1D,
			ObjectError,
			ObjectBinding(target).
			EnumParam(format).
			Index(level)
		);
	}

	/// Specifies a two dimensional texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{TexSubImage1D}
	 */
	static void SubImage1D(
		Target target,
		const images::Image& image,
		GLint xoffs,
		GLint level = 0
	);
#endif // GL_VERSION_3_0

	/// Specifies a texture image
	/**
	 *  @glsymbols
	 *  @glfunref{TexImage3D}
	 *  @glfunref{TexImage2D}
	 *  @glfunref{TexImage1D}
	 */
	static void Image(
		Target target,
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
	static void Image(
		Target target,
		const images::ImageSpec& image_spec,
		GLint level = 0,
		GLint border = 0
	);

	/// Copies a two dimensional texture image from the framebuffer
	/**
	 *  @glsymbols
	 *  @glfunref{CopyTexImage2D}
	 */
	static void CopyImage2D(
		Target target,
		GLint level,
		PixelDataInternalFormat internal_format,
		GLint x,
		GLint y,
		SizeType width,
		SizeType height,
		GLint border
	)
	{
		OGLPLUS_GLFUNC(CopyTexImage2D)(
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
			CopyTexImage2D,
			ObjectError,
			ObjectBinding(target).
			EnumParam(internal_format).
			Index(level)
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Copies a one dimensional texture image from the framebuffer
	/**
	 *  @glsymbols
	 *  @glfunref{CopyTexImage1D}
	 */
	static void CopyImage1D(
		Target target,
		GLint level,
		PixelDataInternalFormat internal_format,
		GLint x,
		GLint y,
		SizeType width,
		GLint border
	)
	{
		OGLPLUS_GLFUNC(CopyTexImage1D)(
			GLenum(target),
			level,
			GLenum(internal_format),
			x,
			y,
			width,
			border
		);
		OGLPLUS_CHECK(
			CopyTexImage1D,
			ObjectError,
			ObjectBinding(target).
			EnumParam(internal_format).
			Index(level)
		);
	}
#endif // GL_VERSION_3_0

	/// Copies a three dimensional texture sub image from the framebuffer
	/**
	 *  @glsymbols
	 *  @glfunref{CopyTexSubImage3D}
	 */
	static void CopySubImage3D(
		Target target,
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
		OGLPLUS_GLFUNC(CopyTexSubImage3D)(
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
			CopyTexSubImage3D,
			ObjectError,
			ObjectBinding(target).
			Index(level)
		);
	}

	/// Copies a two dimensional texture sub image from the framebuffer
	/**
	 *  @glsymbols
	 *  @glfunref{CopyTexSubImage2D}
	 */
	static void CopySubImage2D(
		Target target,
		GLint level,
		GLint xoffs,
		GLint yoffs,
		GLint x,
		GLint y,
		SizeType width,
		SizeType height
	)
	{
		OGLPLUS_GLFUNC(CopyTexSubImage2D)(
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
			CopyTexSubImage2D,
			ObjectError,
			ObjectBinding(target).
			Index(level)
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Copies a one dimensional texture sub image from the framebuffer
	/**
	 *  @glsymbols
	 *  @glfunref{CopyTexSubImage1D}
	 */
	static void CopySubImage1D(
		Target target,
		GLint level,
		GLint xoffs,
		GLint x,
		GLint y,
		SizeType width
	)
	{
		OGLPLUS_GLFUNC(CopyTexSubImage1D)(
			GLenum(target),
			level,
			xoffs,
			x,
			y,
			width
		);
		OGLPLUS_CHECK(
			CopyTexSubImage1D,
			ObjectError,
			ObjectBinding(target).
			Index(level)
		);
	}
#endif // GL_VERSION_3_0

	/// Specifies a three dimensional compressed texture image
	/**
	 *  @glsymbols
	 *  @glfunref{CompressedTexImage3D}
	 */
	static void CompressedImage3D(
		Target target,
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
		OGLPLUS_GLFUNC(CompressedTexImage3D)(
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
			CompressedTexImage3D,
			ObjectError,
			ObjectBinding(target).
			EnumParam(internal_format).
			Index(level)
		);
	}

	/// Specifies a two dimensional compressed texture image
	/**
	 *  @glsymbols
	 *  @glfunref{CompressedTexImage2D}
	 */
	static void CompressedImage2D(
		Target target,
		GLint level,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height,
		GLint border,
		SizeType image_size,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(CompressedTexImage2D)(
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
			CompressedTexImage2D,
			ObjectError,
			ObjectBinding(target).
			EnumParam(internal_format).
			Index(level)
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Specifies a one dimensional compressed texture image
	/**
	 *  @glsymbols
	 *  @glfunref{CompressedTexImage1D}
	 */
	static void CompressedImage1D(
		Target target,
		GLint level,
		PixelDataInternalFormat internal_format,
		SizeType width,
		GLint border,
		SizeType image_size,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(CompressedTexImage1D)(
			GLenum(target),
			level,
			GLenum(internal_format),
			width,
			border,
			image_size,
			data
		);
		OGLPLUS_CHECK(
			CompressedTexImage1D,
			ObjectError,
			ObjectBinding(target).
			EnumParam(internal_format).
			Index(level)
		);
	}
#endif // GL_VERSION_3_0

	/// Specifies a three dimensional compressed texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{CompressedTexSubImage3D}
	 */
	static void CompressedSubImage3D(
		Target target,
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
		OGLPLUS_GLFUNC(CompressedTexSubImage3D)(
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
			CompressedTexSubImage3D,
			ObjectError,
			ObjectBinding(target).
			EnumParam(format).
			Index(level)
		);
	}

	/// Specifies a two dimensional compressed texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{CompressedTexSubImage2D}
	 */
	static void CompressedSubImage2D(
		Target target,
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
		OGLPLUS_GLFUNC(CompressedTexSubImage2D)(
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
			CompressedTexSubImage2D,
			ObjectError,
			ObjectBinding(target).
			EnumParam(format).
			Index(level)
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Specifies a one dimensional compressed texture sub image
	/**
	 *  @glsymbols
	 *  @glfunref{CompressedTexSubImage1D}
	 */
	static void CompressedSubImage1D(
		Target target,
		GLint level,
		GLint xoffs,
		SizeType width,
		PixelDataFormat format,
		SizeType image_size,
		const void* data
	)
	{
		OGLPLUS_GLFUNC(CompressedTexSubImage1D)(
			GLenum(target),
			level,
			xoffs,
			width,
			GLenum(format),
			image_size,
			data
		);
		OGLPLUS_CHECK(
			CompressedTexSubImage1D,
			ObjectError,
			ObjectBinding(target).
			EnumParam(format).
			Index(level)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_2 || GL_ARB_texture_multisample
	/// Sets-up a three dimensional multisample texture
	/**
	 *  @glvoereq{3,2,ARB,texture_multisample}
	 *  @glsymbols
	 *  @glfunref{TexImage3DMultisample}
	 */
	static void Image3DMultisample(
		Target target,
		SizeType samples,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height,
		SizeType depth,
		Boolean fixed_sample_locations
	)
	{
		OGLPLUS_GLFUNC(TexImage3DMultisample)(
			GLenum(target),
			samples,
			GLenum(internal_format),
			width,
			height,
			depth,
			fixed_sample_locations._get()
		);
		OGLPLUS_CHECK(
			TexImage3DMultisample,
			ObjectError,
			ObjectBinding(target).
			EnumParam(internal_format)
		);
	}

	/// Sets-up a two dimensional multisample texture
	/**
	 *  @glvoereq{3,2,ARB,texture_multisample}
	 *  @glsymbols
	 *  @glfunref{TexImage2DMultisample}
	 */
	static void Image2DMultisample(
		Target target,
		SizeType samples,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height,
		Boolean fixed_sample_locations
	)
	{
		OGLPLUS_GLFUNC(TexImage2DMultisample)(
			GLenum(target),
			samples,
			GLenum(internal_format),
			width,
			height,
			fixed_sample_locations._get()
		);
		OGLPLUS_CHECK(
			TexImage2DMultisample,
			ObjectError,
			ObjectBinding(target).
			EnumParam(internal_format)
		);
	}
#endif // texture multisample

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_1
	/// Assigns a buffer storing the texel data to the texture
	/**
	 *  @glverreq{3,1}
	 *  @glsymbols
	 *  @glfunref{TexBuffer}
	 */
	static void Buffer(
		Target target,
		PixelDataInternalFormat internal_format,
		BufferName buffer
	)
	{
		OGLPLUS_GLFUNC(TexBuffer)(
			GLenum(target),
			GLenum(internal_format),
			GetGLName(buffer)
		);
		OGLPLUS_CHECK(
			TexBuffer,
			ObjectPairError,
			Subject(buffer).
			ObjectBinding(target).
			EnumParam(internal_format)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3
	/// Assigns a buffer range storing the texel data to the texture
	/**
	 *  @glverreq{4,3}
	 *  @glsymbols
	 *  @glfunref{TexBufferRange}
	 */
	static void BufferRange(
		Target target,
		PixelDataInternalFormat internal_format,
		BufferName buffer,
		GLintptr offset,
		BigSizeType size
	)
	{
		OGLPLUS_GLFUNC(TexBufferRange)(
			GLenum(target),
			GLenum(internal_format),
			GetGLName(buffer),
			offset,
			size
		);
		OGLPLUS_CHECK(
			TexBufferRange,
			ObjectPairError,
			Subject(buffer).
			ObjectBinding(target).
			EnumParam(internal_format)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_2 || GL_ARB_texture_storage
	/// Specifies all levels of 1D texture at the same time
	/**
	 *  @glvoereq{4,2,ARB,texture_storage}
	 *  @glsymbols
	 *  @glfunref{TexStorage1D}
	 */
	static void Storage1D(
		Target target,
		SizeType levels,
		PixelDataInternalFormat internal_format,
		SizeType width
	)
	{
		OGLPLUS_GLFUNC(TexStorage1D)(
			GLenum(target),
			levels,
			GLenum(internal_format),
			width
		);
		OGLPLUS_CHECK(
			TexStorage1D,
			ObjectError,
			ObjectBinding(target).
			EnumParam(internal_format)
		);
	}

	/// Specifies all levels of 2D texture at the same time
	/**
	 *  @glvoereq{4,2,ARB,texture_storage}
	 *  @glsymbols
	 *  @glfunref{TexStorage2D}
	 */
	static void Storage2D(
		Target target,
		SizeType levels,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height
	)
	{
		OGLPLUS_GLFUNC(TexStorage2D)(
			GLenum(target),
			levels,
			GLenum(internal_format),
			width,
			height
		);
		OGLPLUS_CHECK(
			TexStorage2D,
			ObjectError,
			ObjectBinding(target).
			EnumParam(internal_format)
		);
	}

	/// Specifies all levels of 3D texture at the same time
	/**
	 *  @glvoereq{4,2,ARB,texture_storage}
	 *  @glsymbols
	 *  @glfunref{TexStorage3D}
	 */
	static void Storage3D(
		Target target,
		SizeType levels,
		PixelDataInternalFormat internal_format,
		SizeType width,
		SizeType height,
		SizeType depth
	)
	{
		OGLPLUS_GLFUNC(TexStorage3D)(
			GLenum(target),
			levels,
			GLenum(internal_format),
			width,
			height,
			depth
		);
		OGLPLUS_CHECK(
			TexStorage3D,
			ObjectError,
			ObjectBinding(target).
			EnumParam(internal_format)
		);
	}
#endif

	/// Returns the texture base level (TEXTURE_BASE_LEVEL)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_BASE_LEVEL}
	 */
	static GLuint BaseLevel(Target target)
	{
		return GLuint(GetIntParam(target, GL_TEXTURE_BASE_LEVEL));
	}

	/// Sets the texture base level (TEXTURE_BASE_LEVEL)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_BASE_LEVEL}
	 */
	static void BaseLevel(Target target, GLint level)
	{
		OGLPLUS_GLFUNC(TexParameteri)(
			GLenum(target),
			GL_TEXTURE_BASE_LEVEL,
			level
		);
		OGLPLUS_CHECK(
			TexParameteri,
			ObjectError,
			ObjectBinding(target).
			Index(level)
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Gets the texture border color (TEXTURE_BORDER_COLOR)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_BORDER_COLOR}
	 */
	static Vector<GLfloat, 4> BorderColor(Target target, TypeTag<GLfloat>)
	{
		GLfloat result[4];
		OGLPLUS_GLFUNC(GetTexParameterfv)(
			GLenum(target),
			GL_TEXTURE_BORDER_COLOR,
			result
		);
		OGLPLUS_CHECK(
			GetTexParameterfv,
			ObjectError,
			ObjectBinding(target)
		);
		return Vector<GLfloat, 4>(result, 4);
	}

	/// Sets the texture border color (TEXTURE_BORDER_COLOR)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_BORDER_COLOR}
	 */
	static void BorderColor(Target target, Vector<GLfloat, 4> color)
	{
		OGLPLUS_GLFUNC(TexParameterfv)(
			GLenum(target),
			GL_TEXTURE_BORDER_COLOR,
			Data(color)
		);
		OGLPLUS_CHECK(
			TexParameterfv,
			ObjectError,
			ObjectBinding(target)
		);
	}

	/// Gets the texture border color (TEXTURE_BORDER_COLOR)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_BORDER_COLOR}
	 */
	static Vector<GLint, 4> BorderColor(Target target, TypeTag<GLint>)
	{
		GLint result[4];
		OGLPLUS_GLFUNC(GetTexParameterIiv)(
			GLenum(target),
			GL_TEXTURE_BORDER_COLOR,
			result
		);
		OGLPLUS_CHECK(
			GetTexParameterIiv,
			ObjectError,
			ObjectBinding(target)
		);
		return Vector<GLint, 4>(result, 4);
	}

	/// Sets the texture border color (TEXTURE_BORDER_COLOR)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_BORDER_COLOR}
	 */
	static void BorderColor(Target target, Vector<GLint, 4> color)
	{
		OGLPLUS_GLFUNC(TexParameterIiv)(
			GLenum(target),
			GL_TEXTURE_BORDER_COLOR,
			Data(color)
		);
		OGLPLUS_CHECK(
			TexParameterIiv,
			ObjectError,
			ObjectBinding(target)
		);
	}

	/// Gets the texture border color (TEXTURE_BORDER_COLOR)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_BORDER_COLOR}
	 */
	static Vector<GLuint, 4> BorderColor(Target target, TypeTag<GLuint>)
	{
		GLuint result[4];
		OGLPLUS_GLFUNC(GetTexParameterIuiv)(
			GLenum(target),
			GL_TEXTURE_BORDER_COLOR,
			result
		);
		OGLPLUS_CHECK(
			GetTexParameterIuiv,
			ObjectError,
			ObjectBinding(target)
		);
		return Vector<GLuint, 4>(result, 4);
	}

	/// Sets the texture border color (TEXTURE_BORDER_COLOR)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_BORDER_COLOR}
	 */
	static void BorderColor(Target target, Vector<GLuint, 4> color)
	{
		OGLPLUS_GLFUNC(TexParameterIuiv)(
			GLenum(target),
			GL_TEXTURE_BORDER_COLOR,
			Data(color)
		);
		OGLPLUS_CHECK(
			TexParameterIuiv,
			ObjectError,
			ObjectBinding(target)
		);
	}
#endif // GL_VERSION_3_0

	/// Gets the compare mode (TEXTURE_COMPARE_MODE)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_COMPARE_MODE}
	 */
	static TextureCompareMode CompareMode(Target target)
	{
		return TextureCompareMode(GetIntParam(
			target,
			GL_TEXTURE_COMPARE_MODE
		));
	}

	/// Sets the compare mode (TEXTURE_COMPARE_MODE)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_COMPARE_MODE}
	 */
	static void CompareMode(Target target, TextureCompareMode mode)
	{
		OGLPLUS_GLFUNC(TexParameteri)(
			GLenum(target),
			GL_TEXTURE_COMPARE_MODE,
			GLint(mode)
		);
		OGLPLUS_CHECK(
			TexParameteri,
			ObjectError,
			ObjectBinding(target).
			EnumParam(mode)
		);
	}

	/// Sets the compare function (TEXTURE_COMPARE_FUNC)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_COMPARE_FUNC}
	 */
	static CompareFunction CompareFunc(Target target)
	{
		return CompareFunction(GetIntParam(
			target,
			GL_TEXTURE_COMPARE_FUNC
		));
	}

	/// Sets the compare function (TEXTURE_COMPARE_FUNC)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_COMPARE_FUNC}
	 */
	static void CompareFunc(Target target, CompareFunction func)
	{
		OGLPLUS_GLFUNC(TexParameteri)(
			GLenum(target),
			GL_TEXTURE_COMPARE_FUNC,
			GLint(func)
		);
		OGLPLUS_CHECK(
			TexParameteri,
			ObjectError,
			ObjectBinding(target).
			EnumParam(func)
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Gets the LOD bias value (TEXTURE_LOD_BIAS)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_LOD_BIAS}
	 */
	static GLfloat LODBias(Target target)
	{
		return GetFloatParam(target, GL_TEXTURE_LOD_BIAS);
	}

	/// Sets the LOD bias value (TEXTURE_LOD_BIAS)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_LOD_BIAS}
	 */
	static void LODBias(Target target, GLfloat value)
	{
		OGLPLUS_GLFUNC(TexParameterf)(
			GLenum(target),
			GL_TEXTURE_LOD_BIAS,
			value
		);
		OGLPLUS_CHECK(
			TexParameterf,
			ObjectError,
			ObjectBinding(target)
		);
	}
#endif // GL_VERSION_3_0

	/// Sets both the minification and magnification filter
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_MIN_FILTER}
	 *  @gldefref{TEXTURE_MAG_FILTER}
	 */
	static void Filter(Target target, TextureFilter filter)
	{
		OGLPLUS_GLFUNC(TexParameteri)(
			GLenum(target),
			GL_TEXTURE_MIN_FILTER,
			GLint(filter)
		);
		OGLPLUS_CHECK(
			TexParameteri,
			ObjectError,
			ObjectBinding(target).
			EnumParam(filter)
		);
		OGLPLUS_GLFUNC(TexParameteri)(
			GLenum(target),
			GL_TEXTURE_MAG_FILTER,
			GLint(filter)
		);
		OGLPLUS_CHECK(
			TexParameteri,
			ObjectError,
			ObjectBinding(target).
			EnumParam(filter)
		);
	}

	/// Gets the magnification filter (TEXTURE_MAG_FILTER)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_MAG_FILTER}
	 */
	static TextureMagFilter MagFilter(Target target)
	{
		return TextureMagFilter(GetIntParam(
			target,
			GL_TEXTURE_MAG_FILTER
		));
	}

	/// Sets the magnification filter (TEXTURE_MAG_FILTER)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_MAG_FILTER}
	 */
	static void MagFilter(Target target, TextureMagFilter filter)
	{
		OGLPLUS_GLFUNC(TexParameteri)(
			GLenum(target),
			GL_TEXTURE_MAG_FILTER,
			GLint(filter)
		);
		OGLPLUS_CHECK(
			TexParameteri,
			ObjectError,
			ObjectBinding(target).
			EnumParam(filter)
		);
	}

	/// Gets the minification filter (TEXTURE_MIN_FILTER)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_MIN_FILTER}
	 */
	static TextureMinFilter MinFilter(Target target)
	{
		return TextureMinFilter(GetIntParam(
			target,
			GL_TEXTURE_MIN_FILTER
		));
	}

	/// Sets the minification filter (TEXTURE_MIN_FILTER)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_MIN_FILTER}
	 */
	static void MinFilter(Target target, TextureMinFilter filter)
	{
		OGLPLUS_GLFUNC(TexParameteri)(
			GLenum(target),
			GL_TEXTURE_MIN_FILTER,
			GLint(filter)
		);
		OGLPLUS_CHECK(
			TexParameteri,
			ObjectError,
			ObjectBinding(target).
			EnumParam(filter)
		);
	}

	/// Gets minimal LOD value (TEXTURE_MIN_LOD)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_MIN_LOD}
	 */
	static GLfloat MinLOD(Target target)
	{
		return GetFloatParam(target, GL_TEXTURE_MIN_LOD);
	}

	/// Sets minimal LOD value (TEXTURE_MIN_LOD)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_MIN_LOD}
	 */
	static void MinLOD(Target target, GLfloat value)
	{
		OGLPLUS_GLFUNC(TexParameterf)(
			GLenum(target),
			GL_TEXTURE_MIN_LOD,
			value
		);
		OGLPLUS_CHECK(
			TexParameterf,
			ObjectError,
			ObjectBinding(target)
		);
	}

	/// Gets maximum LOD value (TEXTURE_MAX_LOD)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_MAX_LOD}
	 */
	static GLfloat MaxLOD(Target target)
	{
		return GetFloatParam(target, GL_TEXTURE_MAX_LOD);
	}

	/// Sets maximum LOD value (TEXTURE_MAX_LOD)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_MAX_LOD}
	 */
	static void MaxLOD(Target target, GLfloat value)
	{
		OGLPLUS_GLFUNC(TexParameterf)(
			GLenum(target),
			GL_TEXTURE_MAX_LOD,
			value
		);
		OGLPLUS_CHECK(
			TexParameterf,
			ObjectError,
			ObjectBinding(target)
		);
	}

	/// Gets maximum level value (TEXTURE_MAX_LEVEL)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_MAX_LEVEL}
	 */
	static GLint MaxLevel(Target target)
	{
		return GetIntParam(target, GL_TEXTURE_MAX_LEVEL);
	}

	/// Sets maximum level value (TEXTURE_MAX_LEVEL)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_MAX_LEVEL}
	 */
	static void MaxLevel(Target target, GLint value)
	{
		OGLPLUS_GLFUNC(TexParameteri)(
			GLenum(target),
			GL_TEXTURE_MAX_LEVEL,
			value
		);
		OGLPLUS_CHECK(
			TexParameteri,
			ObjectError,
			ObjectBinding(target)
		);
	}

	/// Gets the maximum anisotropy level
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{MAX_TEXTURE_MAX_ANISOTROPY_EXT}
	 */
	static GLfloat MaxAnisotropy(Target target)
	{
#ifdef  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT
		return GetFloatParam(
			target,
			GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT
		);
#else
		OGLPLUS_FAKE_USE(target);
		return GLfloat(1);
#endif
	}

	/// Gets the current anisotropy level
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_MAX_ANISOTROPY_EXT}
	 */
	static GLfloat Anisotropy(Target target)
	{
#ifdef  GL_TEXTURE_MAX_ANISOTROPY_EXT
		return GetFloatParam(
			target,
			GL_TEXTURE_MAX_ANISOTROPY_EXT
		);
#else
		OGLPLUS_FAKE_USE(target);
		return GLfloat(1);
#endif
	}

	/// Sets the anisotropy level
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_MAX_ANISOTROPY_EXT}
	 */
	static void Anisotropy(Target target, GLfloat value)
	{
#ifdef  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT
		OGLPLUS_GLFUNC(TexParameterf)(
			GLenum(target),
			GL_TEXTURE_MAX_ANISOTROPY_EXT,
			value
		);
		OGLPLUS_CHECK(
			TexParameterf,
			ObjectError,
			ObjectBinding(target)
		);
#else
		OGLPLUS_FAKE_USE(target);
		OGLPLUS_FAKE_USE(value);
#endif
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle
	/// Gets the swizzle parameter (TEXTURE_SWIZZLE_*)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 */
	static TextureSwizzle Swizzle(Target target, TextureSwizzleCoord coord)
	{
		return TextureSwizzle(GetIntParam(
			target,
			GLenum(coord)
		));
	}

	/// Sets the swizzle parameter (TEXTURE_SWIZZLE_*)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 */
	static void Swizzle(
		Target target,
		TextureSwizzleCoord coord,
		TextureSwizzle mode
	)
	{
		OGLPLUS_GLFUNC(TexParameteri)(
			GLenum(target),
			GLenum(coord),
			GLint(mode)
		);
		OGLPLUS_CHECK(
			TexParameteri,
			ObjectError,
			ObjectBinding(target).
			EnumParam(mode)
		);
	}

	/// Gets the swizzle parameter (TEXTURE_SWIZZLE_R)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_R}
	 */
	static TextureSwizzle SwizzleR(Target target)
	{
		return Swizzle(target, TextureSwizzleCoord::R);
	}

	/// Sets the swizzle parameter (TEXTURE_SWIZZLE_R)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_R}
	 */
	static void SwizzleR(Target target, TextureSwizzle mode)
	{
		Swizzle(target, TextureSwizzleCoord::R, mode);
	}

	/// Gets the swizzle parameter (TEXTURE_SWIZZLE_G)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_G}
	 */
	static TextureSwizzle SwizzleG(Target target)
	{
		return Swizzle(target, TextureSwizzleCoord::G);
	}

	/// Sets the swizzle parameter (TEXTURE_SWIZZLE_G)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_G}
	 */
	static void SwizzleG(Target target, TextureSwizzle mode)
	{
		Swizzle(target, TextureSwizzleCoord::G, mode);
	}

	/// Gets the swizzle parameter (TEXTURE_SWIZZLE_B)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_B}
	 */
	static TextureSwizzle SwizzleB(Target target)
	{
		return Swizzle(target, TextureSwizzleCoord::B);
	}

	/// Sets the swizzle parameter (TEXTURE_SWIZZLE_B)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_B}
	 */
	static void SwizzleB(Target target, TextureSwizzle mode)
	{
		Swizzle(target, TextureSwizzleCoord::B, mode);
	}

	/// Gets the swizzle parameter (TEXTURE_SWIZZLE_A)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_A}
	 */
	static TextureSwizzle SwizzleA(Target target)
	{
		return Swizzle(target, TextureSwizzleCoord::A);
	}

	/// Sets the swizzle parameter (TEXTURE_SWIZZLE_A)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_A}
	 */
	static void SwizzleA(Target target, TextureSwizzle mode)
	{
		Swizzle(target, TextureSwizzleCoord::A, mode);
	}

	/// Gets the swizzle parameter (TEXTURE_SWIZZLE_RGBA)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_RGBA}
	 */
	static TextureSwizzleTuple SwizzleRGBA(Target target)
	{
		TextureSwizzleTuple result;
		OGLPLUS_GLFUNC(GetTexParameteriv)(
			GLenum(target),
			GL_TEXTURE_SWIZZLE_RGBA,
			result.Values()
		);
		OGLPLUS_CHECK(
			GetTexParameteriv,
			ObjectError,
			ObjectBinding(target)
		);
		return result;
	}

	/// Sets the RGBA swizzle parameter (TEXTURE_SWIZZLE_RGBA)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_RGBA}
	 */
	static void SwizzleRGBA(Target target, TextureSwizzle mode)
	{
		GLint m = GLint(GLenum(mode));
		GLint params[4] = {m, m, m, m};

		OGLPLUS_GLFUNC(TexParameteriv)(
			GLenum(target),
			GL_TEXTURE_SWIZZLE_RGBA,
			params
		);
		OGLPLUS_CHECK(
			TexParameteriv,
			ObjectError,
			ObjectBinding(target).
			EnumParam(mode)
		);
	}

	/// Sets the RGBA swizzle parameters (TEXTURE_SWIZZLE_RGBA)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_RGBA}
	 */
	static void SwizzleRGBA(
		Target target,
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

		OGLPLUS_GLFUNC(TexParameteriv)(
			GLenum(target),
			GL_TEXTURE_SWIZZLE_RGBA,
			params
		);
		OGLPLUS_CHECK(
			TexParameteriv,
			ObjectError,
			ObjectBinding(target)
		);
	}

	/// Sets the RGBA swizzle parameters (TEXTURE_SWIZZLE_RGBA)
	/**
	 *  @glvoereq{3,3,ARB,texture_swizzle}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_SWIZZLE_RGBA}
	 */
	static void SwizzleRGBA(Target target, const TextureSwizzleTuple& modes)
	{
		OGLPLUS_GLFUNC(TexParameteriv)(
			GLenum(target),
			GL_TEXTURE_SWIZZLE_RGBA,
			modes.Values()
		);
		OGLPLUS_CHECK(
			TexParameteriv,
			ObjectError,
			ObjectBinding(target)
		);
	}
#endif // texture swizzle

	/// Gets the wrap parameter (TEXTURE_WRAP_*)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 */
	static TextureWrap Wrap(Target target, TextureWrapCoord coord)
	{
		return TextureWrap(GetIntParam(
			target,
			GLenum(coord)
		));
	}

	/// Sets the wrap parameter (TEXTURE_WRAP_*)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 */
	static void Wrap(
		Target target,
		TextureWrapCoord coord,
		TextureWrap mode
	)
	{
		OGLPLUS_GLFUNC(TexParameteri)(
			GLenum(target),
			GLenum(coord),
			GLint(mode)
		);
		OGLPLUS_CHECK(
			TexParameteri,
			ObjectError,
			ObjectBinding(target).
			EnumParam(mode)
		);
	}

	/// Gets the wrap parameter (TEXTURE_WRAP_S)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_WRAP_S}
	 */
	static TextureWrap WrapS(Target target)
	{
		return Wrap(target, TextureWrapCoord::S);
	}

	/// Sets the wrap parameter (TEXTURE_WRAP_S)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_WRAP_S}
	 */
	static void WrapS(Target target, TextureWrap mode)
	{
		Wrap(target, TextureWrapCoord::S, mode);
	}

	/// Gets the wrap parameter (TEXTURE_WRAP_T)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_WRAP_T}
	 */
	static TextureWrap WrapT(Target target)
	{
		return Wrap(target, TextureWrapCoord::T);
	}

	/// Sets the wrap parameter (TEXTURE_WRAP_T)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_WRAP_T}
	 */
	static void WrapT(Target target, TextureWrap mode)
	{
		Wrap(target, TextureWrapCoord::T, mode);
	}

	/// Gets the wrap parameter (TEXTURE_WRAP_R)
	/**
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_WRAP_R}
	 */
	static TextureWrap WrapR(Target target)
	{
		return Wrap(target, TextureWrapCoord::R);
	}

	/// Sets the wrap parameter (TEXTURE_WRAP_R)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_WRAP_R}
	 */
	static void WrapR(Target target, TextureWrap mode)
	{
		Wrap(target, TextureWrapCoord::R, mode);
	}

	/// Sets the wrap parameter (TEXTURE_WRAP_*)
	/**
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_WRAP_R}
	 *  @gldefref{TEXTURE_WRAP_T}
	 *  @gldefref{TEXTURE_WRAP_S}
	 */
	static void Wrap(Target target, TextureWrap mode)
	{
		switch(TextureTargetDimensions(target))
		{
			case 3: WrapR(target, mode);
				OGLPLUS_FALLTHROUGH
			case 2: WrapT(target, mode);
				OGLPLUS_FALLTHROUGH
			case 1: WrapS(target, mode);
				OGLPLUS_FALLTHROUGH
			case 0: break;
			default: OGLPLUS_ABORT("Invalid texture wrap dimension");
		}
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3
	/// Gets the depth stencil mode parameter (DEPTH_STENCIL_TEXTURE_MODE)
	/**
	 *  @glverreq{4,3}
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{DEPTH_STENCIL_TEXTURE_MODE}
	 */
	static PixelDataFormat DepthStencilMode(Target target)
	{
		return PixelDataFormat(GetIntParam(
			target,
			GL_DEPTH_STENCIL_TEXTURE_MODE
		));
	}

	/// Sets the depth stencil mode (DEPTH_STENCIL_TEXTURE_MODE)
	/**
	 *  @glverreq{4,3}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{DEPTH_STENCIL_TEXTURE_MODE}
	 */
	static void DepthStencilMode(
		Target target,
		PixelDataFormat mode
	)
	{
		OGLPLUS_GLFUNC(TexParameteri)(
			GLenum(target),
			GL_DEPTH_STENCIL_TEXTURE_MODE,
			GLint(mode)
		);
		OGLPLUS_CHECK(
			TexParameteri,
			ObjectError,
			ObjectBinding(target).
			EnumParam(mode)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_ARB_seamless_cubemap_per_texture
	/// Gets the seamless cubemap setting value
	/**
	 *  @glextreq{ARB,seamless_cubemap_per_texture}
	 *  @glsymbols
	 *  @glfunref{GetTexParameter}
	 *  @gldefref{TEXTURE_CUBE_MAP_SEAMLESS}
	 */
	static Boolean Seamless(Target target)
	{
		return Boolean(
			GetIntParam(
				target,
				GL_TEXTURE_CUBE_MAP_SEAMLESS
			), std::nothrow
		);
	}

	/// Sets the seamless cubemap setting
	/**
	 *  @glextreq{ARB,seamless_cubemap_per_texture}
	 *  @glsymbols
	 *  @glfunref{TexParameter}
	 *  @gldefref{TEXTURE_CUBE_MAP_SEAMLESS}
	 */
	static void Seamless(Target target, Boolean enable)
	{
		OGLPLUS_GLFUNC(TexParameteri)(
			GLenum(target),
			GL_TEXTURE_CUBE_MAP_SEAMLESS,
			enable._get()
		);
		OGLPLUS_CHECK(
			TexParameteri,
			ObjectError,
			ObjectBinding(target)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || \
	GL_VERSION_4_5 || \
	GL_ARB_texture_barrier || \
	GL_NV_texture_barrier
	/// Ensures that texture writes have been completed
	/**
	 *  @glextreq{NV,texture_barrier}
	 *  @glsymbols
	 *  @glfunref{TextureBarrierNV}
	 */
	static void Barrier(void)
	{
#if GL_VERSION_4_5 || GL_ARB_texture_barrier
		OGLPLUS_GLFUNC(TextureBarrier)();
		OGLPLUS_VERIFY_SIMPLE(TextureBarrier);
#elif GL_NV_texture_barrier
		OGLPLUS_GLFUNC(TextureBarrierNV)();
		OGLPLUS_VERIFY_SIMPLE(TextureBarrierNV);
#endif
	}
#endif

	/// Generate mipmap for the texture bound to the specified target
	/**
	 *  @glsymbols
	 *  @glfunref{GenerateMipmap}
	 */
	static void GenerateMipmap(Target target)
	{
		OGLPLUS_GLFUNC(GenerateMipmap)(GLenum(target));
		OGLPLUS_CHECK(
			GenerateMipmap,
			ObjectError,
			ObjectBinding(target)
		);
	}
};

/// DefaultTexture operations with explicit selector
typedef ObjZeroOps<tag::ExplicitSel, tag::Texture>
	DefaultTextureOps;

/// Texture operations with explicit selector
typedef ObjectOps<tag::ExplicitSel, tag::Texture>
	TextureOps;


/// Selector type used with the syntax sugar operators
struct TextureMipmap { };

// Helper class for syntax-sugar operators
struct TextureTargetAndSlot
{
	TextureTarget target;
	GLint slot;

	TextureTargetAndSlot(TextureTarget& t, GLint s)
	 : target(t)
	 , slot(s)
	{ }
};

// syntax sugar operators
inline TextureTargetAndSlot operator | (TextureTarget target, GLint slot)
{
	return TextureTargetAndSlot(target, slot);
}

// helper class for syntax-sugar operators
struct TextureUnitAndTarget
{
	TextureUnitSelector unit;
	TextureTarget tgt;

	TextureUnitAndTarget(TextureUnitSelector u, TextureTarget t)
	 : unit(u)
	 , tgt(t)
	{ }
};

// syntax sugar operators
inline TextureUnitAndTarget operator | (
	TextureUnitSelector unit,
	TextureTarget tex
)
{
	return TextureUnitAndTarget(unit, tex);
}

// Bind
inline TextureTarget operator << (
	const DefaultTextureOps& tex,
	TextureTarget target
)
{
	tex.Bind(target);
	return target;
}

// Filter
inline TextureTarget operator << (TextureTarget target, TextureFilter filter)
{
	DefaultTextureOps::Filter(target, filter);
	return target;
}

// MinFilter
inline TextureTarget operator << (TextureTarget target, TextureMinFilter filter)
{
	DefaultTextureOps::MinFilter(target, filter);
	return target;
}

// MagFilter
inline TextureTarget operator << (TextureTarget target, TextureMagFilter filter)
{
	DefaultTextureOps::MagFilter(target, filter);
	return target;
}

// CompareMode
inline TextureTarget operator << (TextureTarget target, TextureCompareMode mode)
{
	DefaultTextureOps::CompareMode(target, mode);
	return target;
}

// CompareFunc
inline TextureTarget operator << (TextureTarget target, CompareFunction func)
{
	DefaultTextureOps::CompareFunc(target, func);
	return target;
}

// Wrap
inline TextureTarget operator << (TextureTarget target, TextureWrap mode)
{
	DefaultTextureOps::Wrap(target, mode);
	return target;
}

// Wrap
inline TextureTargetAndSlot operator << (
	TextureTargetAndSlot tas,
	TextureWrap wrap
)
{
	switch(tas.slot)
	{
		case 0: DefaultTextureOps::WrapS(tas.target, wrap); break;
		case 1: DefaultTextureOps::WrapT(tas.target, wrap); break;
		case 2: DefaultTextureOps::WrapR(tas.target, wrap); break;
		default: OGLPLUS_ABORT("Invalid texture wrap slot");
	}
	return tas;
}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_3 || GL_ARB_texture_swizzle
// Swizzle
inline TextureTarget operator << (TextureTarget target, TextureSwizzle swizzle)
{
	DefaultTextureOps::SwizzleRGBA(target, swizzle);
	return target;
}

// Swizzle
inline TextureTargetAndSlot operator << (
	TextureTargetAndSlot tas,
	TextureSwizzle swizzle
)
{
	switch(tas.slot)
	{
		case 0: DefaultTextureOps::SwizzleR(tas.target, swizzle); break;
		case 1: DefaultTextureOps::SwizzleG(tas.target, swizzle); break;
		case 2: DefaultTextureOps::SwizzleB(tas.target, swizzle); break;
		case 3: DefaultTextureOps::SwizzleA(tas.target, swizzle); break;
		default: OGLPLUS_ABORT("Invalid texture swizzle slot");
	}
	return tas;
}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
// BorderColor
template <typename T>
inline TextureTarget operator << (
	TextureTarget target,
	const Vector<T, 4>& col
)
{
	DefaultTextureOps::BorderColor(target, col);
	return target;
}
#endif

// Image
inline TextureTarget operator << (
	TextureTarget target,
	const images::Image& image
)
{
	DefaultTextureOps::Image(target, image);
	return target;
}

// Image
inline TextureTarget operator << (
	TextureTarget target,
	const images::ImageSpec& image_spec
)
{
	DefaultTextureOps::Image(target, image_spec);
	return target;
}

// Image + Level
inline TextureTarget operator << (
	TextureTargetAndSlot tas,
	const images::Image& image
)
{
	DefaultTextureOps::Image(tas.target, image, tas.slot);
	return tas.target;
}

// Image + Level
inline TextureTarget operator << (
	TextureTargetAndSlot tas,
	const images::ImageSpec& image_spec
)
{
	DefaultTextureOps::Image(tas.target, image_spec, tas.slot);
	return tas.target;
}

// GenerateMipmap
inline TextureTarget operator << (
	TextureTarget target,
	TextureMipmap
)
{
	DefaultTextureOps::GenerateMipmap(target);
	return target;
}

/// An @ref oglplus_object encapsulating the default texture functionality
/**
 *  @ingroup oglplus_objects
 */
typedef ObjectZero<DefaultTextureOps> DefaultTexture;

/// An @ref oglplus_object encapsulating the texture object functionality
/**
 *  @ingroup oglplus_objects
 */
typedef Object<TextureOps> Texture;

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/texture.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

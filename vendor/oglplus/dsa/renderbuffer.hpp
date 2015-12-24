/**
 *  @file oglplus/dsa/renderbuffer.hpp
 *  @brief Renderbuffer object wrappers with direct state access
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_DSA_RENDERBUFFER_1107121519_HPP
#define OGLPLUS_DSA_RENDERBUFFER_1107121519_HPP

#include <oglplus/renderbuffer.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_5 || GL_ARB_direct_state_access

template <>
struct ObjGenTag<tag::DirectState, tag::Renderbuffer>
{
	typedef tag::Create Type;
};

/// Class wrapping renderbuffer-related functionality with direct state access
/** @note Do not use this class directly, use DSARenderbuffer instead.
 *
 */
template <>
class ObjectOps<tag::DirectState, tag::Renderbuffer>
 : public ObjZeroOps<tag::DirectState, tag::Renderbuffer>
{
protected:
	ObjectOps(RenderbufferName name)
	OGLPLUS_NOEXCEPT(true)
	 : ObjZeroOps<tag::DirectState, tag::Renderbuffer>(name)
	{ }
public:
#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	ObjectOps(ObjectOps&&) = default;
	ObjectOps(const ObjectOps&) = default;
	ObjectOps& operator = (ObjectOps&&) = default;
	ObjectOps& operator = (const ObjectOps&) = default;
#else
	typedef ObjZeroOps<tag::DirectState, tag::Renderbuffer> _base;

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
	GLint GetIntParam(GLenum query) const;

	/// Set the renderbuffer storage parameters
	/**
	 *  @glsymbols
	 *  @glfunref{RenderbufferStorage}
	 */
	void Storage(
		PixelDataInternalFormat internalformat,
		SizeType width,
		SizeType height
	)
	{
		OGLPLUS_GLFUNC(NamedRenderbufferStorage)(
			_obj_name(),
			GLenum(internalformat),
			width,
			height
		);
		OGLPLUS_CHECK(
			NamedRenderbufferStorage,
			ObjectError,
			Object(*this).
			EnumParam(internalformat)
		);
	}

	/// Set the renderbuffer storage parameters
	/**
	 *  @glsymbols
	 *  @glfunref{RenderbufferStorage}
	 */
	void Storage(const images::ImageSpec& image_spec);

	/// Set the renderbuffer multisample storage parameters
	/**
	 *  @glsymbols
	 *  @glfunref{RenderbufferStorageMultisample}
	 */
	void StorageMultisample(
		SizeType samples,
		PixelDataInternalFormat internalformat,
		SizeType width,
		SizeType height
	)
	{
		OGLPLUS_GLFUNC(NamedRenderbufferStorageMultisample)(
			_obj_name(),
			samples,
			GLenum(internalformat),
			width,
			height
		);
		OGLPLUS_CHECK(
			NamedRenderbufferStorageMultisample,
			ObjectError,
			Object(*this).
			EnumParam(internalformat)
		);
	}


	/// Returns the width of the renderbuffer as it was specified by Storage*
	/**
	 *  @see Height
	 *
	 *  @glsymbols
	 *  @glfunref{GetRenderbufferParameter}
	 *  @gldefref{RENDERBUFFER_WIDTH}
	 */
	SizeType Width(void) const
	{
		return MakeSizeType(
			GetIntParam(GL_RENDERBUFFER_WIDTH),
			std::nothrow
		);
	}

	/// Returns the height of the renderbuffer as it was specified by Storage*
	/**
	 *  @see Width
	 *
	 *  @glsymbols
	 *  @glfunref{GetRenderbufferParameter}
	 *  @gldefref{RENDERBUFFER_HEIGHT}
	 */
	SizeType Height(void) const
	{
		return MakeSizeType(
			GetIntParam(GL_RENDERBUFFER_HEIGHT),
			std::nothrow
		);
	}

	/// Returns the size in bits of the renderbuffer's red component
	/**
	 *  @see Green
	 *  @see Blue
	 *  @see Alpha
	 *
	 *  @glsymbols
	 *  @glfunref{GetRenderbufferParameter}
	 *  @gldefref{RENDERBUFFER_RED_SIZE}
	 */
	SizeType RedSize(void) const
	{
		return MakeSizeType(
			GetIntParam(GL_RENDERBUFFER_RED_SIZE),
			std::nothrow
		);
	}

	/// Returns the size in bits of the renderbuffer's green component
	/**
	 *  @see RedSize
	 *  @see BlueSize
	 *  @see AlphaSize
	 *  @see DepthSize
	 *  @see StencilSize
	 *
	 *  @glsymbols
	 *  @glfunref{GetRenderbufferParameter}
	 *  @gldefref{RENDERBUFFER_GREEN_SIZE}
	 */
	SizeType GreenSize(void) const
	{
		return MakeSizeType(
			GetIntParam(GL_RENDERBUFFER_GREEN_SIZE),
			std::nothrow
		);
	}

	/// Returns the size in bits of the renderbuffer's blue component
	/**
	 *  @see RedSize
	 *  @see GreenSize
	 *  @see AlphaSize
	 *  @see DepthSize
	 *  @see StencilSize
	 *
	 *  @glsymbols
	 *  @glfunref{GetRenderbufferParameter}
	 *  @gldefref{RENDERBUFFER_BLUE_SIZE}
	 */
	SizeType BlueSize(void) const
	{
		return MakeSizeType(
			GetIntParam(GL_RENDERBUFFER_BLUE_SIZE),
			std::nothrow
		);
	}

	/// Returns the size in bits of the renderbuffer's alpha component
	/**
	 *  @see RedSize
	 *  @see GreenSize
	 *  @see BlueSize
	 *  @see DepthSize
	 *  @see StencilSize
	 *
	 *  @glsymbols
	 *  @glfunref{GetRenderbufferParameter}
	 *  @gldefref{RENDERBUFFER_ALPHA_SIZE}
	 */
	SizeType AlphaSize(void) const
	{
		return MakeSizeType(
			GetIntParam(GL_RENDERBUFFER_ALPHA_SIZE),
			std::nothrow
		);
	}

	/// Returns the size in bits of the renderbuffer's depth component
	/**
	 *  @see RedSize
	 *  @see GreenSize
	 *  @see BlueSize
	 *  @see AlphaSize
	 *  @see StencilSize
	 *
	 *  @glsymbols
	 *  @glfunref{GetRenderbufferParameter}
	 *  @gldefref{RENDERBUFFER_DEPTH_SIZE}
	 */
	SizeType DepthSize(void) const
	{
		return MakeSizeType(
			GetIntParam(GL_RENDERBUFFER_DEPTH_SIZE),
			std::nothrow
		);
	}

	/// Returns the size in bits of the renderbuffer's stencil component
	/**
	 *  @see RedSize
	 *  @see GreenSize
	 *  @see BlueSize
	 *  @see AlphaSize
	 *  @see DepthSize
	 *
	 *  @glsymbols
	 *  @glfunref{GetRenderbufferParameter}
	 *  @gldefref{RENDERBUFFER_STENCIL_SIZE}
	 */
	SizeType StencilSize(void) const
	{
		return MakeSizeType(
			GetIntParam(GL_RENDERBUFFER_STENCIL_SIZE),
			std::nothrow
		);
	}

	/// Returns the number of samples of the renderbuffer
	/**
	 *  @glsymbols
	 *  @glfunref{GetRenderbufferParameter}
	 *  @gldefref{RENDERBUFFER_SAMPLES}
	 */
	SizeType Samples(void) const
	{
		return MakeSizeType(
			GetIntParam(GL_RENDERBUFFER_SAMPLES),
			std::nothrow
		);
	}

	/// Returns the internal format of the renderbuffer
	/**
	 *  @glsymbols
	 *  @glfunref{GetRenderbufferParameter}
	 *  @gldefref{RENDERBUFFER_INTERNAL_FORMAT}
	 */
	PixelDataInternalFormat InternalFormat(void) const
	{
		return PixelDataInternalFormat(
			GetIntParam(GL_RENDERBUFFER_INTERNAL_FORMAT)
		);
	}
};

/// Renderbuffer operations with direct state access
typedef ObjectOps<tag::DirectState, tag::Renderbuffer>
	DSARenderbufferOps;

// syntax-sugar operators

// Bind
inline DSARenderbufferOps& operator << (
	DSARenderbufferOps& rbo,
	RenderbufferTarget target
)
{
	rbo.Bind(target);
	return rbo;
}

// Storage
inline DSARenderbufferOps& operator << (
	DSARenderbufferOps& rbo,
	const images::ImageSpec& image_spec
)
{
	rbo.Storage(image_spec);
	return rbo;
}

/// An @ref oglplus_object encapsulating the OpenGL renderbuffer functionality
/**
 *  @ingroup oglplus_objects
 */
typedef Object<DSARenderbufferOps> DSARenderbuffer;

#endif // GL_ARB_direct_state_access

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/dsa/renderbuffer.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

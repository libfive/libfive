/**
 *  @file oglplus/dsa/framebuffer.hpp
 *  @brief Framebuffer object wrappers with direct state access
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_DSA_FRAMEBUFFER_1310090720_HPP
#define OGLPLUS_DSA_FRAMEBUFFER_1310090720_HPP

#include <oglplus/framebuffer.hpp>
#include <oglplus/color_buffer.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_5 || GL_ARB_direct_state_access

template <>
struct ObjGenTag<tag::DirectState, tag::Framebuffer>
{
	typedef tag::Create Type;
};

/// Class wrapping framebuffer-related functionality with direct state access
/** @note Do not use this class directly, use DSARenderbuffer instead.
 *
 */
template <>
class ObjectOps<tag::DirectState, tag::Framebuffer>
 : public ObjZeroOps<tag::DirectState, tag::Framebuffer>
{
protected:
	ObjectOps(FramebufferName name)
	OGLPLUS_NOEXCEPT(true)
	 : ObjZeroOps<tag::DirectState, tag::Framebuffer>(name)
	{ }
public:
#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	ObjectOps(ObjectOps&&) = default;
	ObjectOps(const ObjectOps&) = default;
	ObjectOps& operator = (ObjectOps&&) = default;
	ObjectOps& operator = (const ObjectOps&) = default;
#else
	typedef ObjZeroOps<tag::DirectState, tag::Framebuffer> _base;

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
	typedef FramebufferOps::Property Property;

	using ObjZeroOps<tag::DirectState, tag::Framebuffer>::Bind;
	void Bind(Target target)
	{
		ObjZeroOps<tag::DirectState, tag::Framebuffer>::Bind(target);
	}


	/// Checks the status of the framebuffer
	/** Returns one of the values in the @c FramebufferStatus enumeration.
	 *  For complete framebuffers this member function returns
	 *  Status::Complete.
	 *
	 *  @see IsComplete
	 *
	 *  @glsymbols
	 *  @glfunref{CheckFramebufferStatus}
	 */
	FramebufferStatus Status(Target target) const
	{
		GLenum result = OGLPLUS_GLFUNC(CheckNamedFramebufferStatus)(
			_obj_name(),
			GLenum(target)
		);
		if(result == 0) OGLPLUS_CHECK(
			CheckNamedFramebufferStatus,
			ObjectError,
			Object(*this).
			BindTarget(target)
		);
		return FramebufferStatus(result);
	}

	/// Returns true if the framebuffer is complete
	/**
	 *  @see FramebufferStatus
	 *  @see Status()
	 *
	 *  @glsymbols
	 *  @glfunref{CheckFramebufferStatus}
	 */
	bool IsComplete(Target target) const
	{
		return Status(target) == FramebufferStatus::Complete;
	}

	void HandleIncompleteError(FramebufferStatus status) const;

	/// Throws an exception if the framebuffer is not complete
	void Complete(Target target) const
	{
		FramebufferStatus status = Status(target);
		if(status != FramebufferStatus::Complete)
		{
			HandleIncompleteError(status);
		}
	}

	/// Attach a @p renderbuffer to the @p attachment point of this FBO
	/**
	 *  @see AttachColorRenderbuffer
	 *  @see AttachTexture
	 *  @see AttachTextureLayer
	 *  @see AttachTexture1D
	 *  @see AttachTexture2D
	 *  @see AttachTexture3D
	 *  @see AttachColorTexture
	 *
	 *  @glsymbols
	 *  @glfunref{FramebufferRenderbuffer}
	 */
	void AttachRenderbuffer(
		Property::Attachment attachment,
		RenderbufferName renderbuffer
	)
	{
		OGLPLUS_GLFUNC(NamedFramebufferRenderbuffer)(
			_obj_name(),
			GLenum(attachment),
			GL_RENDERBUFFER,
			GetGLName(renderbuffer)
		);
		OGLPLUS_CHECK(
			NamedFramebufferRenderbuffer,
			ObjectPairError,
			Subject(renderbuffer).
			Object(*this)
		);
	}

	/// Attach a @p renderbuffer to the color @p attachment_no of this FBO
	/**
	 *  @see AttachRenderbuffer
	 *  @see AttachTexture
	 *  @see AttachTextureLayer
	 *  @see AttachTexture1D
	 *  @see AttachTexture2D
	 *  @see AttachTexture3D
	 *  @see AttachColorTexture
	 *
	 *  @glsymbols
	 *  @glfunref{FramebufferRenderbuffer}
	 */
	void AttachColorRenderbuffer(
		FramebufferColorAttachmentNumber attachment_no,
		RenderbufferName renderbuffer
	)
	{
		OGLPLUS_GLFUNC(NamedFramebufferRenderbuffer)(
			_obj_name(),
			GL_COLOR_ATTACHMENT0 + GLuint(attachment_no),
			GL_RENDERBUFFER,
			GetGLName(renderbuffer)
		);
		OGLPLUS_CHECK(
			NamedFramebufferRenderbuffer,
			ObjectPairError,
			Subject(renderbuffer).
			Object(*this)
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_2
	/// Attach a @p texture to the @p attachment point of this FBO
	/**
	 *  @see AttachRenderbuffer
	 *  @see AttachColorRenderbuffer
	 *  @see AttachTextureLayer
	 *  @see AttachTexture1D
	 *  @see AttachTexture2D
	 *  @see AttachTexture3D
	 *  @see AttachColorTexture
	 *
	 *  @glsymbols
	 *  @glfunref{FramebufferTexture}
	 */
	void AttachTexture(
		Property::Attachment attachment,
		TextureName texture,
		GLint level
	)
	{
		OGLPLUS_GLFUNC(NamedFramebufferTexture)(
			_obj_name(),
			GLenum(attachment),
			GetGLName(texture),
			level
		);
		OGLPLUS_CHECK(
			NamedFramebufferTexture,
			ObjectPairError,
			Subject(texture).
			Object(*this).
			Index(level)
		);
	}

	/// Attach a @p texture to the color @p attachment point of this FBO
	/**
	 *  @see AttachRenderbuffer
	 *  @see AttachColorRenderbuffer
	 *  @see AttachTexture1D
	 *  @see AttachTexture2D
	 *  @see AttachTexture3D
	 *  @see AttachTexture
	 *  @see AttachTextureLayer
	 *
	 *  @glsymbols
	 *  @glfunref{FramebufferTexture}
	 */
	void AttachColorTexture(
		FramebufferColorAttachmentNumber attachment_no,
		TextureName texture,
		GLint level
	)
	{
		OGLPLUS_GLFUNC(NamedFramebufferTexture)(
			_obj_name(),
			GL_COLOR_ATTACHMENT0 + GLenum(attachment_no),
			GetGLName(texture),
			level
		);
		OGLPLUS_CHECK(
			NamedFramebufferTexture,
			ObjectPairError,
			Subject(texture).
			Object(*this).
			Index(level)
		);
	}
#endif

	/// Attach a @p texture layer to the @p attachment point of this FBO
	/**
	 *  @see AttachRenderbuffer
	 *  @see AttachColorRenderbuffer
	 *  @see AttachTexture1D
	 *  @see AttachTexture2D
	 *  @see AttachTexture3D
	 *  @see AttachColorTexture
	 *  @see AttachTexture
	 *
	 *  @glsymbols
	 *  @glfunref{FramebufferTextureLayer}
	 */
	void AttachTextureLayer(
		Property::Attachment attachment,
		TextureName texture,
		GLint level,
		GLint layer
	)
	{
		OGLPLUS_GLFUNC(NamedFramebufferTextureLayer)(
			_obj_name(),
			GLenum(attachment),
			GetGLName(texture),
			level,
			layer
		);
		OGLPLUS_CHECK(
			NamedFramebufferTextureLayer,
			ObjectPairError,
			Subject(texture).
			Object(*this).
			Index(level)
		);
	}

	/// Color buffer specification type
	typedef OneOf<
		GLenum,
		std::tuple<
			oglplus::ColorBuffer,
			oglplus::FramebufferColorAttachment
		>
	> ColorBuffer;

	/// Sets the destination color buffer for draw operations
	/**
	 *  @glsymbols
	 *  @glfunref{DrawBuffer}
	 */
	void DrawBuffer(ColorBuffer buffer)
	{
		OGLPLUS_GLFUNC(NamedFramebufferDrawBuffer)(
			_obj_name(),
			GLenum(buffer)
		);
		OGLPLUS_VERIFY(
			NamedFramebufferDrawBuffer,
			ObjectError,
			Object(*this)
		);
	}

	/// Sets the destination color buffers for draw operations
	/**
	 *  @glsymbols
	 *  @glfunref{DrawBuffers}
	 */
	template <unsigned N>
	void DrawBuffers(const EnumArray<ColorBuffer>& buffers)
	{
		OGLPLUS_GLFUNC(NamedFramebufferDrawBuffers)(
			_obj_name(),
			buffers.Count(),
			buffers.Values()
		);
		OGLPLUS_VERIFY(
			NamedFramebufferDrawBuffers,
			ObjectError,
			Object(*this)
		);
	}

	/// Sets the source color buffer for read operations
	/**
	 *  @glsymbols
	 *  @glfunref{ReadBuffer}
	 */
	void ReadBuffer(ColorBuffer buffer)
	{
		OGLPLUS_GLFUNC(NamedFramebufferReadBuffer)(
			_obj_name(),
			GLenum(buffer)
		);
		OGLPLUS_VERIFY(
			NamedFramebufferReadBuffer,
			ObjectError,
			Object(*this)
		);
	}
};

/// Framebuffer operations with direct state access
typedef ObjectOps<tag::DirectState, tag::Framebuffer>
	DSAFramebufferOps;

// Helper class for syntax-sugar operators
struct DSAFramebufferOpsAndAttch
{
	typedef DSAFramebufferOps::Property::Attachment Attachment;

	DSAFramebufferOps& fbo;
	Attachment attachment;

	DSAFramebufferOpsAndAttch(DSAFramebufferOps& f, Attachment a)
	 : fbo(f)
	 , attachment(a)
	{ }
};

inline DSAFramebufferOpsAndAttch operator << (
	DSAFramebufferOps& fbo,
	DSAFramebufferOps::Property::Attachment attch
)
{
	return DSAFramebufferOpsAndAttch(fbo, attch);
}

// Bind
inline DSAFramebufferOps& operator << (
	DSAFramebufferOps& fbo,
	FramebufferTarget target
)
{
	fbo.Bind(target);
	return fbo;
}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_2
// AttachTexture
inline DSAFramebufferOps& operator << (
	DSAFramebufferOpsAndAttch&& faa,
	TextureName tex
)
{
	faa.fbo.AttachTexture(faa.attachment, tex, 0);
	return faa.fbo;
}
#endif

// AttachRenderbuffer
inline DSAFramebufferOps& operator << (
	DSAFramebufferOpsAndAttch&& faa,
	RenderbufferName rbo
)
{
	faa.fbo.AttachRenderbuffer(faa.attachment, rbo);
	return faa.fbo;
}

/// An @ref oglplus_object encapsulating the OpenGL framebuffer functionality
/**
 *  @ingroup oglplus_objects
 */
typedef Object<DSAFramebufferOps> DSAFramebuffer;

#endif // GL_ARB_direct_state_access

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/dsa/framebuffer.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

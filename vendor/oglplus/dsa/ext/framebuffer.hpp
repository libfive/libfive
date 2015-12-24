/**
 *  @file oglplus/dsa/ext/framebuffer.hpp
 *  @brief Framebuffer object wrappers with direct state access
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_DSA_EXT_FRAMEBUFFER_1310090720_HPP
#define OGLPLUS_DSA_EXT_FRAMEBUFFER_1310090720_HPP

#include <oglplus/framebuffer.hpp>
#include <oglplus/color_buffer.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_EXT_direct_state_access

/// Class wrapping framebuffer-related functionality with direct state access
/** @note Do not use this class directly, use DSAFramebufferEXT instead.
 *
 */
template <>
class ObjectOps<tag::DirectStateEXT, tag::Framebuffer>
 : public ObjZeroOps<tag::DirectStateEXT, tag::Framebuffer>
{
protected:
	ObjectOps(FramebufferName name)
	OGLPLUS_NOEXCEPT(true)
	 : ObjZeroOps<tag::DirectStateEXT, tag::Framebuffer>(name)
	{ }
public:
#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	ObjectOps(ObjectOps&&) = default;
	ObjectOps(const ObjectOps&) = default;
	ObjectOps& operator = (ObjectOps&&) = default;
	ObjectOps& operator = (const ObjectOps&) = default;
#else
	typedef ObjZeroOps<tag::DirectStateEXT, tag::Framebuffer> _base;

	ObjectOps(ObjectOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	 : _base(static_cast<_base&&>(temp))
	 , target(temp.target)
	{ }

	ObjectOps(const ObjectOps& that)
	OGLPLUS_NOEXCEPT(true)
	 : _base(static_cast<const _base&>(that))
	 , target(that.target)
	{ }

	ObjectOps& operator = (ObjectOps&& temp)
	OGLPLUS_NOEXCEPT(true)
	{
		_base::operator = (static_cast<_base&&>(temp));
		target = temp.target;
		return *this;
	}

	ObjectOps& operator = (const ObjectOps& that)
	OGLPLUS_NOEXCEPT(true)
	{
		_base::operator = (static_cast<const _base&>(that));
		target = that.target;
		return *this;
	}
#endif
	/// Used as the default value for functions taking Target arguments
	Target target;

	/// Types related to Framebuffer
	typedef FramebufferOps::Property Property;

	using ObjZeroOps<tag::DirectStateEXT, tag::Framebuffer>::Bind;
	void Bind(void)
	{
		ObjZeroOps<tag::DirectStateEXT, tag::Framebuffer>::Bind(target);
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
	FramebufferStatus Status(Target fbo_target) const
	{
		GLenum result = OGLPLUS_GLFUNC(CheckNamedFramebufferStatusEXT)(
			_obj_name(),
			GLenum(fbo_target)
		);
		if(result == 0) OGLPLUS_CHECK(
			CheckNamedFramebufferStatusEXT,
			ObjectError,
			Object(*this).
			BindTarget(fbo_target)
		);
		return FramebufferStatus(result);
	}

	FramebufferStatus Status(void) const
	{
		return Status(target);
	}

	/// Returns true if the framebuffer is complete
	/**
	 *  @see FramebufferStatus
	 *  @see Status()
	 *
	 *  @glsymbols
	 *  @glfunref{CheckFramebufferStatus}
	 */
	bool IsComplete(Target fbo_target) const
	{
		return Status(fbo_target) == FramebufferStatus::Complete;
	}

	bool IsComplete(void) const
	{
		return IsComplete(target);
	}

	void HandleIncompleteError(FramebufferStatus status) const;

	/// Throws an exception if the framebuffer is not complete
	void Complete(Target fbo_target) const
	{
		FramebufferStatus status = Status(fbo_target);
		if(status != FramebufferStatus::Complete)
		{
			HandleIncompleteError(status);
		}
	}

	void Complete(void) const
	{
		Complete(target);
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
		OGLPLUS_GLFUNC(NamedFramebufferRenderbufferEXT)(
			_obj_name(),
			GLenum(attachment),
			GL_RENDERBUFFER,
			GetGLName(renderbuffer)
		);
		OGLPLUS_CHECK(
			NamedFramebufferRenderbufferEXT,
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
		OGLPLUS_GLFUNC(NamedFramebufferRenderbufferEXT)(
			_obj_name(),
			GL_COLOR_ATTACHMENT0 + GLuint(attachment_no),
			GL_RENDERBUFFER,
			GetGLName(renderbuffer)
		);
		OGLPLUS_CHECK(
			NamedFramebufferRenderbufferEXT,
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
		OGLPLUS_GLFUNC(NamedFramebufferTextureEXT)(
			_obj_name(),
			GLenum(attachment),
			GetGLName(texture),
			level
		);
		OGLPLUS_CHECK(
			NamedFramebufferTextureEXT,
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
		OGLPLUS_GLFUNC(NamedFramebufferTextureEXT)(
			_obj_name(),
			GL_COLOR_ATTACHMENT0 + GLenum(attachment_no),
			GetGLName(texture),
			level
		);
		OGLPLUS_CHECK(
			NamedFramebufferTextureEXT,
			ObjectPairError,
			Subject(texture).
			Object(*this).
			Index(level)
		);
	}
#endif

	/// Attach a 1D @p texture to the @p attachment point of this FBO
	/**
	 *  @see AttachRenderbuffer
	 *  @see AttachColorRenderbuffer
	 *  @see AttachTexture2D
	 *  @see AttachTexture3D
	 *  @see AttachColorTexture
	 *  @see AttachTexture
	 *  @see AttachTextureLayer
	 *
	 *  @glsymbols
	 *  @glfunref{FramebufferTexture1D}
	 */
	void AttachTexture1D(
		Property::Attachment attachment,
		TextureTarget textarget,
		TextureName texture,
		GLint level
	)
	{
		OGLPLUS_GLFUNC(NamedFramebufferTexture1DEXT)(
			_obj_name(),
			GLenum(attachment),
			GLenum(textarget),
			GetGLName(texture),
			level
		);
		OGLPLUS_CHECK(
			NamedFramebufferTexture1DEXT,
			ObjectPairError,
			Subject(texture).
			Object(*this).
			Index(level)
		);
	}

	/// Attach a 2D @p texture to the @p attachment point of this FBO
	/**
	 *  @see AttachRenderbuffer
	 *  @see AttachColorRenderbuffer
	 *  @see AttachTexture1D
	 *  @see AttachTexture3D
	 *  @see AttachColorTexture
	 *  @see AttachTexture
	 *  @see AttachTextureLayer
	 *
	 *  @glsymbols
	 *  @glfunref{FramebufferTexture2D}
	 */
	void AttachTexture2D(
		Property::Attachment attachment,
		TextureTarget textarget,
		TextureName texture,
		GLint level
	)
	{
		OGLPLUS_GLFUNC(NamedFramebufferTexture2DEXT)(
			_obj_name(),
			GLenum(attachment),
			GLenum(textarget),
			GetGLName(texture),
			level
		);
		OGLPLUS_CHECK(
			NamedFramebufferTexture2DEXT,
			ObjectPairError,
			Subject(texture).
			Object(*this).
			Index(level)
		);
	}

	/// Attach a 3D @p texture to the @p attachment point of this FBO
	/**
	 *  @see AttachRenderbuffer
	 *  @see AttachColorRenderbuffer
	 *  @see AttachTexture1D
	 *  @see AttachTexture2D
	 *  @see AttachColorTexture
	 *  @see AttachTexture
	 *  @see AttachTextureLayer
	 *
	 *  @glsymbols
	 *  @glfunref{FramebufferTexture3D}
	 */
	void AttachTexture3D(
		Property::Attachment attachment,
		TextureTarget textarget,
		TextureName texture,
		GLint level,
		GLint layer
	)
	{
		OGLPLUS_GLFUNC(NamedFramebufferTexture3DEXT)(
			_obj_name(),
			GLenum(attachment),
			GLenum(textarget),
			GetGLName(texture),
			level,
			layer
		);
		OGLPLUS_CHECK(
			NamedFramebufferTexture3DEXT,
			ObjectPairError,
			Subject(texture).
			Object(*this).
			Index(level)
		);
	}

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
		OGLPLUS_GLFUNC(NamedFramebufferTextureLayerEXT)(
			_obj_name(),
			GLenum(attachment),
			GetGLName(texture),
			level,
			layer
		);
		OGLPLUS_CHECK(
			NamedFramebufferTextureLayerEXT,
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
		OGLPLUS_GLFUNC(FramebufferDrawBufferEXT)(
			_obj_name(),
			GLenum(buffer)
		);
		OGLPLUS_VERIFY(
			FramebufferDrawBufferEXT,
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
		OGLPLUS_GLFUNC(FramebufferDrawBuffersEXT)(
			_obj_name(),
			GLint(buffers.Count()),
			buffers.Values()
		);
		OGLPLUS_VERIFY(
			FramebufferDrawBuffersEXT,
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
		OGLPLUS_GLFUNC(FramebufferReadBufferEXT)(
			_obj_name(),
			GLenum(buffer)
		);
		OGLPLUS_VERIFY(
			FramebufferReadBufferEXT,
			ObjectError,
			Object(*this)
		);
	}
};

/// Framebuffer operations with direct state access
typedef ObjectOps<tag::DirectStateEXT, tag::Framebuffer>
	DSAFramebufferOpsEXT;

// Helper class for syntax-sugar operators
struct DSAFramebufferOpsAndAttchEXT
{
	typedef DSAFramebufferOpsEXT::Property::Attachment Attachment;

	DSAFramebufferOpsEXT& fbo;
	Attachment attachment;

	DSAFramebufferOpsAndAttchEXT(DSAFramebufferOpsEXT& f, Attachment a)
	 : fbo(f)
	 , attachment(a)
	{ }
};

inline DSAFramebufferOpsAndAttchEXT operator << (
	DSAFramebufferOpsEXT& fbo,
	DSAFramebufferOpsEXT::Property::Attachment attch
)
{
	return DSAFramebufferOpsAndAttchEXT(fbo, attch);
}

// Bind
inline DSAFramebufferOpsEXT& operator << (
	DSAFramebufferOpsEXT& fbo,
	FramebufferTarget target
)
{
	fbo.target = target;
	fbo.Bind();
	return fbo;
}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_2
// AttachTexture
inline DSAFramebufferOpsEXT& operator << (
	DSAFramebufferOpsAndAttchEXT&& faa,
	TextureName tex
)
{
	faa.fbo.AttachTexture(faa.attachment, tex, 0);
	return faa.fbo;
}
#endif

// AttachRenderbuffer
inline DSAFramebufferOpsEXT& operator << (
	DSAFramebufferOpsAndAttchEXT&& faa,
	RenderbufferName rbo
)
{
	faa.fbo.AttachRenderbuffer(faa.attachment, rbo);
	return faa.fbo;
}

// Complete
inline DSAFramebufferOpsEXT& operator << (
	DSAFramebufferOpsEXT& fbo,
	FramebufferComplete
)
{
	fbo.Complete();
	return fbo;
}

/// An @ref oglplus_object encapsulating the OpenGL framebuffer functionality
/**
 *  @ingroup oglplus_objects
 */
typedef Object<DSAFramebufferOpsEXT> DSAFramebufferEXT;

#else
#error Direct State Access Framebuffers not available
#endif // GL_EXT_direct_state_access

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/dsa/ext/framebuffer.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

/**
 *  @file oglplus/framebuffer.hpp
 *  @brief Framebuffer object wrappers
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_FRAMEBUFFER_1107121519_HPP
#define OGLPLUS_FRAMEBUFFER_1107121519_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/error/framebuffer.hpp>
#include <oglplus/framebuffer_target.hpp>
#include <oglplus/framebuffer_attachment.hpp>
#include <oglplus/framebuffer_status.hpp>
#include <oglplus/texture_target.hpp>
#include <oglplus/one_of.hpp>
#include <oglplus/object/wrapper.hpp>
#include <cassert>

namespace oglplus {

// NOTE: Xlib.h defines this symbol
// using the preprocessor.
#ifdef Status
#undef Status
#endif

/// Class wrapping framebuffer construction/destruction functions
/** @note Do not use this class directly, use Framebuffer instead.
 *
 *  @glsymbols
 *  @glfunref{GenFramebuffers}
 *  @glfunref{DeleteFramebuffers}
 *  @glfunref{IsFramebuffer}
 */
template <>
class ObjGenDelOps<tag::Framebuffer>
{
protected:
	static void Gen(tag::Generate, GLsizei count, GLuint* names)
	{
		assert(names != nullptr);
		OGLPLUS_GLFUNC(GenFramebuffers)(count, names);
		OGLPLUS_CHECK_SIMPLE(GenFramebuffers);
	}
#if GL_VERSION_4_5 || GL_ARB_direct_state_access
	static void Gen(tag::Create, GLsizei count, GLuint* names)
	{
		assert(names != nullptr);
		OGLPLUS_GLFUNC(CreateFramebuffers)(count, names);
		OGLPLUS_CHECK_SIMPLE(CreateFramebuffers);
	}
#endif

	static void Delete(GLsizei count, GLuint* names)
	{
		assert(names != nullptr);
		OGLPLUS_GLFUNC(DeleteFramebuffers)(count, names);
		OGLPLUS_VERIFY_SIMPLE(DeleteFramebuffers);
	}

	static Boolean IsA(GLuint name)
	{
		Boolean result(
			OGLPLUS_GLFUNC(IsFramebuffer)(name),
			std::nothrow
		);
		OGLPLUS_VERIFY_SIMPLE(IsFramebuffer);
		return result;
	}
};

/// Framebuffer binding operations
template <>
class ObjBindingOps<tag::Framebuffer>
{
private:
	static GLenum _binding_query(FramebufferTarget target);
protected:
	static GLuint _binding(FramebufferTarget target);
public:
	/// Framebuffer bind targets
	typedef FramebufferTarget Target;

	/// Returns the current Framebuffer bound to specified @p target
	/**
	 *  @glsymbols
	 *  @glfunref{GetIntegerv}
	 */
	static FramebufferName Binding(Target target)
	{
		return FramebufferName(_binding(target));
	}

	/// Binds the specified @p framebuffer to the specified @p target
	/**
	 *  @glsymbols
	 *  @glfunref{BindFramebuffer}
	 */
	static void Bind(
		Target target,
		FramebufferName framebuffer
	)
	{
		OGLPLUS_GLFUNC(BindFramebuffer)(
			GLenum(target),
			GetGLName(framebuffer)
		);
		OGLPLUS_VERIFY(
			BindFramebuffer,
			ObjectError,
			ObjectBinding(target)
		);
	}
};

/// Common framebuffer operations
/** @note Do not use this class directly, use Framebuffer
 *  or DefaultFramebuffer instead.
 */
template <>
class ObjCommonOps<tag::Framebuffer>
 : public FramebufferName
 , public ObjBindingOps<tag::Framebuffer>
{
protected:
	ObjCommonOps(FramebufferName name)
	OGLPLUS_NOEXCEPT(true)
	 : FramebufferName(name)
	{ }
public:
#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	ObjCommonOps(ObjCommonOps&&) = default;
	ObjCommonOps(const ObjCommonOps&) = default;
	ObjCommonOps& operator = (ObjCommonOps&&) = default;
	ObjCommonOps& operator = (const ObjCommonOps&) = default;
#else
	typedef FramebufferName _base1;
	typedef ObjBindingOps<tag::Framebuffer> _base2;

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
	using ObjBindingOps<tag::Framebuffer>::Bind;

	/// Binds this framebuffer to the specified @p target
	/**
	 *  @glsymbols
	 *  @glfunref{BindFramebuffer}
	 */
	void Bind(Target target) const
	{
		Bind(target, *this);
	}
};

/// Class wrapping framebuffer functions with explicit target selector
/** @note Do not use this class directly, use Framebuffer instead.
 */
template <>
class ObjectOps<tag::ExplicitSel, tag::Framebuffer>
 : public ObjZeroOps<tag::ExplicitSel, tag::Framebuffer>
{
protected:
	ObjectOps(FramebufferName name)
	OGLPLUS_NOEXCEPT(true)
	 : ObjZeroOps<tag::ExplicitSel, tag::Framebuffer>(name)
	{ }
public:
#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	ObjectOps(ObjectOps&&) = default;
	ObjectOps(const ObjectOps&) = default;
	ObjectOps& operator = (ObjectOps&&) = default;
	ObjectOps& operator = (const ObjectOps&) = default;
#else
	typedef ObjZeroOps<tag::ExplicitSel, tag::Framebuffer> _base;

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
	/// Types related to Framebuffer
	struct Property
	{
		/// Buffer of default FB or attachment of a FBO
		typedef OneOf<
			GLenum,
			std::tuple<
				FramebufferBuffer,
				FramebufferAttachment,
				FramebufferColorAttachment
			>
		> Buffer;

		/// Attachment of a Framebuffer
		typedef OneOf<
			GLenum,
			std::tuple<
				FramebufferAttachment,
				FramebufferColorAttachment
			>
		> Attachment;

		/// Status of a Framebuffer
		typedef FramebufferStatus Status;
	};

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
	static FramebufferStatus Status(Target target)
	{
		GLenum result = OGLPLUS_GLFUNC(CheckFramebufferStatus)(
			GLenum(target)
		);
		if(result == 0) OGLPLUS_CHECK(
			CheckFramebufferStatus,
			ObjectError,
			ObjectBinding(target)
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
	static bool IsComplete(Target target)
	{
		return Status(target) == FramebufferStatus::Complete;
	}

	static void HandleIncompleteError(Target target, FramebufferStatus status);

	/// Throws an exception if the framebuffer is not complete
	static void Complete(Target target)
	{
		FramebufferStatus status = Status(target);
		if(status != FramebufferStatus::Complete)
		{
			HandleIncompleteError(target, status);
		}
	}

	/// Attach a @p renderbuffer to the @p attachment point of @p target
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
	static void AttachRenderbuffer(
		Target target,
		Property::Attachment attachment,
		RenderbufferName renderbuffer
	)
	{
		OGLPLUS_GLFUNC(FramebufferRenderbuffer)(
			GLenum(target),
			GLenum(attachment),
			GL_RENDERBUFFER,
			GetGLName(renderbuffer)
		);
		OGLPLUS_CHECK(
			FramebufferRenderbuffer,
			ObjectPairError,
			Subject(renderbuffer).
			ObjectBinding(target)
		);
	}

	/// Attach a @p renderbuffer to the color @p attachment_no of @p target
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
	static void AttachColorRenderbuffer(
		Target target,
		FramebufferColorAttachmentNumber attachment_no,
		RenderbufferName renderbuffer
	)
	{
		OGLPLUS_GLFUNC(FramebufferRenderbuffer)(
			GLenum(target),
			GL_COLOR_ATTACHMENT0 + GLuint(attachment_no),
			GL_RENDERBUFFER,
			GetGLName(renderbuffer)
		);
		OGLPLUS_CHECK(
			FramebufferRenderbuffer,
			ObjectPairError,
			Subject(renderbuffer).
			ObjectBinding(target)
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_2
	/// Attach a @p texture to the @p attachment point of @p target
	/**
	 *  @see AttachRenderbuffer
	 *  @see AttachColorRenderbuffer
	 *  @see AttachTextureLayer
	 *  @see AttachTexture1D
	 *  @see AttachTexture2D
	 *  @see AttachTexture3D
	 *  @see AttachColorTexture
	 *
	 *  @glverreq{3,2}
	 *  @glsymbols
	 *  @glfunref{FramebufferTexture}
	 */
	static void AttachTexture(
		Target target,
		Property::Attachment attachment,
		TextureName texture,
		GLint level
	)
	{
		OGLPLUS_GLFUNC(FramebufferTexture)(
			GLenum(target),
			GLenum(attachment),
			GetGLName(texture),
			level
		);
		OGLPLUS_CHECK(
			FramebufferTexture,
			ObjectPairError,
			Subject(texture).
			ObjectBinding(target).
			Index(level)
		);
	}

	/// Attach a @p texture to the color @p attachment point of @p target
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
	static void AttachColorTexture(
		Target target,
		FramebufferColorAttachmentNumber attachment_no,
		TextureName texture,
		GLint level
	)
	{
		OGLPLUS_GLFUNC(FramebufferTexture)(
			GLenum(target),
			GL_COLOR_ATTACHMENT0 + GLenum(attachment_no),
			GetGLName(texture),
			level
		);
		OGLPLUS_CHECK(
			FramebufferTexture,
			ObjectPairError,
			Subject(texture).
			ObjectBinding(target).
			Index(level)
		);
	}
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Attach a 1D @p texture to the @p attachment point of @p target
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
	static void AttachTexture1D(
		Target target,
		Property::Attachment attachment,
		TextureTarget textarget,
		TextureName texture,
		GLint level
	)
	{
		OGLPLUS_GLFUNC(FramebufferTexture1D)(
			GLenum(target),
			GLenum(attachment),
			GLenum(textarget),
			GetGLName(texture),
			level
		);
		OGLPLUS_CHECK(
			FramebufferTexture1D,
			ObjectPairError,
			Subject(texture).
			ObjectBinding(target).
			Index(level)
		);
	}
#endif

	/// Attach a 2D @p texture to the @p attachment point of @p target
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
	static void AttachTexture2D(
		Target target,
		Property::Attachment attachment,
		TextureTarget textarget,
		TextureName texture,
		GLint level
	)
	{
		OGLPLUS_GLFUNC(FramebufferTexture2D)(
			GLenum(target),
			GLenum(attachment),
			GLenum(textarget),
			GetGLName(texture),
			level
		);
		OGLPLUS_CHECK(
			FramebufferTexture2D,
			ObjectPairError,
			Subject(texture).
			ObjectBinding(target).
			Index(level)
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
	/// Attach a 3D @p texture to the @p attachment point of @p target
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
	static void AttachTexture3D(
		Target target,
		Property::Attachment attachment,
		TextureTarget textarget,
		TextureName texture,
		GLint level,
		GLint layer
	)
	{
		OGLPLUS_GLFUNC(FramebufferTexture3D)(
			GLenum(target),
			GLenum(attachment),
			GLenum(textarget),
			GetGLName(texture),
			level,
			layer
		);
		OGLPLUS_CHECK(
			FramebufferTexture3D,
			ObjectPairError,
			Subject(texture).
			ObjectBinding(target).
			Index(level)
		);
	}
#endif

	/// Attach a @p texture layer to the @p attachment point of @p target
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
	static void AttachTextureLayer(
		Target target,
		Property::Attachment attachment,
		TextureName texture,
		GLint level,
		GLint layer
	)
	{
		OGLPLUS_GLFUNC(FramebufferTextureLayer)(
			GLenum(target),
			GLenum(attachment),
			GetGLName(texture),
			level,
			layer
		);
		OGLPLUS_CHECK(
			FramebufferTextureLayer,
			ObjectPairError,
			Subject(texture).
			ObjectBinding(target).
			Index(level)
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3 || GL_ARB_invalidate_subdata
	/// Invalidates the specified attachments or buffers of the Framebuffer
	/**
	 *  @glvoereq{4,3,ARB,invalidate_subdata}
	 *  @glsymbols
	 *  @glfunref{InvalidateFramebuffer}
	 */
	static void Invalidate(
		Target target,
		const EnumArray<Property::Buffer>& buffers
	)
	{
		OGLPLUS_GLFUNC(InvalidateFramebuffer)(
			GLenum(target),
			GLsizei(buffers.Count()),
			buffers.Values()
		);
		OGLPLUS_CHECK(
			InvalidateFramebuffer,
			ObjectError,
			ObjectBinding(target)
		);
	}

	/// Invalidates the specified attachments or buffers of the Framebuffer
	/**
	 *  @glvoereq{4,3,ARB,invalidate_subdata}
	 *  @glsymbols
	 *  @glfunref{InvalidateFramebuffer}
	 */
	static void Invalidate(
		Target target,
		SizeType count,
		const Property::Buffer* buffers
	)
	{
		Invalidate(
			target,
			EnumArray<Property::Buffer>(
				count,
				buffers
			)
		);
	}

	/// Invalidates parts of attachments or buffers of the Framebuffer
	/**
	 *  @glvoereq{4,3,ARB,invalidate_subdata}
	 *  @glsymbols
	 *  @glfunref{InvalidateSubFramebuffer}
	 */
	static void Invalidate(
		Target target,
		const EnumArray<Property::Buffer>& buffers,
		GLint x,
		GLint y,
		SizeType width,
		SizeType height
	)
	{
		OGLPLUS_GLFUNC(InvalidateSubFramebuffer)(
			GLenum(target),
			GLsizei(buffers.Count()),
			buffers.Values(),
			x,
			y,
			width,
			height
		);
		OGLPLUS_CHECK(
			InvalidateSubFramebuffer,
			ObjectError,
			ObjectBinding(target)
		);
	}

	/// Invalidates parts of attachments or buffers of the Framebuffer
	/**
	 *  @glvoereq{4,3,ARB,invalidate_subdata}
	 *  @glsymbols
	 *  @glfunref{InvalidateSubFramebuffer}
	 */
	static void Invalidate(
		Target target,
		SizeType count,
		const Property::Buffer* buffers,
		GLint x,
		GLint y,
		SizeType width,
		SizeType height
	)
	{
		Invalidate(
			target,
			EnumArray<Property::Buffer>(
				count,
				buffers
			),
			x, y, width, height
		);
	}
#endif
};

/// Framebuffer operations with explicit selector
typedef ObjectOps<tag::ExplicitSel, tag::Framebuffer>
	FramebufferOps;

/// Helper class used with syntax-sugar operators
struct FramebufferComplete { };

// Helper class for syntax-sugar operators
struct FramebufferTargetAndAttch
{
	FramebufferTarget target;

	typedef FramebufferOps::Property::Attachment Attachment;
	Attachment attachment;

	FramebufferTargetAndAttch(FramebufferTarget& t, Attachment a)
	 : target(t)
	 , attachment(a)
	{ }
};

// syntax sugar operators
inline FramebufferTargetAndAttch operator | (
	FramebufferTarget target,
	FramebufferOps::Property::Attachment attachment
)
{
	return FramebufferTargetAndAttch(target, attachment);
}

inline FramebufferTargetAndAttch operator << (
	FramebufferTarget target,
	FramebufferOps::Property::Attachment attachment
)
{
	return FramebufferTargetAndAttch(target, attachment);
}

// Bind
inline FramebufferTarget operator << (
	const FramebufferOps& fbo,
	FramebufferTarget target
)
{
	fbo.Bind(target);
	return target;
}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_2
// AttachTexture
inline FramebufferTarget operator << (
	FramebufferTargetAndAttch taa,
	TextureName tex
)
{
	FramebufferOps::AttachTexture(
		taa.target,
		taa.attachment,
		tex,
		0
	);
	return taa.target;
}
#endif

// AttachRenderbuffer
inline FramebufferTarget operator << (
	FramebufferTargetAndAttch taa,
	RenderbufferName rbo
)
{
	FramebufferOps::AttachRenderbuffer(
		taa.target,
		taa.attachment,
		rbo
	);
	return taa.target;
}

// Complete
inline FramebufferTarget operator << (
	FramebufferTarget target,
	FramebufferComplete
)
{
	FramebufferOps::Complete(target);
	return target;
}

/// An @ref oglplus_object encapsulating the default framebuffer functionality
/**
 *  @ingroup oglplus_objects
 */
typedef ObjectZero<ObjZeroOps<tag::ExplicitSel, tag::Framebuffer>>
	DefaultFramebuffer;

inline FramebufferTarget operator << (
	DefaultFramebuffer dfb,
	FramebufferTarget target
)
{
	dfb.Bind(target);
	return target;
}

/// An @ref oglplus_object encapsulating the framebuffer object functionality
/**
 *  @ingroup oglplus_objects
 */
typedef Object<FramebufferOps> Framebuffer;

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/framebuffer.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

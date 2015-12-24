
/**
 *  @file oglplus/bound/framebuffer.hpp
 *  @brief Specialization of ObjectOps for Framebuffer.
 *
 *  Automatically generated file, do not edit manually!
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */


#ifndef OGLPLUS_BOUND_FRAMEBUFFER_1107121519_HPP
#define OGLPLUS_BOUND_FRAMEBUFFER_1107121519_HPP

#include <oglplus/object/bound.hpp>
#include <oglplus/framebuffer.hpp>
#include <utility>

namespace oglplus {

/// Specialization of the BoundObjOps for Framebuffer  >.
/** This template implements wrappers around the member functions
 *  of Framebuffer, which have
 *  a FramebufferTarget parameter
 *  specifying the binding point on which they should operate.
 *
 *  @note Do not use this template class directly use
 *  Bound < Framebuffer > or the Context::Current()
 *  function instead.
 *
 *  @ingroup utility_classes
 */
template <>
class BoundObjOps<tag::Framebuffer>
{
private:
	typedef ObjectOps<tag::ExplicitSel, tag::Framebuffer> ExplicitOps;
public:
	typedef ExplicitOps::Target Target;
	Target target;

	BoundObjOps(void)
	{ }

	BoundObjOps(Target init_tgt)
	 : target(init_tgt)
	{ }

	/** Wrapper for Framebuffer::Status()
	 *  @see Framebuffer::Status()
	 */
	FramebufferStatus Status(void) const
	{
		return ExplicitOps::Status(
			this->target
		);
	}


	/** Wrapper for Framebuffer::IsComplete()
	 *  @see Framebuffer::IsComplete()
	 */
	bool IsComplete(void) const
	{
		return ExplicitOps::IsComplete(
			this->target
		);
	}


	/** Wrapper for Framebuffer::HandleIncompleteError()
	 *  @see Framebuffer::HandleIncompleteError()
	 */
	const BoundObjOps& HandleIncompleteError(
		FramebufferStatus status
	) const
	{
		ExplicitOps::HandleIncompleteError(
			this->target,
			status
		);
		return *this;
	}


	/** Wrapper for Framebuffer::Complete()
	 *  @see Framebuffer::Complete()
	 */
	const BoundObjOps& Complete(void) const
	{
		ExplicitOps::Complete(
			this->target
		);
		return *this;
	}


	/** Wrapper for Framebuffer::AttachRenderbuffer()
	 *  @see Framebuffer::AttachRenderbuffer()
	 */
	const BoundObjOps& AttachRenderbuffer(
		ExplicitOps::Property::Attachment attachment,
		RenderbufferName renderbuffer
	) const
	{
		ExplicitOps::AttachRenderbuffer(
			this->target,
			attachment,
			renderbuffer
		);
		return *this;
	}


	/** Wrapper for Framebuffer::AttachColorRenderbuffer()
	 *  @see Framebuffer::AttachColorRenderbuffer()
	 */
	const BoundObjOps& AttachColorRenderbuffer(
		FramebufferColorAttachmentNumber attachment_no,
		RenderbufferName renderbuffer
	) const
	{
		ExplicitOps::AttachColorRenderbuffer(
			this->target,
			attachment_no,
			renderbuffer
		);
		return *this;
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_2

	/** Wrapper for Framebuffer::AttachTexture()
	 *  @see Framebuffer::AttachTexture()
	 */
	const BoundObjOps& AttachTexture(
		ExplicitOps::Property::Attachment attachment,
		TextureName texture,
		GLint level
	) const
	{
		ExplicitOps::AttachTexture(
			this->target,
			attachment,
			texture,
			level
		);
		return *this;
	}
#endif // GL_VERSION_3_2


	/** Wrapper for Framebuffer::AttachColorTexture()
	 *  @see Framebuffer::AttachColorTexture()
	 */
	const BoundObjOps& AttachColorTexture(
		FramebufferColorAttachmentNumber attachment_no,
		TextureName texture,
		GLint level
	) const
	{
		ExplicitOps::AttachColorTexture(
			this->target,
			attachment_no,
			texture,
			level
		);
		return *this;
	}


	/** Wrapper for Framebuffer::AttachTexture1D()
	 *  @see Framebuffer::AttachTexture1D()
	 */
	const BoundObjOps& AttachTexture1D(
		ExplicitOps::Property::Attachment attachment,
		TextureTarget textarget,
		TextureName texture,
		GLint level
	) const
	{
		ExplicitOps::AttachTexture1D(
			this->target,
			attachment,
			textarget,
			texture,
			level
		);
		return *this;
	}


	/** Wrapper for Framebuffer::AttachTexture2D()
	 *  @see Framebuffer::AttachTexture2D()
	 */
	const BoundObjOps& AttachTexture2D(
		ExplicitOps::Property::Attachment attachment,
		TextureTarget textarget,
		TextureName texture,
		GLint level
	) const
	{
		ExplicitOps::AttachTexture2D(
			this->target,
			attachment,
			textarget,
			texture,
			level
		);
		return *this;
	}


	/** Wrapper for Framebuffer::AttachTexture3D()
	 *  @see Framebuffer::AttachTexture3D()
	 */
	const BoundObjOps& AttachTexture3D(
		ExplicitOps::Property::Attachment attachment,
		TextureTarget textarget,
		TextureName texture,
		GLint level,
		GLint layer
	) const
	{
		ExplicitOps::AttachTexture3D(
			this->target,
			attachment,
			textarget,
			texture,
			level,
			layer
		);
		return *this;
	}


	/** Wrapper for Framebuffer::AttachTextureLayer()
	 *  @see Framebuffer::AttachTextureLayer()
	 */
	const BoundObjOps& AttachTextureLayer(
		ExplicitOps::Property::Attachment attachment,
		TextureName texture,
		GLint level,
		GLint layer
	) const
	{
		ExplicitOps::AttachTextureLayer(
			this->target,
			attachment,
			texture,
			level,
			layer
		);
		return *this;
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3 || GL_ARB_invalidate_subdata

	/** Wrapper for Framebuffer::Invalidate()
	 *  @see Framebuffer::Invalidate()
	 */
	const BoundObjOps& Invalidate(
		const EnumArray< ExplicitOps::Property::Buffer > & buffers
	) const
	{
		ExplicitOps::Invalidate(
			this->target,
			buffers
		);
		return *this;
	}
#endif // GL_VERSION_4_3 GL_ARB_invalidate_subdata

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3 || GL_ARB_invalidate_subdata

	/** Wrapper for Framebuffer::Invalidate()
	 *  @see Framebuffer::Invalidate()
	 */
	const BoundObjOps& Invalidate(
		SizeType count,
		const ExplicitOps::Property::Buffer * buffers
	) const
	{
		ExplicitOps::Invalidate(
			this->target,
			count,
			buffers
		);
		return *this;
	}
#endif // GL_VERSION_4_3 GL_ARB_invalidate_subdata

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3 || GL_ARB_invalidate_subdata

	/** Wrapper for Framebuffer::Invalidate()
	 *  @see Framebuffer::Invalidate()
	 */
	const BoundObjOps& Invalidate(
		const EnumArray< ExplicitOps::Property::Buffer > & buffers,
		GLint x,
		GLint y,
		SizeType width,
		SizeType height
	) const
	{
		ExplicitOps::Invalidate(
			this->target,
			buffers,
			x,
			y,
			width,
			height
		);
		return *this;
	}
#endif // GL_VERSION_4_3 GL_ARB_invalidate_subdata

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3 || GL_ARB_invalidate_subdata

	/** Wrapper for Framebuffer::Invalidate()
	 *  @see Framebuffer::Invalidate()
	 */
	const BoundObjOps& Invalidate(
		SizeType count,
		const ExplicitOps::Property::Buffer * buffers,
		GLint x,
		GLint y,
		SizeType width,
		SizeType height
	) const
	{
		ExplicitOps::Invalidate(
			this->target,
			count,
			buffers,
			x,
			y,
			width,
			height
		);
		return *this;
	}
#endif // GL_VERSION_4_3 GL_ARB_invalidate_subdata


}; // class BoundObjOps

} // namespace oglplus

#endif // include guard

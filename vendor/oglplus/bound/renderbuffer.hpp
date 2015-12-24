
/**
 *  @file oglplus/bound/renderbuffer.hpp
 *  @brief Specialization of ObjectOps for Renderbuffer.
 *
 *  Automatically generated file, do not edit manually!
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */


#ifndef OGLPLUS_BOUND_RENDERBUFFER_1107121519_HPP
#define OGLPLUS_BOUND_RENDERBUFFER_1107121519_HPP

#include <oglplus/object/bound.hpp>
#include <oglplus/renderbuffer.hpp>
#include <utility>

namespace oglplus {

/// Specialization of the BoundObjOps for Renderbuffer  >.
/** This template implements wrappers around the member functions
 *  of Renderbuffer, which have
 *  a RenderbufferTarget parameter
 *  specifying the binding point on which they should operate.
 *
 *  @note Do not use this template class directly use
 *  Bound < Renderbuffer > or the Context::Current()
 *  function instead.
 *
 *  @ingroup utility_classes
 */
template <>
class BoundObjOps<tag::Renderbuffer>
{
private:
	typedef ObjectOps<tag::ExplicitSel, tag::Renderbuffer> ExplicitOps;
public:
	typedef ExplicitOps::Target Target;
	Target target;

	BoundObjOps(void)
	{ }

	BoundObjOps(Target init_tgt)
	 : target(init_tgt)
	{ }

	/** Wrapper for Renderbuffer::GetIntParam()
	 *  @see Renderbuffer::GetIntParam()
	 */
	GLint GetIntParam(
		GLenum query
	) const
	{
		return ExplicitOps::GetIntParam(
			this->target,
			query
		);
	}


	/** Wrapper for Renderbuffer::Storage()
	 *  @see Renderbuffer::Storage()
	 */
	const BoundObjOps& Storage(
		PixelDataInternalFormat internalformat,
		SizeType width,
		SizeType height
	) const
	{
		ExplicitOps::Storage(
			this->target,
			internalformat,
			width,
			height
		);
		return *this;
	}


	/** Wrapper for Renderbuffer::Storage()
	 *  @see Renderbuffer::Storage()
	 */
	const BoundObjOps& Storage(
		const images::ImageSpec & image_spec
	) const
	{
		ExplicitOps::Storage(
			this->target,
			image_spec
		);
		return *this;
	}


	/** Wrapper for Renderbuffer::StorageMultisample()
	 *  @see Renderbuffer::StorageMultisample()
	 */
	const BoundObjOps& StorageMultisample(
		SizeType samples,
		PixelDataInternalFormat internalformat,
		SizeType width,
		SizeType height
	) const
	{
		ExplicitOps::StorageMultisample(
			this->target,
			samples,
			internalformat,
			width,
			height
		);
		return *this;
	}


	/** Wrapper for Renderbuffer::Width()
	 *  @see Renderbuffer::Width()
	 */
	SizeType Width(void) const
	{
		return ExplicitOps::Width(
			this->target
		);
	}


	/** Wrapper for Renderbuffer::Height()
	 *  @see Renderbuffer::Height()
	 */
	SizeType Height(void) const
	{
		return ExplicitOps::Height(
			this->target
		);
	}


	/** Wrapper for Renderbuffer::RedSize()
	 *  @see Renderbuffer::RedSize()
	 */
	SizeType RedSize(void) const
	{
		return ExplicitOps::RedSize(
			this->target
		);
	}


	/** Wrapper for Renderbuffer::GreenSize()
	 *  @see Renderbuffer::GreenSize()
	 */
	SizeType GreenSize(void) const
	{
		return ExplicitOps::GreenSize(
			this->target
		);
	}


	/** Wrapper for Renderbuffer::BlueSize()
	 *  @see Renderbuffer::BlueSize()
	 */
	SizeType BlueSize(void) const
	{
		return ExplicitOps::BlueSize(
			this->target
		);
	}


	/** Wrapper for Renderbuffer::AlphaSize()
	 *  @see Renderbuffer::AlphaSize()
	 */
	SizeType AlphaSize(void) const
	{
		return ExplicitOps::AlphaSize(
			this->target
		);
	}


	/** Wrapper for Renderbuffer::DepthSize()
	 *  @see Renderbuffer::DepthSize()
	 */
	SizeType DepthSize(void) const
	{
		return ExplicitOps::DepthSize(
			this->target
		);
	}


	/** Wrapper for Renderbuffer::StencilSize()
	 *  @see Renderbuffer::StencilSize()
	 */
	SizeType StencilSize(void) const
	{
		return ExplicitOps::StencilSize(
			this->target
		);
	}


	/** Wrapper for Renderbuffer::Samples()
	 *  @see Renderbuffer::Samples()
	 */
	SizeType Samples(void) const
	{
		return ExplicitOps::Samples(
			this->target
		);
	}


	/** Wrapper for Renderbuffer::InternalFormat()
	 *  @see Renderbuffer::InternalFormat()
	 */
	PixelDataInternalFormat InternalFormat(void) const
	{
		return ExplicitOps::InternalFormat(
			this->target
		);
	}


}; // class BoundObjOps

} // namespace oglplus

#endif // include guard

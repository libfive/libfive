
/**
 *  @file oglplus/bound/buffer.hpp
 *  @brief Specialization of ObjectOps for Buffer.
 *
 *  Automatically generated file, do not edit manually!
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */


#ifndef OGLPLUS_BOUND_BUFFER_1107121519_HPP
#define OGLPLUS_BOUND_BUFFER_1107121519_HPP

#include <oglplus/object/bound.hpp>
#include <oglplus/buffer.hpp>
#include <utility>

namespace oglplus {

/// Specialization of the BoundObjOps for Buffer  >.
/** This template implements wrappers around the member functions
 *  of Buffer, which have
 *  a BufferTarget parameter
 *  specifying the binding point on which they should operate.
 *
 *  @note Do not use this template class directly use
 *  Bound < Buffer > or the Context::Current()
 *  function instead.
 *
 *  @ingroup utility_classes
 */
template <>
class BoundObjOps<tag::Buffer>
{
private:
	typedef ObjectOps<tag::ExplicitSel, tag::Buffer> ExplicitOps;
public:
	typedef ExplicitOps::Target Target;
	Target target;

	BoundObjOps(void)
	{ }

	BoundObjOps(Target init_tgt)
	 : target(init_tgt)
	{ }

	/** Wrapper for Buffer::GetIntParam()
	 *  @see Buffer::GetIntParam()
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


	/** Wrapper for Buffer::Mapped()
	 *  @see Buffer::Mapped()
	 */
	Boolean Mapped(void) const
	{
		return ExplicitOps::Mapped(
			this->target
		);
	}


	/** Wrapper for Buffer::Resize()
	 *  @see Buffer::Resize()
	 */
	const BoundObjOps& Resize(
		BufferSize size,
		BufferUsage usage = BufferUsage::StaticDraw
	) const
	{
		ExplicitOps::Resize(
			this->target,
			size,
			usage
		);
		return *this;
	}


	/** Wrapper for Buffer::Data()
	 *  @see Buffer::Data()
	 */
	const BoundObjOps& Data(
		const BufferData & data,
		BufferUsage usage = BufferUsage::StaticDraw
	) const
	{
		ExplicitOps::Data(
			this->target,
			data,
			usage
		);
		return *this;
	}


	/** Wrapper for Buffer::RawData()
	 *  @see Buffer::RawData()
	 */
	const BoundObjOps& RawData(
		BufferSize size,
		const GLvoid * data,
		BufferUsage usage = BufferUsage::StaticDraw
	) const
	{
		ExplicitOps::RawData(
			this->target,
			size,
			data,
			usage
		);
		return *this;
	}


	/** Wrapper for Buffer::Data()
	 *  @see Buffer::Data()
	 */
	template <typename GLtype>
	const BoundObjOps& Data(
		SizeType count,
		const GLtype * data,
		BufferUsage usage = BufferUsage::StaticDraw
	) const
	{
		ExplicitOps::Data(
			this->target,
			count,
			data,
			usage
		);
		return *this;
	}


	/** Wrapper for Buffer::SubData()
	 *  @see Buffer::SubData()
	 */
	const BoundObjOps& SubData(
		BufferSize offset,
		const BufferData & data
	) const
	{
		ExplicitOps::SubData(
			this->target,
			offset,
			data
		);
		return *this;
	}


	/** Wrapper for Buffer::SubData()
	 *  @see Buffer::SubData()
	 */
	template <typename GLtype>
	const BoundObjOps& SubData(
		BufferSize offset,
		SizeType count,
		const GLtype * data
	) const
	{
		ExplicitOps::SubData(
			this->target,
			offset,
			count,
			data
		);
		return *this;
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3

	/** Wrapper for Buffer::ClearData()
	 *  @see Buffer::ClearData()
	 */
	template <typename GLtype>
	const BoundObjOps& ClearData(
		PixelDataInternalFormat internal_format,
		PixelDataFormat format,
		const GLtype * data
	) const
	{
		ExplicitOps::ClearData(
			this->target,
			internal_format,
			format,
			data
		);
		return *this;
	}
#endif // GL_VERSION_4_3

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3

	/** Wrapper for Buffer::ClearSubData()
	 *  @see Buffer::ClearSubData()
	 */
	template <typename GLtype>
	const BoundObjOps& ClearSubData(
		PixelDataInternalFormat internal_format,
		BufferSize offset,
		BufferSize size,
		PixelDataFormat format,
		const GLtype * data
	) const
	{
		ExplicitOps::ClearSubData(
			this->target,
			internal_format,
			offset,
			size,
			format,
			data
		);
		return *this;
	}
#endif // GL_VERSION_4_3

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_4 || GL_ARB_buffer_storage

	/** Wrapper for Buffer::Storage()
	 *  @see Buffer::Storage()
	 */
	const BoundObjOps& Storage(
		const BufferData & data,
		Bitfield< BufferStorageBit > flags
	) const
	{
		ExplicitOps::Storage(
			this->target,
			data,
			flags
		);
		return *this;
	}
#endif // GL_VERSION_4_4 GL_ARB_buffer_storage

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_4 || GL_ARB_buffer_storage

	/** Wrapper for Buffer::Storage()
	 *  @see Buffer::Storage()
	 */
	const BoundObjOps& Storage(
		BufferSize size,
		const void * data,
		Bitfield< BufferStorageBit > flags
	) const
	{
		ExplicitOps::Storage(
			this->target,
			size,
			data,
			flags
		);
		return *this;
	}
#endif // GL_VERSION_4_4 GL_ARB_buffer_storage

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_4 || GL_ARB_buffer_storage

	/** Wrapper for Buffer::ImmutableStorage()
	 *  @see Buffer::ImmutableStorage()
	 */
	Boolean ImmutableStorage(void) const
	{
		return ExplicitOps::ImmutableStorage(
			this->target
		);
	}
#endif // GL_VERSION_4_4 GL_ARB_buffer_storage

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_4 || GL_ARB_buffer_storage

	/** Wrapper for Buffer::StorageFlags()
	 *  @see Buffer::StorageFlags()
	 */
	Bitfield< BufferStorageBit > StorageFlags(void) const
	{
		return ExplicitOps::StorageFlags(
			this->target
		);
	}
#endif // GL_VERSION_4_4 GL_ARB_buffer_storage

#if OGLPLUS_DOCUMENTATION_ONLY || GL_ARB_sparse_buffer

	/** Wrapper for Buffer::PageCommitment()
	 *  @see Buffer::PageCommitment()
	 */
	const BoundObjOps& PageCommitment(
		BufferSize offset,
		BufferSize size,
		Boolean commit
	) const
	{
		ExplicitOps::PageCommitment(
			this->target,
			offset,
			size,
			commit
		);
		return *this;
	}
#endif // GL_ARB_sparse_buffer


	/** Wrapper for Buffer::Size()
	 *  @see Buffer::Size()
	 */
	SizeType Size(void) const
	{
		return ExplicitOps::Size(
			this->target
		);
	}


	/** Wrapper for Buffer::Usage()
	 *  @see Buffer::Usage()
	 */
	BufferUsage Usage(void) const
	{
		return ExplicitOps::Usage(
			this->target
		);
	}


	/** Wrapper for Buffer::Access()
	 *  @see Buffer::Access()
	 */
	Bitfield< BufferMapAccess > Access(void) const
	{
		return ExplicitOps::Access(
			this->target
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_NV_shader_buffer_load

	/** Wrapper for Buffer::MakeResident()
	 *  @see Buffer::MakeResident()
	 */
	const BoundObjOps& MakeResident(
		AccessSpecifier access
	) const
	{
		ExplicitOps::MakeResident(
			this->target,
			access
		);
		return *this;
	}
#endif // GL_NV_shader_buffer_load

#if OGLPLUS_DOCUMENTATION_ONLY || GL_NV_shader_buffer_load

	/** Wrapper for Buffer::MakeNonResident()
	 *  @see Buffer::MakeNonResident()
	 */
	const BoundObjOps& MakeNonResident(void) const
	{
		ExplicitOps::MakeNonResident(
			this->target
		);
		return *this;
	}
#endif // GL_NV_shader_buffer_load

#if OGLPLUS_DOCUMENTATION_ONLY || GL_NV_shader_buffer_load

	/** Wrapper for Buffer::GPUAddress()
	 *  @see Buffer::GPUAddress()
	 */
	BufferGPUAddress GPUAddress(void) const
	{
		return ExplicitOps::GPUAddress(
			this->target
		);
	}
#endif // GL_NV_shader_buffer_load


}; // class BoundObjOps

} // namespace oglplus

#endif // include guard

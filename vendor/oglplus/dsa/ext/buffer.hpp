/**
 *  @file oglplus/dsa/ext/buffer.hpp
 *  @brief Buffer wrappers with direct state access
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_DSA_EXT_BUFFER_1309301821_HPP
#define OGLPLUS_DSA_EXT_BUFFER_1309301821_HPP

#include <oglplus/buffer.hpp>
#include <oglplus/dsa/ext/buffer_map.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_EXT_direct_state_access

/// Class wrapping buffer-related functionality with direct state access
/** @note Do not use this class directly, use DSABufferEXT instead.
 *
 */
template <>
class ObjectOps<tag::DirectStateEXT, tag::Buffer>
 : public ObjZeroOps<tag::DirectStateEXT, tag::Buffer>
{
protected:
	ObjectOps(BufferName name)
	OGLPLUS_NOEXCEPT(true)
	 : ObjZeroOps<tag::DirectStateEXT, tag::Buffer>(name)
	{ }
public:
#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	ObjectOps(ObjectOps&&) = default;
	ObjectOps(const ObjectOps&) = default;
	ObjectOps& operator = (ObjectOps&&) = default;
	ObjectOps& operator = (const ObjectOps&) = default;
#else
	typedef ObjZeroOps<tag::DirectStateEXT, tag::Buffer> _base;

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

	/// Types related to Buffer
	typedef BufferOps::Property Property;

	/// Mapping of the buffer to the client address space
	typedef DSABufferTypedMapEXT<GLubyte> Map;

	/// Returns true if the buffer is mapped
	/**
	 *  @glsymbols
	 *  @glfunref{GetBufferParameter}
	 *  @gldefref{BUFFER_MAPPED}
	 *
	 *  @throws Error
	 */
	Boolean Mapped(void) const
	{
		return Boolean(
			GetIntParam(GL_BUFFER_MAPPED),
			std::nothrow
		);
	}

	/// Allocates buffer storage to the specified size without any data
	/** This member function allows to (re-)allocate the buffer storage
	 *  to the specifies @p size, without uploading any data.
	 *
	 *  @glsymbols
	 *  @glfunref{NamedBufferDataEXT}
	 *
	 *  @see SubData
	 *  @throws Error
	 */
	void Resize(
		BufferSize size,
		BufferUsage usage = BufferUsage::StaticDraw
	)
	{
		OGLPLUS_GLFUNC(NamedBufferDataEXT)(
			_obj_name(),
			size.Get(),
			nullptr,
			GLenum(usage)
		);
		OGLPLUS_CHECK(
			NamedBufferDataEXT,
			ObjectError,
			Object(*this).
			EnumParam(usage)
		);
	}

	/// Uploads (sets) the buffer data
	/** This member function uploads the specified data to this buffer
	 *  using the @p usage as hint.
	 *
	 *  @see SubData
	 *  @see CopySubData
	 *  @throws Error
	 */
	void Data(
		const BufferData& data,
		BufferUsage usage = BufferUsage::StaticDraw
	) const
	{
		OGLPLUS_GLFUNC(NamedBufferDataEXT)(
			_obj_name(),
			GLsizei(data.Size()),
			data.Data(),
			GLenum(usage)
		);
		OGLPLUS_CHECK(
			NamedBufferDataEXT,
			ObjectError,
			Object(*this).
			EnumParam(usage)
		);
	}

	/// Uploads (sets) the buffer data
	/** This member function uploads @p size bytes
	 *  from the location pointed to by @p data to this buffer
	 *  using the @p usage as hint.
	 *
	 *  @see SubData
	 *  @see CopySubData
	 *  @throws Error
	 */
	void RawData(
		BufferSize size,
		const GLvoid* data,
		BufferUsage usage = BufferUsage::StaticDraw
	) const
	{
		Data(BufferData(size, data), usage);
	}

	/// Uploads (sets) the buffer data
	/** This member function uploads @p count units of @c sizeof(GLtype)
	 *  from the location pointed to by @p data to this buffer
	 *  using the @p usage as hint.
	 *
	 *  @see SubData
	 *  @see CopySubData
	 *  @throws Error
	 */
	template <typename GLtype>
	void Data(
		GLsizei count,
		const GLtype* data,
		BufferUsage usage = BufferUsage::StaticDraw
	) const
	{
		Data(BufferData(count, data), usage);
	}

	/// Uploads (sets) a subrange of the buffer data
	/**
	 *  @see Data
	 *  @see CopySubData
	 *  @throws Error
	 */
	void SubData(
		BufferSize offset,
		const BufferData& data
	) const
	{
		OGLPLUS_GLFUNC(NamedBufferSubDataEXT)(
			_obj_name(),
			GLintptr(offset.Get()),
			GLsizei(data.Size()),
			data.Data()
		);
		OGLPLUS_CHECK(
			NamedBufferSubDataEXT,
			ObjectError,
			Object(*this)
		);
	}

	/// Uploads (sets) a subrange of the buffer data
	/**
	 *  @see Data
	 *  @see CopySubData
	 *  @throws Error
	 */
	template <typename GLtype>
	void SubData(
		BufferSize offset,
		GLsizei count,
		GLtype* data
	) const
	{
		SubData(offset, BufferData(count, data));
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_1 || GL_ARB_copy_buffer
	/// Copy data between buffers
	/**
	 *  @see Data
	 *  @see SubData
	 *  @throws Error
	 *
	 *  @glvoereq{3,1,ARB,copy_buffer}
	 */
	static void CopySubData(
		BufferName readbuffer,
		BufferName writebuffer,
		BufferSize readoffset,
		BufferSize writeoffset,
		BufferSize size
	)
	{
		OGLPLUS_GLFUNC(NamedCopyBufferSubDataEXT)(
			GetGLName(readbuffer),
			GetGLName(writebuffer),
			GLintptr(readoffset.Get()),
			GLintptr(writeoffset.Get()),
			GLsizeiptr(size.Get())
		);
		OGLPLUS_CHECK(
			NamedCopyBufferSubDataEXT,
			ObjectPairError,
			Subject(readbuffer).
			Object(writebuffer)
		);
	}
#endif // copy buffer

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_3
	/// Clear the buffer data
	/**
	 *  @see Data
	 *  @see ClearSubData
	 *  @see SubData
	 *  @see CopySubData
	 *
	 *  @throws Error
	 *
	 *  @glverreq{4,3}
	 */
	template <typename GLtype>
	void ClearData(
		PixelDataInternalFormat internal_format,
		PixelDataFormat format,
		const GLtype* data
	) const
	{
		OGLPLUS_GLFUNC(ClearNamedBufferDataEXT)(
			_obj_name(),
			GLenum(internal_format),
			GLenum(format),
			GLenum(GetDataType<GLtype>()),
			data
		);
		OGLPLUS_CHECK(
			ClearNamedBufferDataEXT,
			ObjectError,
			Object(*this).
			EnumParam(internal_format)
		);
	}

	/// Clear a subrange of the buffer data
	/**
	 *  @see Data
	 *  @see ClearData
	 *  @see SubData
	 *  @see CopySubData
	 *
	 *  @throws Error
	 *
	 *  @glverreq{4,3}
	 */
	template <typename GLtype>
	void ClearSubData(
		PixelDataInternalFormat internal_format,
		BufferSize offset,
		BufferSize size,
		PixelDataFormat format,
		const GLtype* data
	) const
	{
		OGLPLUS_GLFUNC(ClearNamedBufferSubDataEXT)(
			_obj_name(),
			GLenum(internal_format),
			GLintptr(offset.Get()),
			GLsizeiptr(size.Get()),
			GLenum(format),
			GLenum(GetDataType<GLtype>()),
			data
		);
		OGLPLUS_CHECK(
			ClearNamedBufferSubDataEXT,
			ObjectError,
			Object(*this).
			EnumParam(internal_format)
		);
	}
#endif

	/// Returns the buffer size
	/**
	 *  @glsymbols
	 *  @glfunref{GetBufferParameter}
	 *  @gldefref{BUFFER_SIZE}
	 *
	 *  @throws Error
	 */
	GLsizei Size(void) const
	{
		return GLsizei(GetIntParam(GL_BUFFER_SIZE));
	}

	/// Returns the buffer usage
	/**
	 *  @see Access
	 *
	 *  @glsymbols
	 *  @glfunref{GetBufferParameter}
	 *  @gldefref{BUFFER_USAGE}
	 *
	 *  @throws Error
	 */
	BufferUsage Usage(void) const
	{
		return BufferUsage(GetIntParam(GL_BUFFER_USAGE));
	}

	/// Returns the buffer usage
	/**
	 *  @see Usage
	 *
	 *  @glsymbols
	 *  @glfunref{GetBufferParameter}
	 *  @gldefref{BUFFER_ACCESS}
	 *
	 *  @throws Error
	 */
	Bitfield<BufferMapAccess> Access(void) const
	{
		return Bitfield<BufferMapAccess>(
			GLbitfield(GetIntParam(GL_BUFFER_ACCESS))
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY || GL_NV_shader_buffer_load
	/// Makes this buffer accessible to GLSL shaders
	/**
	 *  @glextreq{NV,shader_buffer_load}
	 *  @glsymbols
	 *  @glfunref{MakeNamedBufferResidentNV}
	 *
	 *  @throws Error
	 */
	void MakeResident(AccessSpecifier access) const
	{
		OGLPLUS_GLFUNC(MakeNamedBufferResidentNV)(
			_obj_name(),
			GLenum(access)
		);
		OGLPLUS_CHECK(
			MakeNamedBufferResidentNV,
			ObjectError,
			Object(*this).
			EnumParam(access)
		);
	}

	/// Makes this buffer inaccessible to GLSL shaders
	/**
	 *  @glextreq{NV,shader_buffer_load}
	 *  @glsymbols
	 *  @glfunref{MakeNamedBufferNonResidentNV}
	 *
	 *  @throws Error
	 */
	void MakeNonResident(void) const
	{
		OGLPLUS_GLFUNC(MakeNamedBufferNonResidentNV)(_obj_name());
		OGLPLUS_CHECK(
			MakeNamedBufferNonResidentNV,
			ObjectError,
			Object(*this)
		);
	}

	/// Returns the GPU address of this buffer
	/**
	 *  @glextreq{NV,shader_buffer_load}
	 *  @glsymbols
	 *  @glfunref{GetNamedBufferParameterui64vNV}
	 *  @gldefref{BUFFER_GPU_ADDRESS_NV}
	 *
	 *  @throws Error
	 */
	BufferGPUAddress GPUAddress(void) const
	{
		GLuint64EXT value = 0;
		OGLPLUS_GLFUNC(GetNamedBufferParameterui64vNV)(
			_obj_name(),
			GL_BUFFER_GPU_ADDRESS_NV,
			&value
		);
		OGLPLUS_CHECK(
			GetNamedBufferParameterui64vNV,
			ObjectError,
			Object(*this)
		);
		return BufferGPUAddress(value);
	}
#endif
};

/// Buffer operations with direct state access
typedef ObjectOps<tag::DirectStateEXT, tag::Buffer>
	DSABufferOpsEXT;

// Helper class for syntax sugar operators
struct DSABufferOpsAndUsageEXT
{
	DSABufferOpsEXT& buf;
	BufferUsage usage;

	DSABufferOpsAndUsageEXT(DSABufferOpsEXT& b, BufferUsage u)
	 : buf(b)
	 , usage(u)
	{ }
};

inline DSABufferOpsAndUsageEXT operator << (
	DSABufferOpsEXT& buf,
	BufferUsage usage
)
{
	return DSABufferOpsAndUsageEXT(buf, usage);
}

// Helper class for syntax sugar operators
struct DSABufferOpsAndIdxTgtEXT
{
	DSABufferOpsEXT& buf;
	BufferIndexedTarget target;

	DSABufferOpsAndIdxTgtEXT(DSABufferOpsEXT& b, BufferIndexedTarget t)
	 : buf(b)
	 , target(t)
	{ }
};

inline DSABufferOpsAndIdxTgtEXT operator << (
	DSABufferOpsEXT& buf,
	BufferIndexedTarget target
)
{
	return DSABufferOpsAndIdxTgtEXT(buf, target);
}

// Helper class for syntax sugar operators
struct DSABufferOpsAndOffsetEXT
{
	DSABufferOpsEXT& buf;
	GLintptr offset;

	DSABufferOpsAndOffsetEXT(DSABufferOpsEXT& b, GLintptr o)
	 : buf(b)
	 , offset(o)
	{ }
};

inline DSABufferOpsAndOffsetEXT operator + (
	DSABufferOpsEXT& buf,
	GLintptr offset
)
{
	return DSABufferOpsAndOffsetEXT(buf, offset);
}

// syntax-sugar operators

// Bind
inline DSABufferOpsEXT& operator << (
	DSABufferOpsEXT& buf,
	BufferTarget target
)
{
	buf.Bind(target);
	return buf;
}

// BindBase
inline DSABufferOpsEXT& operator << (
	const DSABufferOpsAndIdxTgtEXT& bat,
	GLuint index
)
{
	bat.buf.BindBase(bat.target, index);
	return bat.buf;
}

// Data
template <typename GLtype>
inline DSABufferOpsEXT& operator << (
	DSABufferOpsEXT& buf,
	const std::vector<GLtype>& data
)
{
	buf.Data(data);
	return buf;
}

// Data
template <typename GLtype>
inline DSABufferOpsEXT& operator << (
	DSABufferOpsAndUsageEXT&& bau,
	const std::vector<GLtype>& data
)
{
	bau.buf.Data(data, bau.usage);
	return bau.buf;
}

// Data
template <typename GLtype, std::size_t Count>
inline DSABufferOpsEXT& operator << (
	DSABufferOpsEXT& buf,
	const GLtype (&data)[Count]
)
{
	buf.Data(data);
	return buf;
}

// Data
template <typename GLtype, std::size_t Count>
inline DSABufferOpsEXT& operator << (
	DSABufferOpsAndUsageEXT&& bau,
	const GLtype (&data)[Count]
)
{
	bau.buf.Data(data, bau.usage);
	return bau.buf;
}

// SubData
template <typename GLtype>
inline DSABufferOpsEXT& operator << (
	DSABufferOpsAndOffsetEXT&& bao,
	const std::vector<GLtype>& data
)
{
	bao.buf.SubData(bao.offset, data);
	return bao.buf;
}

// SubData
template <typename GLtype, std::size_t Count>
inline DSABufferOpsEXT& operator << (
	DSABufferOpsAndOffsetEXT&& bao,
	const GLtype (&data)[Count]
)
{
	bao.buf.SubData(bao.offset, data);
	return bao.buf;
}

/// An @ref oglplus_object encapsulating the OpenGL buffer functionality
/**
 *  @ingroup oglplus_objects
 */
typedef Object<DSABufferOpsEXT> DSABufferEXT;

#else
#error Direct State Access Buffers not available
#endif // GL_EXT_direct_state_access

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/dsa/ext/buffer.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

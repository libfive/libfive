/**
 *  @file oglplus/dsa/buffer_map.hpp
 *  @brief BufferMap wrappers with direct state access
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_DSA_BUFFER_MAP_1309301821_HPP
#define OGLPLUS_DSA_BUFFER_MAP_1309301821_HPP

#include <oglplus/buffer.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_4_5 || GL_ARB_direct_state_access

class DSABufferRawMap
{
private:
	const GLintptr _offset;
	GLsizeiptr _size;
	GLvoid* _ptr;
	const GLuint _buf_name;

	static GLsizeiptr _get_size(GLuint buf_name)
	{
		GLint value = 0;
		OGLPLUS_GLFUNC(GetNamedBufferParameteriv)(
			buf_name,
			GL_BUFFER_SIZE,
			&value
		);
		OGLPLUS_CHECK(
			GetNamedBufferParameteriv,
			ObjectError,
			Object(BufferName(buf_name))
		);
		return GLsizeiptr(value);
	}

	static GLenum _translate(GLbitfield access)
	{
		switch(access)
		{
			case GL_MAP_READ_BIT:
				return GL_READ_ONLY;
			case GL_MAP_WRITE_BIT:
				return GL_WRITE_ONLY;
			case GL_MAP_READ_BIT|GL_MAP_WRITE_BIT:
				return GL_READ_WRITE;
		}
		return GL_READ_ONLY;
	}
public:
	/// Maps a range of the buffer
	/**
	 *  @param target use the buffer bound to the target specified
	 *  @param offset map offset in units of Type
	 *  @param size map size in units of Type
	 *  @param access the access specifier for the buffer mapping
	 *
	 *  @throws Error
	 */
	DSABufferRawMap(
		BufferName buffer,
		BufferSize offset,
		BufferSize size,
		Bitfield<BufferMapAccess> access
	): _offset(GLintptr(offset.Get()))
	 , _size(GLsizeiptr(size.Get()))
	 , _ptr(
		OGLPLUS_GLFUNC(MapNamedBufferRange)(
			GetGLName(buffer),
			_offset,
			_size,
			GLbitfield(access)
		)
	), _buf_name(GetGLName(buffer))
	{
		OGLPLUS_CHECK(
			MapNamedBufferRange,
			ObjectError,
			Object(buffer)
		);
	}

	/// Maps the whole buffer
	/**
	 *  @param target use the buffer bound to the target specified
	 *  @param access the access specifier for the buffer mapping
	 *
	 * This class is non-copyable.
	 *
	 *  @throws Error
	 */
	DSABufferRawMap(
		BufferName buffer,
		Bitfield<BufferMapAccess> access
	): _offset(0)
	 , _size(_get_size(GetGLName(buffer)))
	 , _ptr(
		OGLPLUS_GLFUNC(MapNamedBuffer)(
			GetGLName(buffer),
			_translate(GLbitfield(access))
		)
	), _buf_name(GetGLName(buffer))
	{
		OGLPLUS_CHECK(
			MapNamedBuffer,
			ObjectError,
			Object(buffer)
		);
	}

#if !OGLPLUS_NO_DELETED_FUNCTIONS
	DSABufferRawMap(const DSABufferRawMap&) = delete;
#else
private:
	DSABufferRawMap(const DSABufferRawMap&);
public:
#endif

	/// Move construction is enabled
	DSABufferRawMap(DSABufferRawMap&& temp)
	 : _offset(temp._offset)
	 , _size(temp._size)
	 , _ptr(temp._ptr)
	 , _buf_name(temp._buf_name)
	{
		temp._ptr = nullptr;
	}

	~DSABufferRawMap(void)
	{
		try { Unmap(); }
		catch(...) { }
	}

	/// Unmaps the buffer from client address space
	/**
	 *  @glsymbols
	 *  @glfunref{UnmapNamedBuffer}
	 *
	 *  @throws Error
	 */
	void Unmap(void)
	{
		if(_ptr != nullptr)
		{
			OGLPLUS_GLFUNC(UnmapNamedBuffer)(_buf_name);
			OGLPLUS_IGNORE(UnmapNamedBuffer);
			_ptr = nullptr;
		}
	}

	/// Returns true if the buffer is mapped
	bool Mapped(void) const
	{
		return _ptr != nullptr;
	}

	/// Returns the size (in bytes) of the mapped buffer
	BigSizeType Size(void) const
	{
		return BigSizeType(_size, std::nothrow);
	}

	/// Returns a const pointer to the mapped data
	/**
	 *  @pre Mapped()
	 */
	const GLvoid* RawData(void) const
	{
		assert(Mapped());
		return _ptr;
	}

	/// Returns a pointer to the mapped data
	/**
	 *  @pre Mapped()
	 */
	GLvoid* RawData(void)
	{
		assert(Mapped());
		return _ptr;
	}

	/// Indicate modifications to a mapped range
	/**
	 *  @glsymbols
	 *  @glfunref{FlushMappedNamedBufferRange}
	 *
	 *  @pre Mapped()
	 *
	 *  @throws Error
	 */
	void FlushRange(BufferSize offset, BufferSize length)
	{
		OGLPLUS_GLFUNC(FlushMappedNamedBufferRange)(
			_buf_name,
			GLintptr(offset.Get()),
			GLsizeiptr(length.Get())
		);
		OGLPLUS_CHECK(
			FlushMappedNamedBufferRange,
			ObjectError,
			Object(BufferName(_buf_name))
		);
	}
};

/// Untyped mapping of the buffer to the client address space
template <typename Type>
class DSABufferTypedMap
 : public DSABufferRawMap
{
public:
	/// Maps a range of the buffer
	/**
	 *  @param target use the buffer bound to the target specified
	 *  @param offset map offset in units of Type
	 *  @param size map size in units of Type
	 *  @param access the access specifier for the buffer mapping
	 *
	 *  @throws Error
	 */
	DSABufferTypedMap(
		BufferName buffer,
		BufferTypedSize<Type> offset,
		BufferTypedSize<Type> size,
		Bitfield<BufferMapAccess> access
	): DSABufferRawMap(buffer, offset, size, access)
	{ }

	/// Maps the whole buffer
	/**
	 *  @param target use the buffer bound to the target specified
	 *  @param access the access specifier for the buffer mapping
	 *
	 * This class is non-copyable.
	 *
	 *  @throws Error
	 */
	DSABufferTypedMap(
		BufferName buffer,
		Bitfield<BufferMapAccess> access
	): DSABufferRawMap(buffer, access)
	{ }

	/// Move construction is enabled
	DSABufferTypedMap(DSABufferTypedMap&& temp)
	 : DSABufferRawMap(static_cast<DSABufferRawMap&&>(temp))
	{ }

	/// Returns the count of elements of Type in the mapped buffer
	GLsizeiptr Count(void) const
	{
		assert(this->Size() % sizeof(Type) == 0);
		return this->Size() / sizeof(Type);
	}

	/// Returns a const pointer to the mapped data
	const Type* Data(void) const
	{
		return static_cast<const Type*>(this->RawData());
	}

	/// Returns a pointer to the mapped data
	Type* Data(void)
	{
		return static_cast<Type*>(this->RawData());
	}

	/// Returns a const reference to the element at the specified index
	const Type& At(GLuint index) const
	{
		assert(Data() != nullptr);
		assert(((index+1)*sizeof(Type)) <= std::size_t(this->Size()));
		return Data()[index];
	}

	/// Returns a reference to the element at the specified index
	Type& At(GLuint index)
	{
		assert(Data() != nullptr);
		assert(((index+1)*sizeof(Type)) <= std::size_t(this->Size()));
		return Data()[index];
	}
};

#endif // GL_ARB_direct_state_access

} // namespace oglplus

#endif // include guard

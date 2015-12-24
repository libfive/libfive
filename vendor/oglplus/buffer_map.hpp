/**
 *  @file oglplus/buffer_map.hpp
 *  @brief Buffer map wrapper
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_BUFFER_MAP_1107121519_HPP
#define OGLPLUS_BUFFER_MAP_1107121519_HPP

#include <oglplus/glfunc.hpp>
#include <oglplus/error/object.hpp>
#include <oglplus/buffer_map_access.hpp>
#include <oglplus/buffer_target.hpp>
#include <oglplus/buffer_size.hpp>

namespace oglplus {

#if OGLPLUS_DOCUMENTATION_ONLY || GL_VERSION_3_0
/// Untyped mapping of the buffer to the client address space
class BufferRawMap
{
private:
	const GLintptr _offset;
	GLsizeiptr _size;
	GLvoid* _ptr;
	const BufferTarget _target;

	static GLsizeiptr _get_size(BufferTarget target)
	{
		GLint value = 0;
		OGLPLUS_GLFUNC(GetBufferParameteriv)(
			GLenum(target),
			GL_BUFFER_SIZE,
			&value
		);
		OGLPLUS_CHECK(
			GetBufferParameteriv,
			ObjectError,
			ObjectBinding(target)
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
	 *  @param target use the buffer bound to the specified target
	 *  @param byte_offset map offset in machine bytes
	 *  @param size_bytes map size in machine bytes
	 *  @param access the access specifier for the buffer mapping
	 *
	 *  @glsymbols
	 *  @glfunref{MapBufferRange}
	 *
	 *  @throws Error
	 */
	BufferRawMap(
		BufferTarget target,
		BufferSize byte_offset,
		BufferSize size_bytes,
		Bitfield<BufferMapAccess> access
	): _offset(GLintptr(byte_offset.Get()))
	 , _size(GLsizeiptr(size_bytes.Get()))
	 , _ptr(
		OGLPLUS_GLFUNC(MapBufferRange)(
			GLenum(target),
			_offset,
			_size,
			GLbitfield(access)
		)
	), _target(target)
	{
		OGLPLUS_CHECK(
			MapBufferRange,
			Error,
			EnumParam(target)
		);
	}

	/// Maps the whole buffer
	/**
	 *  @param target use the buffer bound to the target specified
	 *  @param access the access specifier for the buffer mapping
	 *
	 * This class is non-copyable.
	 *
	 *  @glsymbols
	 *  @glfunref{MapBuffer}
	 *
	 *  @throws Error
	 */
	BufferRawMap(BufferTarget target, Bitfield<BufferMapAccess> access)
	 : _offset(0)
	 , _size(_get_size(target))
	 , _ptr(
		OGLPLUS_GLFUNC(MapBuffer)(
			GLenum(target),
			_translate(GLbitfield(access))
		)
	), _target(target)
	{
		OGLPLUS_CHECK(
			MapBuffer,
			ObjectError,
			ObjectBinding(_target)
		);
	}

#if !OGLPLUS_NO_DELETED_FUNCTIONS
	BufferRawMap(const BufferRawMap&) = delete;
#else
private:
	BufferRawMap(const BufferRawMap&);
public:
#endif
	/// Move construction is enabled
	BufferRawMap(BufferRawMap&& temp)
	 : _offset(temp._offset)
	 , _size(temp._size)
	 , _ptr(temp._ptr)
	 , _target(temp._target)
	{
		temp._ptr = nullptr;
	}

	/// Unmaps the buffer from client address space (if mapped)
	~BufferRawMap(void)
	{
		try { Unmap(); }
		catch(...){ }
	}

	/// Unmaps the buffer from client address space
	/**
	 *  @glsymbols
	 *  @glfunref{UnmapBuffer}
	 *
	 *  @throws Error
	 */
	void Unmap(void)
	{
		if(_ptr != nullptr)
		{
			OGLPLUS_GLFUNC(UnmapBuffer)(GLenum(_target));
			OGLPLUS_VERIFY(
				UnmapBuffer,
				ObjectError,
				ObjectBinding(_target)
			);
			_ptr = nullptr;
		}
	}

	/// Returns true if the buffer is mapped
	bool Mapped(void) const
	{
		return _ptr != nullptr;
	}

	/// Returns the size (in bytes) of the mapped buffer
	GLsizeiptr Size(void) const
	{
		return _size;
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
	 *  @glfunref{FlushMappedBufferRange}
	 *
	 *  @pre Mapped()
	 *
	 *  @throws Error
	 */
	void FlushRange(BufferSize offset, BufferSize length)
	{
		OGLPLUS_GLFUNC(FlushMappedBufferRange)(
			GLenum(_target),
			GLintptr(offset.Get()),
			GLsizeiptr(length.Get())
		);
		OGLPLUS_CHECK(
			FlushMappedBufferRange,
			ObjectError,
			ObjectBinding(_target)
		);
	}
};

/// Typed mapping of the buffer to the client address space
template <typename Type>
class BufferTypedMap
 : public BufferRawMap
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
	BufferTypedMap(
		BufferTarget target,
		BufferTypedSize<Type> offset,
		BufferTypedSize<Type> size,
		Bitfield<BufferMapAccess> access
	): BufferRawMap(target, offset, size, access)
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
	BufferTypedMap(BufferTarget target, Bitfield<BufferMapAccess> access)
	 : BufferRawMap(target, access)
	{ }

	/// Move construction is enabled
	BufferTypedMap(BufferTypedMap&& temp)
	 : BufferRawMap(static_cast<BufferRawMap&&>(temp))
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

	/// Indicate modifications to a mapped range of elements of Type
	/**
	 *  @param start Index of the first element.
	 *  @param count The number of elements to be flushed.
	 *
	 *  @glsymbols
	 *  @glfunref{FlushMappedBufferRange}
	 *
	 *  @pre this->Mapped()
	 *
	 *  @throws Error
	 */
	void FlushElements(
		BufferTypedSize<Type> start,
		BufferTypedSize<Type> count
	)
	{
		this->FlushRange(start, count);
	}
};
#endif // GL_VERSION_3_0

} // namespace oglplus

#endif // include guard

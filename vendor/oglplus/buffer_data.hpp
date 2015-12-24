/**
 *  @file oglplus/buffer_data.hpp
 *  @brief Object wrapping data to be stored in a Buffer
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_BUFFER_DATA_1310102147_HPP
#define OGLPLUS_BUFFER_DATA_1310102147_HPP

#include <oglplus/buffer_size.hpp>

namespace oglplus {

/// Class used for passing the size of and pointer to data to be put in a Buffer
class BufferData
{
private:
	BufferSize _size;
	const GLvoid* _data;

	BufferData(const BufferData&);
public:
	/// Construction from @p size in bytes and pointer to @p data
	BufferData(BufferSize size, const GLvoid* data)
	 : _size(size)
	 , _data(data)
	{ }

	/// Construction from @p count of instances of type @c T at @p data
	template <typename T>
	BufferData(SizeType count, const T* data)
	 : _size(count, data)
	 , _data(data)
	{ }

	/// Construction from an array with known size
	template <typename T, std::size_t N>
	BufferData(const T (&data)[N])
	 : _size(data)
	 , _data(data)
	{ }

	/// Construction from a std::array
	template <typename T, std::size_t N>
	BufferData(const std::array<T, N>& a)
	 : _size(a)
	 , _data(a.data())
	{ }

	/// Construction from a std::vector
	template <typename T>
	BufferData(const std::vector<T>& v)
	 : _size(v)
	 , _data(v.data())
	{ }

	GLsizeiptr Size(void) const
	{
		return _size.Get();
	}

	const GLvoid* Data(void) const
	{
		return _data;
	}
};

} // namespace oglplus

#endif // include guard

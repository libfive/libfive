/**
 *  @file oglplus/output_data.hpp
 *  @brief Object wrapping buffer to store output data
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_OUTPUT_DATA_1310102147_HPP
#define OGLPLUS_OUTPUT_DATA_1310102147_HPP

#include <oglplus/buffer_size.hpp>
#include <oglplus/data_type.hpp>
#include <oglplus/size_type.hpp>

namespace oglplus {

/// Class used for passing the address and size of a writable buffer to functions
class OutputData
{
public:
	typedef OneOf<
		GLenum,
		std::tuple<
			DataType,
			PixelDataType
		>
	> PixDataType;
private:
	PixDataType _type;
	BufferSize _size;
	GLvoid* _addr;

	OutputData(const OutputData&);
public:

	/// Construction from @p size in bytes and pointer to @p addr
	OutputData(BufferSize size, GLvoid* addr)
	 : _type(DataType::UnsignedByte)
	 , _size(size)
	 , _addr(addr)
	{ }

	/// Construction from @p type, @p size in bytes and pointer to @p addr
	OutputData(PixDataType type, BufferSize size, GLvoid* addr)
	 : _type(type)
	 , _size(size)
	 , _addr(addr)
	{ }

	/// Construction from @p count of instances of type @c T at @p addr
	template <typename T>
	OutputData(SizeType count, T* addr)
	 : _type(GetDataType<T>())
	 , _size(unsigned(count), addr)
	 , _addr(addr)
	{ }

	/// Construction from an array with known size
	template <typename T, std::size_t N>
	OutputData(T (&addr)[N])
	 : _type(GetDataType<T>())
	 , _size(addr)
	 , _addr(addr)
	{ }

	/// Construction from a std::array
	template <typename T, std::size_t N>
	OutputData(std::array<T, N>& a)
	 : _type(GetDataType<T>())
	 , _size(a)
	 , _addr(a.data())
	{ }

	/// Construction from a std::vector
	template <typename T>
	OutputData(std::vector<T>& v)
	 : _type(GetDataType<T>())
	 , _size(v)
	 , _addr(v.data())
	{ }

	PixDataType Type(void) const
	{
		return _type;
	}

	BigSizeType Size(void) const
	{
		return _size;
	}

	GLvoid* Addr(void) const
	{
		return _addr;
	}
};

} // namespace oglplus

#endif // include guard

/**
 *  @file oglplus/detaul/aligned_pod_array.hpp
 *  @brief Aligned plain-old-data array
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_AUX_ALIGNED_POD_ARRAY_1107121519_HPP
#define OGLPLUS_AUX_ALIGNED_POD_ARRAY_1107121519_HPP

#include <cassert>
#include <cstddef>
#include <cstring>

namespace oglplus {
namespace aux {

// Helper class for storing (image) PO data
class AlignedPODArray
{
private:
	std::size_t _count;
	std::size_t _sizeof;

	void* _data;

	void (*_delete)(void*);
	void* (*_dup)(const void*, std::size_t);

	template <typename T>
	static void _do_delete(void* ptr)
	{
		delete[] static_cast<T*>(ptr);
	}

	template <typename T>
	static void* _do_dup(const void* src, std::size_t count)
	{
		T* dst = new T[count];
		if(src != nullptr) std::memcpy(dst, src, count*sizeof(T));
		return static_cast<void*>(dst);
	}

	void* _data_copy(void) const
	{
		if(_dup) return _dup(_data, _count);
		assert(!_data && !_count);
		return nullptr;
	}

	void* _release_data(void)
	{
		void* result = _data;
		_data = nullptr;
		_count = 0;
		return result;
	}

	void _cleanup(void)
	{
		if(_data)
		{
			assert(_delete);
			_delete(_data);
		}
	}
public:
	AlignedPODArray(void)
	 : _count(0)
	 , _sizeof(0)
	 , _data(nullptr)
	 , _delete(nullptr)
	 , _dup(nullptr)
	{ }

	template <typename T>
	AlignedPODArray(const T* data, std::size_t count)
	 : _count(count)
	 , _sizeof(sizeof(T))
	 , _data(_do_dup<T>(static_cast<const void*>(data), count))
	 , _delete(&_do_delete<T>)
	 , _dup(&_do_dup<T>)
	{ }

	AlignedPODArray(AlignedPODArray&& tmp)
	 : _count(tmp._count)
	 , _sizeof(tmp._sizeof)
	 , _data(tmp._release_data())
	 , _delete(tmp._delete)
	 , _dup(tmp._dup)
	{ }

	AlignedPODArray(const AlignedPODArray& that)
	 : _count(that._count)
	 , _sizeof(that._sizeof)
	 , _data(that._data_copy())
	 , _delete(that._delete)
	 , _dup(that._dup)
	{ }

	~AlignedPODArray(void)
	{
		_cleanup();
	}

	AlignedPODArray& operator = (AlignedPODArray&& tmp)
	{
		if(this != &tmp)
		{
			_cleanup();
			_count = tmp._count;
			_sizeof = tmp._sizeof;
			_data = tmp._release_data();
			_delete = tmp._delete;
			_dup = tmp._dup;
		}
		return *this;
	}

	AlignedPODArray& operator = (const AlignedPODArray& that)
	{
		if(this != &that)
		{
			void* tmp_data = that._data_copy();
			_cleanup();
			_count = that._count;
			_sizeof = that._sizeof;
			_data = tmp_data;
			_delete = that._delete;
			_dup = that._dup;
		}
		return *this;
	}

	void fill(unsigned char b)
	{
		std::memset(begin(), b, size());
	}

	void* begin(void) const
	{
		return const_cast<void*>(_data);
	}

	void* end(void) const
	{
		typedef unsigned char byte;
		return static_cast<void*>(static_cast<byte*>(begin())+size());
	}

	void* at(std::size_t offs) const
	{
		assert(!empty());
		typedef unsigned char byte;
		std::size_t boffs = offs*_sizeof;
		return static_cast<void*>(static_cast<byte*>(begin())+boffs);
	}

	std::size_t Count(void) const
	{
		return _count;
	}

	std::size_t ElemSize(void) const
	{
		return _sizeof;
	}

	std::size_t size(void) const
	{
		return Count()*ElemSize();
	}

	bool empty(void) const
	{
		return _data == nullptr;
	}
};

} // namespace aux
} // namespace oglplus

#endif // include guard

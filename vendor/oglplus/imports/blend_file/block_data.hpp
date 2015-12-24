/**
 *  @file oglplus/imports/blend_file/block_data.hpp
 *  @brief Helper class providing access to .blend file block data
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMPORTS_BLEND_FILE_BLOCK_DATA_1107121519_HPP
#define OGLPLUS_IMPORTS_BLEND_FILE_BLOCK_DATA_1107121519_HPP

#include <oglplus/imports/blend_file/visitor.hpp>

namespace oglplus {
namespace imports {

/// Class wrapping the data of a file block
class BlendFileBlockData
{
private:
	// TODO: some kind of caching in BlendFile or BlendFileBlock
	// and only a reference to the buffer here
	// to make this class more lightweight
	std::vector<char> _block_data;
	Endian _byte_order;
	std::size_t _ptr_size;
	std::size_t _struct_size;

	friend class BlendFile;

	BlendFileBlockData(
		std::vector<char>&& block_data,
		Endian byte_order,
		std::size_t ptr_size,
		std::size_t struct_size
	): _block_data(block_data)
	 , _byte_order(byte_order)
	 , _ptr_size(ptr_size)
	 , _struct_size(struct_size)
	{ }

	template <unsigned Level>
	BlendFilePointerTpl<Level> _do_make_pointer(
		const char* pos,
		std::size_t type_index
	) const;

	template <unsigned Level>
	BlendFilePointerTpl<Level> _do_get_pointer(
		std::size_t type_index,
		std::size_t field_offset,
		std::size_t block_element,
		std::size_t field_element,
		std::size_t data_offset
	) const;

	template <unsigned Level>
	BlendFilePointerTpl<Level> _get_pointer(
		const BlendFileFlattenedStructField& flat_field,
		std::size_t block_element,
		std::size_t field_element,
		std::size_t data_offset
	) const;
public:
	BlendFileBlockData(BlendFileBlockData&& tmp)
	 : _block_data(tmp._block_data)
	 , _byte_order(tmp._byte_order)
	 , _ptr_size(tmp._ptr_size)
	 , _struct_size(tmp._struct_size)
	{ }

	/// Returns the raw data of the block
	const char* RawData(void) const
	{
		return _block_data.data();
	}

	/// Returns the i-th byte in the block
	char RawByte(std::size_t i) const
	{
		assert(i < _block_data.size());
		return _block_data[i];
	}

	/// returns the size (in bytes) of the raw data
	std::size_t DataSize(void) const
	{
		return _block_data.size();
	}

	/// Returns a pointer at the specified index
	BlendFilePointer AsPointerTo(
		const BlendFileType& type,
		std::size_t index = 0,
		std::size_t data_offset = 0
	) const;

	/// Returns the value of the specified field as a pointer
	BlendFilePointer GetPointer(
		const BlendFileFlattenedStructField& flat_field,
		std::size_t block_element = 0,
		std::size_t field_element = 0,
		std::size_t data_offset = 0
	) const;

	/// Returns the value of the specified field as a pointer to pointer
	BlendFilePointerToPointer GetPointerToPointer(
		const BlendFileFlattenedStructField& flat_field,
		std::size_t block_element = 0,
		std::size_t field_element = 0,
		std::size_t data_offset = 0
	) const;

	/// Returns the value at the specified offset as an integer
	template <typename Int>
	Int GetInt(
		std::size_t field_offset,
		std::size_t block_element,
		std::size_t field_element,
		std::size_t data_offset
	) const
	{
		const char* pos =
			_block_data.data() +
			data_offset +
			block_element * _struct_size +
			field_element * sizeof(Int) +
			field_offset;
		return aux::ReorderToNative(
			_byte_order,
			*reinterpret_cast<const Int*>(pos)
		);
	}

	/// Returns the value of the specified field as an integer
	template <typename Int>
	Int GetInt(
		const BlendFileFlattenedStructField& flat_field,
		std::size_t block_element = 0,
		std::size_t field_element = 0,
		std::size_t data_offset = 0
	) const
	{
		assert(sizeof(Int) == flat_field.Field().BaseType().Size());
		return GetInt<Int>(
			flat_field.Offset(),
			block_element,
			field_element,
			data_offset
		);
	}

	/// Returns the value at the specified offset as a floating point value
	template <typename Float>
	Float GetFloat(
		std::size_t field_offset,
		std::size_t block_element,
		std::size_t field_element,
		std::size_t data_offset
	) const
	{
		const char* pos =
			_block_data.data() +
			data_offset +
			block_element * _struct_size +
			field_element * sizeof(Float) +
			field_offset;
		return *reinterpret_cast<const Float*>(pos);
	}

	/// Returns the value of the specified field as a floating point value
	template <typename Float>
	Float GetFloat(
		const BlendFileFlattenedStructField& flat_field,
		std::size_t block_element = 0,
		std::size_t field_element = 0,
		std::size_t data_offset = 0
	) const
	{
		assert(sizeof(Float) == flat_field.Field().BaseType().Size());
		return GetFloat<Float>(
			flat_field.Offset(),
			block_element,
			field_element,
			data_offset
		);
	}

	/// Returns the value at the specified offset as a string
	std::string GetString(
		std::size_t field_size,
		std::size_t field_offset,
		std::size_t block_element,
		std::size_t field_element,
		std::size_t data_offset
	) const;

	/// Returns the value of the specified field as a string
	std::string GetString(
		const BlendFileFlattenedStructField& flat_field,
		std::size_t block_element = 0,
		std::size_t field_element = 0,
		std::size_t data_offset = 0
	) const
	{
		return GetString(
			flat_field.Size(),
			flat_field.Offset(),
			block_element,
			field_element,
			data_offset
		);
	}

	/// Visits the value of the specified field by a visitor
	void ValueVisitRef(
		BlendFileVisitor& visitor,
		const BlendFileFlattenedStructField& flat_field,
		std::size_t block_element = 0,
		std::size_t field_element = 0,
		std::size_t data_offset = 0
	) const;

	template <typename Visitor>
	void ValueVisit(
		Visitor visitor,
		const BlendFileFlattenedStructField& flat_field,
		std::size_t block_element = 0,
		std::size_t field_element = 0
	) const
	{
		ValueVisitRef(
			visitor,
			flat_field,
			block_element,
			field_element
		);
	}
};

} // imports
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/imports/blend_file/block_data.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

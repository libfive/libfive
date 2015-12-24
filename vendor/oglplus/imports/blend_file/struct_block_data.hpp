/**
 *  @file oglplus/imports/blend_file/struct_block_data.hpp
 *  @brief Convenience class for easy manipulation of block data
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMPORTS_BLEND_FILE_STRUCT_BLOCK_DATA_1107121519_HPP
#define OGLPLUS_IMPORTS_BLEND_FILE_STRUCT_BLOCK_DATA_1107121519_HPP

#include <oglplus/utils/type_tag.hpp>
#include <type_traits>

namespace oglplus {
namespace imports {

template <typename T>
class BlendFileFlatStructTypedFieldData;

template <typename T>
class BlendFileFlatStructTypedFieldDataImpl
{
private:
	BlendFileFlattenedStructField _flat_field;
	const BlendFileBlockData* _block_data_ref;
	std::size_t _offset;

	friend class BlendFileFlatStructTypedFieldData<T>;

protected:
	BlendFileFlatStructTypedFieldDataImpl(
		BlendFileFlattenedStructField&& flat_field,
		const BlendFileBlockData& block_data_ref,
		std::size_t offset
	): _flat_field(std::move(flat_field))
	 , _block_data_ref(&block_data_ref)
	 , _offset(offset)
	{ }

	BlendFileFlatStructTypedFieldDataImpl(
		BlendFileFlatStructTypedFieldDataImpl&& tmp
	): _flat_field(std::move(tmp._flat_field))
	 , _block_data_ref(tmp._block_data_ref)
	 , _offset(tmp._offset)
	{ }

	template <unsigned Level>
	BlendFilePointerTpl<Level> _do_get(
		TypeTag<BlendFilePointerTpl<Level>> /*selector*/,
		std::size_t block_element,
		std::size_t field_element
	) const
	{
		return _block_data_ref->template _get_pointer<Level>(
			_flat_field,
			block_element,
			field_element,
			_offset
		);
	}

	BlendFilePointer _do_get(
		TypeTag<void*> /*selector*/,
		std::size_t block_element,
		std::size_t field_element
	) const
	{
		return _block_data_ref->GetPointer(
			_flat_field,
			block_element,
			field_element,
			_offset
		);
	}

	BlendFilePointerToPointer _do_get(
		TypeTag<void**> /*selector*/,
		std::size_t block_element,
		std::size_t field_element
	) const
	{
		return _block_data_ref->GetPointerToPointer(
			_flat_field,
			block_element,
			field_element,
			_offset
		);
	}

	std::string _do_get(
		TypeTag<std::string> /*selector*/,
		std::size_t block_element,
		std::size_t field_element
	) const
	{
		return _block_data_ref->GetString(
			_flat_field,
			block_element,
			field_element,
			_offset
		);
	}

	char _do_get(
		TypeTag<char> /*selector*/,
		std::size_t block_element,
		std::size_t field_element
	) const
	{
		return _block_data_ref->GetInt<char>(
			_flat_field,
			block_element,
			field_element,
			_offset
		);
	}

	float _do_get(
		TypeTag<float> /*selector*/,
		std::size_t block_element,
		std::size_t field_element
	) const
	{
		return _block_data_ref->GetFloat<float>(
			_flat_field,
			block_element,
			field_element,
			_offset
		);
	}

	double _do_get(
		TypeTag<double> /*selector*/,
		std::size_t block_element,
		std::size_t field_element
	) const
	{
		return _block_data_ref->GetFloat<double>(
			_flat_field,
			block_element,
			field_element,
			_offset
		);
	}

	template <typename Int>
	typename std::enable_if<
		std::is_integral<Int>::value,
		Int
	>::type _do_get(
		TypeTag<Int> /*selector*/,
		std::size_t block_element,
		std::size_t field_element
	) const
	{
		return _block_data_ref->GetInt<Int>(
			_flat_field,
			block_element,
			field_element,
			_offset
		);
	}
};

/// Helper class for direct access to a field's data from a specific block
template <typename T>
class BlendFileFlatStructTypedFieldData
 : public BlendFileFlatStructTypedFieldDataImpl<T>
{
private:
	typedef BlendFileFlatStructTypedFieldDataImpl<T> _base;
	static const _base& _that(void);

	friend class BlendFileFlatStructBlockData;

	BlendFileFlatStructTypedFieldData(
		BlendFileFlattenedStructField&& flat_field,
		const BlendFileBlockData& block_data_ref,
		std::size_t offset
	): _base(std::move(flat_field), block_data_ref, offset)
	{ }
public:
	typedef decltype(_that()._do_get(TypeTag<T>(), 0, 0)) _value_type;
	// this is a workaround for MSVC 12
	typedef typename BlendFileFlatStructTypedFieldData<T>::_value_type
		ValueType;

	BlendFileFlatStructTypedFieldData(BlendFileFlatStructTypedFieldData&& tmp)
	 : _base(static_cast<_base&&>(tmp))
	{ }

	/// Get the value of the field from the block
	ValueType Get(
		std::size_t block_element,
		std::size_t field_element
	) const
	{
		return this->_do_get(TypeTag<T>(), block_element, field_element);
	}

	/// Get the first value of the field from the first block
	ValueType Get(void) const
	{
		return Get(0, 0);
	}

	/// Get the value of the field from the block
	operator ValueType (void) const
	{
		return Get(0, 0);
	}
};

/// Convenience class combining functionality of FlattenedStruct, Block and BlockData
class BlendFileFlatStructBlockData
{
private:
	BlendFileFlattenedStruct _flat_struct;
	BlendFileBlock _block;
	BlendFileBlockData _data;
	std::size_t _offset;

	friend class BlendFile;

	BlendFileFlatStructBlockData(
		BlendFileFlattenedStruct&& flat_struct,
		BlendFileBlock&& block,
		BlendFileBlockData&& data,
		std::size_t offset
	): _flat_struct(std::move(flat_struct))
	 , _block(std::move(block))
	 , _data(std::move(data))
	 , _offset(offset)
	{ }

	template <typename T>
	static T _adjust_value(T* ptr)
	{
		assert(ptr);
		return *ptr;
	}

	static BlendFilePointer _adjust_value(void**)
	{
		return BlendFilePointer();
	}

	template <typename T>
	struct _adjust_type
	{
		typedef decltype(_adjust_value(&TypeTag<T>())) type;
	};

public:
	BlendFileFlatStructBlockData(BlendFileFlatStructBlockData&& tmp)
	 : _flat_struct(std::move(tmp._flat_struct))
	 , _block(std::move(tmp._block))
	 , _data(std::move(tmp._data))
	 , _offset(tmp._offset)
	{ }

	/// Returns true if the data is structured
	bool IsStructure(void) const
	{
		return _flat_struct.IsStructure();
	}

	/// Returns the data structure
	const BlendFileFlattenedStruct& Structure(void) const
	{
		return _flat_struct;
	}

	/// The name of the block's structure type
	const std::string& StructureName(void) const
	{
		return Structure().Name();
	}

	/// The size of instance of the structure in bytes
	std::size_t StructureSize(void) const
	{
		return Structure().Size();
	}

	/// Returns a range of fields of the block's flattened structure
	BlendFileFlattenedStructFieldRange StructureFields(void) const
	{
		return Structure().Fields();
	}

	/// Returns a block's structure's field by its full name
	BlendFileFlattenedStructField StructureFieldByName(
		const std::string& field_name
	) const
	{
		return Structure().FieldByName(field_name);
	}

	/// Alias for StructureFieldByName
	BlendFileFlattenedStructField operator / (
		const std::string& field_name
	) const
	{
		return Structure().FieldByName(field_name);
	}

	/// Returns the block
	const BlendFileBlock& Block(void) const
	{
		return _block;
	}

	/// Returns the code of the block
	std::string BlockCode(void) const
	{
		return _block.Code();
	}

	/// Returns the size of the block in bytes
	uint32_t BlockSize(void) const
	{
		return _block.Size();
	}

	/// Returns the number of elements in the block
	uint32_t BlockElementCount(void) const
	{
		return _block.ElementCount();
	}

	/// Returns the 'old' pointer value of the block as loaded from input
	BlendFilePointer BlockPointer(void) const
	{
		return _block.Pointer();
	}

	/// Returns the block data object
	const BlendFileBlockData& BlockData(void) const
	{
		return _data;
	}

	/// Visit the value of the specified field by a visitor
	template <class Visitor>
	void BlockDataValueVisit(
		Visitor visitor,
		const BlendFileFlattenedStructField& flat_field,
		std::size_t block_element = 0,
		std::size_t field_element = 0
	) const
	{
		BlockData().ValueVisit(
			visitor,
			flat_field,
			block_element,
			field_element,
			_offset
		);
	}

	/// Returns a getter object that allows to obtain the value of a field
	template <typename T>
	BlendFileFlatStructTypedFieldData<T> Field(
		const std::string& field_name
	) const
	{
		return BlendFileFlatStructTypedFieldData<T>(
			Structure().FieldByName(field_name),
			BlockData(),
			_offset
		);
	}

	template <typename T>
	typename _adjust_type<T>::type TryGet(
		const std::string& field_name,
		T default_value,
		std::size_t block_element,
		std::size_t field_element
	) const
	{
		try
		{
			return Field<T>(field_name).Get(
				block_element,
				field_element
			);
		}
		catch(...) { }
		return _adjust_value(&default_value);
	}

	template <typename T>
	typename _adjust_type<T>::type TryGet(
		const std::string& field_name,
		T default_value
	)
	{
		return TryGet<T>(field_name, default_value, 0, 0);
	}
};

} // imports
} // oglplus

#endif // include guard

/**
 *  @file oglplus/imports/blend_file/block_data.ipp
 *  @brief Implementation of BlendFileBlockData
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */
#include <oglplus/config/basic.hpp>
#include <oglplus/assert.hpp>

namespace oglplus {
namespace imports {

template <unsigned Level>
BlendFilePointerTpl<Level> BlendFileBlockData::_do_make_pointer(
	const char* pos,
	std::size_t type_index
) const
{
	if(_ptr_size == 4)
		return BlendFilePointerTpl<Level>(aux::ReorderToNative(
			_byte_order,
			*reinterpret_cast<const uint32_t*>(pos)
		), type_index);
	if(_ptr_size == 8)
		return BlendFilePointerTpl<Level>(aux::ReorderToNative(
			_byte_order,
			*reinterpret_cast<const uint64_t*>(pos)
		), type_index);
	OGLPLUS_ABORT("Invalid pointer size!");
	return BlendFilePointerTpl<Level>();
}

template <unsigned Level>
BlendFilePointerTpl<Level> BlendFileBlockData::_do_get_pointer(
	std::size_t type_index,
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
		field_element * _ptr_size +
		field_offset;
	return _do_make_pointer<Level>(pos, type_index);
}

template <unsigned Level>
BlendFilePointerTpl<Level> BlendFileBlockData::_get_pointer(
	const BlendFileFlattenedStructField& flat_field,
	std::size_t block_element,
	std::size_t field_element,
	std::size_t data_offset
) const
{
	return _do_get_pointer<Level>(
		flat_field._sdna->_structs[
			flat_field._flat_fields->_field_structs[
				flat_field._flat_field_index
			]
		]._field_type_indices[
			flat_field._flat_fields->_field_indices[
				flat_field._flat_field_index
			]
		],
		flat_field.Offset(),
		block_element,
		field_element,
		data_offset
	);
}

OGLPLUS_LIB_FUNC
BlendFilePointer BlendFileBlockData::AsPointerTo(
	const BlendFileType& type,
	std::size_t index,
	std::size_t data_offset
) const
{
	const char* pos =
		_block_data.data() +
		data_offset +
		index * _ptr_size;
	return _do_make_pointer<1>(pos, type._type_index);
}

OGLPLUS_LIB_FUNC
BlendFilePointer BlendFileBlockData::GetPointer(
	const BlendFileFlattenedStructField& flat_field,
	std::size_t block_element,
	std::size_t field_element,
	std::size_t data_offset
) const
{
	return _get_pointer<1>(
		flat_field,
		block_element,
		field_element,
		data_offset
	);
}

OGLPLUS_LIB_FUNC
BlendFilePointerToPointer BlendFileBlockData::GetPointerToPointer(
	const BlendFileFlattenedStructField& flat_field,
	std::size_t block_element,
	std::size_t field_element,
	std::size_t data_offset
) const
{
	return _get_pointer<2>(
		flat_field,
		block_element,
		field_element,
		data_offset
	);
}

OGLPLUS_LIB_FUNC
std::string BlendFileBlockData::GetString(
	std::size_t field_size,
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
		field_element * field_size +
		field_offset;
	return std::string(pos, field_size);
}

OGLPLUS_LIB_FUNC
void BlendFileBlockData::ValueVisitRef(
	BlendFileVisitor& visitor,
	const BlendFileFlattenedStructField& flat_field,
	std::size_t block_element,
	std::size_t field_element,
	std::size_t data_offset
) const
{
	auto f = flat_field.Field();
	if(f.IsPointer())
	{
		auto bt = f.BaseType();
		if(bt.IsNative<char>())
		{
			visitor.VisitStr(GetString(
				flat_field,
				block_element,
				field_element,
				data_offset
			));
		}
		else
		{
			visitor.VisitPtr(GetPointer(
				flat_field,
				block_element,
				field_element,
				data_offset
			));
		}
	}
	else if(f.IsPointerToPointer())
	{
		visitor.VisitPPtr(GetPointerToPointer(
			flat_field,
			block_element,
			field_element,
			data_offset
		));
	}
	else
	{
		auto bt = f.BaseType();
		if(bt.IsNative<char>())
		{
			if(f.IsArray())
			{
				visitor.VisitStr(GetString(
					flat_field,
					block_element,
					field_element,
					data_offset
				));
			}
			else
			{
				visitor.VisitChr(GetInt<char>(
					flat_field,
					block_element,
					field_element,
					data_offset
				));
			}
		}
		else if(bt.IsNative<uint8_t>())
		{
			visitor.VisitU8(GetInt<uint8_t>(
				flat_field,
				block_element,
				field_element,
				data_offset
			));
		}
		else if(bt.IsNative<int8_t>())
		{
			visitor.VisitI8(GetInt<int8_t>(
				flat_field,
				block_element,
				field_element,
				data_offset
			));
		}
		else if(bt.IsNative<uint16_t>())
		{
			visitor.VisitU16(GetInt<uint16_t>(
				flat_field,
				block_element,
				field_element,
				data_offset
			));
		}
		else if(bt.IsNative<int16_t>())
		{
			visitor.VisitI16(GetInt<int16_t>(
				flat_field,
				block_element,
				field_element,
				data_offset
			));
		}
		else if(bt.IsNative<uint32_t>())
		{
			visitor.VisitU32(GetInt<uint32_t>(
				flat_field,
				block_element,
				field_element,
				data_offset
			));
		}
		else if(bt.IsNative<int32_t>())
		{
			visitor.VisitI32(GetInt<int32_t>(
				flat_field,
				block_element,
				field_element,
				data_offset
			));
		}
		else if(bt.IsNative<float>())
		{
			visitor.VisitFlt(GetFloat<float>(
				flat_field,
				block_element,
				field_element,
				data_offset
			));
		}
		else if(bt.IsNative<double>())
		{
			visitor.VisitDbl(GetFloat<double>(
				flat_field,
				block_element,
				field_element,
				data_offset
			));
		}
		else
		{
			visitor.VisitRaw(
				_block_data.data() +
				data_offset +
				block_element * _struct_size +
				flat_field.Offset(),
				flat_field.Size()
			);
		}
	}
}

} // imports
} // oglplus


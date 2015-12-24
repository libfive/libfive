/**
 *  @file oglplus/imports/blend_file/flattened.ipp
 *  @brief Implementation of BlendFileFlattenedStruct
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */
#include <oglplus/config/basic.hpp>

namespace oglplus {
namespace imports {

OGLPLUS_LIB_FUNC
const std::string& BlendFileFlattenedStructField::Name(void) const
{
	return _flat_fields->_field_names[_flat_field_index];
}

OGLPLUS_LIB_FUNC
BlendFileStruct BlendFileFlattenedStructField::Parent(void) const
{
	return BlendFileStruct(
		_sdna,
		_flat_fields->_field_structs[_flat_field_index]
	);
}

OGLPLUS_LIB_FUNC
BlendFileStructField BlendFileFlattenedStructField::Field(void) const
{
	return BlendFileStructField(
		_sdna,
		_flat_fields->_field_structs[_flat_field_index],
		_flat_fields->_field_indices[_flat_field_index]
	);
}

OGLPLUS_LIB_FUNC
uint32_t BlendFileFlattenedStructField::Offset(void) const
{
	return _flat_fields->_field_offsets[_flat_field_index];
}

OGLPLUS_LIB_FUNC
BlendFileFlattenedStructFieldRange
BlendFileFlattenedStruct::Fields(void) const
{
	const BlendFileSDNA::_flat_struct_info* flat_fields =
		(_struct_index == _sdna->_invalid_struct_index())?
		static_cast<const BlendFileSDNA::_flat_struct_info*>(nullptr):
		(_sdna->_struct_flatten_fields(_struct_index).get());

	return BlendFileFlattenedStructFieldRange(
		_sdna,
		_struct_index,
		flat_fields
	);
}

OGLPLUS_LIB_FUNC
BlendFileFlattenedStructField
BlendFileFlattenedStruct::FieldByName(const std::string& name) const
{
	// this of course does not work for atomic types
	if(_struct_index == _sdna->_invalid_struct_index())
	{
		std::string what("Requesting field '");
		what.append(name);
		what.append("' in an atomic type");
		throw std::runtime_error(what);
	}

	const BlendFileSDNA::_flat_struct_info* flat_fields =
		_sdna->_struct_flatten_fields(_struct_index).get();
	assert(flat_fields);

	auto pos = flat_fields->_field_map.find(&name);

	if(pos == flat_fields->_field_map.end())
	{
		std::string what("Cannot find field '");
		what.append(name);
		what.append("' in flattened structure");
		throw std::runtime_error(what);
	}

	const std::size_t flat_field_index = pos->second;

	return BlendFileFlattenedStructField(
		_sdna,
		_struct_index,
		flat_field_index,
		flat_fields
	);
}

} // imports
} // oglplus


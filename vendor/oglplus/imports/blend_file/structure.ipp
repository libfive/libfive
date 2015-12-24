/**
 *  @file oglplus/imports/blend_file/structure.ipp
 *  @brief Implementation of BlendFileStruct
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */
#include <oglplus/config/basic.hpp>

namespace oglplus {
namespace imports {

OGLPLUS_LIB_FUNC
BlendFileType BlendFileStructField::BaseType(void) const
{
	auto type_index = _sdna->_structs[_struct_index].
			_field_type_indices[_field_index];
	return BlendFileType(
		_sdna,
		type_index,
		_sdna->_type_structs[type_index]
	);
}

OGLPLUS_LIB_FUNC
const std::string& BlendFileStructField::Definition(void) const
{
	return _sdna->_names[
		_sdna->_structs[_struct_index].
		_field_name_indices[_field_index]
	];
}

OGLPLUS_LIB_FUNC
std::string BlendFileStructField::Name(void) const
{
	return _sdna->_field_name_from_def(Definition());
}

OGLPLUS_LIB_FUNC
bool BlendFileStructField::IsPointer(void) const
{
	bool is_ptr = _sdna->_structs[_struct_index].
		_field_ptr_flags[_field_index];
	bool is_ptr2 = _sdna->_structs[_struct_index].
		_field_ptr2_flags[_field_index];
	return is_ptr && !is_ptr2;
}

OGLPLUS_LIB_FUNC
bool BlendFileStructField::IsPointerToPointer(void) const
{
	bool is_ptr = _sdna->_structs[_struct_index].
		_field_ptr_flags[_field_index];
	bool is_ptr2 = _sdna->_structs[_struct_index].
		_field_ptr2_flags[_field_index];
	return is_ptr && is_ptr2;
}

OGLPLUS_LIB_FUNC
bool BlendFileStructField::IsPointerToFunc(void) const
{
	bool is_ptr = _sdna->_structs[_struct_index].
		_field_ptr_flags[_field_index];
	bool is_ptr2 = _sdna->_structs[_struct_index].
		_field_ptr2_flags[_field_index];
	return !is_ptr && is_ptr2;
}

OGLPLUS_LIB_FUNC
bool BlendFileStructField::IsArray(void) const
{
	return	_sdna->_structs[_struct_index].
		_field_array_flags[_field_index];
}

OGLPLUS_LIB_FUNC
std::size_t BlendFileStructField::ElementCount(void) const
{
	return	_sdna->_structs[_struct_index].
		_field_elem_counts[_field_index];
}

OGLPLUS_LIB_FUNC
std::size_t BlendFileStructField::Size(void) const
{
	bool is_ptr = IsPointer() || IsPointerToFunc();

	if(IsArray())
	{
		std::size_t ec = ElementCount();
		if(is_ptr) return _sdna->_ptr_size * ec;
		else return BaseType().Size() * ec;
	}
	if(is_ptr) return _sdna->_ptr_size;
	return BaseType().Size();
}

OGLPLUS_LIB_FUNC
std::size_t BlendFileStructFieldRange::_field_count(
	BlendFileSDNA* sdna,
	std::size_t struct_index
)
{
	if(struct_index == sdna->_invalid_struct_index()) return 0;
	else return sdna->_structs[struct_index]._field_count();
}

} // imports
} // oglplus


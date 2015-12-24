/**
 *  @file oglplus/imports/blend_file/sdna.hpp
 *  @brief Helper class wrapping the SDNA structure of a .blend file
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMPORTS_BLEND_FILE_SDNA_1107121519_HPP
#define OGLPLUS_IMPORTS_BLEND_FILE_SDNA_1107121519_HPP

#include <oglplus/utils/type_tag.hpp>
#include <vector>
#include <map>
#include <memory>

namespace oglplus {
namespace imports {

// Helper class storing the SDNA structure of a .blend file
// NOTE: implementation detail do not use directly
class BlendFileSDNA
 : public BlendFileReaderClient
 , public BlendFileUtils
{
private:
	BlendFileSDNA(const BlendFileSDNA&);

	// list of structure field names as loaded from the file's SDNA
	std::vector<std::string> _names;

	// list of type names as loaded from the the file's SDNA
	std::vector<std::string> _type_names;

	// list of type sizes as loaded from the the file's SDNA
	std::vector<uint16_t> _type_sizes;

	// returns an invalid type index value
	std::size_t _invalid_type_index(void) const
	{
		return _type_sizes.size();
	}

	// 'map' of types to indices of structures stored in _structs
	// if the i-th type is atomic then the value stored here
	// is _invalid_struct_index(), if it is a structured type then the
	// value stored here is an index into the _structs list
	std::vector<uint32_t> _type_structs;

	// checks if a type with the specified index is a structure
	bool _type_is_struct(uint32_t type) const
	{
		return _type_structs[type] != _invalid_struct_index();
	}

	// helper comparator
	struct _pstr_less
	{
		typedef const std::string* ptr;
		bool operator()(ptr a, ptr b) const
		{
			assert(a && b);
			return *a < *b;
		}
	};

	// stores the data describing a flattened structure.
	// flattening is a process of unfolding of the structure
	// so that it contains only atomic fields. if a structure's
	// field is another structure then this field is replaced
	// here by the list of the fields of that (also recursivelly
	// flattened) structure.
	struct _flat_struct_info
	{
	private:
		// non copyable
		_flat_struct_info(const _flat_struct_info&);
	public:
		// flat field information
		// all vectors below have the same number of elements
		// and each stores a different property of the atomic
		// fields
		//
		// the full field names
		std::vector<std::string> _field_names;
		//
		// the indices of structures where the field is defined
		// if the field is defined in a nested structure
		// then the index of this structure is stored here
		// instead of the index of the original flattened structure
		std::vector<uint16_t> _field_structs;
		//
		// the indices inside of structures where field is defined
		std::vector<uint16_t> _field_indices;
		//
		// the offset of the fields in the flattened structure
		std::vector<uint32_t> _field_offsets;

		std::map<const std::string*, std::size_t, _pstr_less> _field_map;

		_flat_struct_info(std::size_t field_count)
		 : _field_names(field_count)
		 , _field_structs(field_count)
		 , _field_indices(field_count)
		 , _field_offsets(field_count)
		{ }

		// returns the number of fields in the flattened structure
		std::size_t _field_count(void) const
		{
			assert(
				_field_names.size() ==
				_field_structs.size()
			);
			assert(
				_field_names.size() ==
				_field_indices.size()
			);
			assert(
				_field_names.size() ==
				_field_offsets.size()
			);
			return _field_names.size();
		}
	};

	// stores the description of a single structured type
	struct _struct_info
	{
		// index to _type_names and _type_sized
		uint16_t _type_index;

		// field properties:
		// all vectors below have the same size -
		// the number of fields in a structure
		// and each stores a different property
		// of the individual fields
		//
		// indices to sdna::_type_indices
		std::vector<uint16_t> _field_type_indices;
		// indices to sdna::_name_indices
		std::vector<uint16_t> _field_name_indices;
		// stores the number of elements in the fields
		std::vector<uint16_t> _field_elem_counts;
		//
		// stores values indicating if the i-th
		//field is a pointer
		std::vector<bool> _field_ptr_flags;
		//
		// stores values indicating if the i-th
		// field is a pointer to a function
		// or if combined with _field_ptr_flags[i]
		// it means that it is a pointer to pointer
		std::vector<bool> _field_ptr2_flags;
		//
		// stores values indication if the i-th
		// field is an array with multiple values
		std::vector<bool> _field_array_flags;

		// pointer to a _flat_struct_info storing information
		// about the structure after flattening
		// this pointer is initialized on demand
		// by sdna's functions implemented below
		std::shared_ptr<_flat_struct_info> _flat_fields;

		// returns the number of fields in the structure
		std::size_t _field_count(void) const
		{
			assert(
				_field_type_indices.size() ==
				_field_name_indices.size()
			);
			assert(
				_field_type_indices.size() ==
				_field_elem_counts.size()
			);
			assert(
				_field_type_indices.size() ==
				_field_ptr_flags.size()
			);
			assert(
				_field_type_indices.size() ==
				_field_ptr2_flags.size()
			);
			assert(
				_field_type_indices.size() ==
				_field_array_flags.size()
			);
			return _field_type_indices.size();
		}
	};

	// helper function of testing whether a field specified
	// by a definition def is a pointer
	static bool _field_is_ptr(const std::string& def)
	{
		if(def.empty()) return false;
		return def[0] == '*';
	}

	// helper function of testing whether a field specified
	// by a definition def is a pointer to pointer
	static bool _field_is_ptr_to_ptr(const std::string& def)
	{
		if(def.size() < 2) return false;
		return (def[0] == '*') && (def[1] == '*');
	}

	// helper function of testing whether a field specified
	// by a definition def is a pointer to function
	static bool _field_is_fn_ptr(const std::string& def)
	{
		if(def.empty()) return false;
		return def.back() == ')';
	}

	// helper function of testing whether a field specified
	// by a definition def is an array
	static bool _field_is_array(const std::string& def)
	{
		if(def.empty()) return false;
		return def.back() == ']';
	}

	// helper function that parses a field definition string
	// and returns the number of elements.
	// returns 1 for non-arrays or the number of elements
	// in case of an (potentially multi-dimensional) array
	static std::size_t _field_elem_count(const std::string& def);

	// stores the information about the structures
	// in the SDNA block
	std::vector<_struct_info> _structs;

	std::size_t _invalid_struct_index(void) const
	{
		return _structs.size();
	}

	// maps type name to index in _type_names and _type_sizes
	// used for lookup of type properties by name
	std::map<const std::string*, std::size_t, _pstr_less> _type_map;

	// returns true if c is a valid character
	// for a structure field name identifier
	static bool _is_field_name_char(char c)
	{
		return	(c >= 'A' && c <= 'Z') ||
			(c >= 'a' && c <= 'z') ||
			(c >= '0' && c <= '9') ||
			(c == '_');
	}

	static std::string _field_name_from_def(std::string result);

	static std::string _elem_field_suffix(uint64_t i);

	// returns the number of fields in a flattened structure
	// i.e. returns the number of the atomic fields
	// the structure is specified by its index in _structs
	std::size_t _struct_flat_field_count(uint32_t struct_index);

	const std::shared_ptr<_flat_struct_info>&
	_struct_flatten_fields(std::size_t struct_index);

	// a sequence for assigning unique integer identifiers to
	// C++ types
	static std::size_t& _type_id_seq(void)
	{
		static std::size_t seq = 0;
		return seq;
	}

	// returns an unique integer identifier for type T
	template <typename T>
	static std::size_t _type_id(TypeTag<T>)
	{
		static std::size_t tid = _type_id_seq()++;
		return tid;
	}

	// maps type identifier to a sdna type index
	std::vector<std::size_t> _type_id_to_type_index;

	template <typename T>
	void _init_type_id_index(const std::string& type_name)
	{
		// get the type idntifier for type T
		std::size_t tid = _type_id(TypeTag<T>());
		// check if the type is already registered
		if(_type_id_to_type_index.size() <= tid)
		{
			_type_id_to_type_index.resize(
				tid+4,
				_invalid_type_index()
			);
		}

		auto pos = _type_map.find(&type_name);
		if(pos == _type_map.end())
			_type_id_to_type_index[tid] = _invalid_type_index();
		else _type_id_to_type_index[tid] = pos->second;
	}

	template <typename T>
	std::size_t _find_type_index(void) const
	{
		std::size_t tid = _type_id(TypeTag<T>());
		if(_type_id_to_type_index.size() > tid)
			return _type_id_to_type_index[tid];
		else return _invalid_type_index();
	}

	template <typename T>
	bool _is_known_type(void) const
	{
		return _find_type_index<T>() != _invalid_type_index();
	}

	template <typename T>
	bool _type_matches(std::size_t type_index) const
	{
		return _find_type_index<T>() == type_index;
	}

	// this class is used internally by a whole heap
	// of other classes
	friend class BlendFile;
	friend class BlendFileType;
	friend class BlendFileStruct;
	friend class BlendFileStructRange;
	friend class BlendFileStructField;
	friend class BlendFileStructFieldRange;
	friend class BlendFileFlattenedStruct;
	friend class BlendFileFlattenedStructField;
	friend class BlendFileFlattenedStructFieldRange;
	friend class BlendFileBlockData;
	friend class BlendFileFlatStructBlockData;

public:
	// constructor uses a reader and info to parse the SDNA
	// block from the input stream
	BlendFileSDNA(BlendFileReader& bfr, const BlendFileInfo& bfi);
};

} // imports
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/imports/blend_file/sdna.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

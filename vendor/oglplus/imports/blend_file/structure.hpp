/**
 *  @file oglplus/imports/blend_file/structure.hpp
 *  @brief Class providing information about a structure defined in a .blend file
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2013 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMPORTS_BLEND_FILE_STRUCTURE_1107121519_HPP
#define OGLPLUS_IMPORTS_BLEND_FILE_STRUCTURE_1107121519_HPP

namespace oglplus {
namespace imports {

class BlendFileFlattenedStruct;

/// Provides information about a field of a blender structure
class BlendFileStructField
{
private:
	BlendFileSDNA* _sdna;
	std::size_t _struct_index;
	std::size_t _field_index;

	friend class BlendFileStructFieldRange;
	friend class BlendFileFlattenedStructField;

	BlendFileStructField(
		BlendFileSDNA* sdna,
		std::size_t struct_index,
		std::size_t field_index
	): _sdna(sdna)
	 , _struct_index(struct_index)
	 , _field_index(field_index)
	{
		assert(_sdna);
	}
public:
	/// Returns the base type of the field
	/**
	 *  @note The actual type may be a pointer to the base
	 *  type or an array of multiple elements of the base type.
	 *
	 *  @see IsPointer
	 *  @see IsPointerToFunc
	 *  @see IsArray
	 *  @see ElementCount
 	 */
	BlendFileType BaseType(void) const;

	/// Returns the defintion string of the structure field
	const std::string& Definition(void) const;

	/// Returns the name of the structure field
	std::string Name(void) const;

	/// Returns true if the field is a regular pointer
	bool IsPointer(void) const;

	/// Returns true if the field is a pointer to a pointer
	bool IsPointerToPointer(void) const;

	/// Returns true if the field is a pointer to a function
	bool IsPointerToFunc(void) const;

	/// Returns true if the field is an array
	bool IsArray(void) const;

	/// Returns the number of elements in case of an array, 1 otherwise
	std::size_t ElementCount(void) const;

	/// Returns the size of the field in bytes
	std::size_t Size(void) const;
};


/// Class allowing the traversal of structure's fields
class BlendFileStructFieldRange
 : public BlendFileRangeTpl<BlendFileStructFieldRange, BlendFileStructField>
{
private:
	BlendFileSDNA* _sdna;
	std::size_t _struct_index;

	static std::size_t _field_count(
		BlendFileSDNA* sdna,
		std::size_t struct_index
	);

	typedef BlendFileRangeTpl<
		BlendFileStructFieldRange,
		BlendFileStructField
	> Base;

	friend class BlendFileStruct;

	BlendFileStructFieldRange(
		BlendFileSDNA* sdna,
		std::size_t struct_index
	): Base(_field_count(sdna, struct_index))
	 , _sdna(sdna)
	 , _struct_index(struct_index)
	{ }
public:
	BlendFileStructField Get(std::size_t index) const
	{
		return BlendFileStructField(
			_sdna,
			_struct_index,
			index
		);
	}
};

/// Wrapper around Blender structure meta-data
class BlendFileStruct
 : public BlendFileType
{
private:

	friend class BlendFile;
	friend class BlendFileStructRange;
	friend class BlendFileStructField;
	friend class BlendFileFlattenedStructField;

	BlendFileStruct(
		BlendFileSDNA* sdna,
		std::size_t struct_index
	): BlendFileType(
		sdna,
		sdna->_structs[struct_index]._type_index,
		struct_index
	)
	{ }

public:
	BlendFileStruct(const BlendFileType& type)
	 : BlendFileType(type)
	{ }

	/// Returns a range of fields of structure
	BlendFileStructFieldRange Fields(void) const
	{
		return BlendFileStructFieldRange(
			_sdna,
			_struct_index
		);
	}

	inline BlendFileFlattenedStruct Flattened(void) const;
};

BlendFileStruct BlendFileType::AsStructure(void) const
{
	return BlendFileStruct(*this);
}

/// Allows the traversal of a range of Blender structures
class BlendFileStructRange
 : public BlendFileRangeTpl<BlendFileStructRange, BlendFileStruct>
{
private:
	BlendFileSDNA* _sdna;

	typedef BlendFileRangeTpl<BlendFileStructRange, BlendFileStruct> Base;

	friend class BlendFile;

	BlendFileStructRange(BlendFileSDNA* sdna)
	 : Base(sdna->_structs.size())
	 , _sdna(sdna)
	{ }
public:
	BlendFileStruct Get(std::size_t index) const
	{
		return BlendFileStruct(_sdna, index);
	}
};

} // imports
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/imports/blend_file/structure.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

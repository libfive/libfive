/**
 *  @file oglplus/imports/blend_file/type.hpp
 *  @brief Class providing information about a type defined in a .blend file
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2013 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMPORTS_BLEND_FILE_TYPE_1107121519_HPP
#define OGLPLUS_IMPORTS_BLEND_FILE_TYPE_1107121519_HPP

namespace oglplus {
namespace imports {

class BlendFileStruct;

/// Provides information about a type defined in a .blend file
class BlendFileType
{
protected:
	BlendFileSDNA* _sdna;
	const std::size_t _type_index;
	const std::size_t _struct_index;

	friend class BlendFile;
	friend class BlendFileBlockData;
	friend class BlendFileStructField;
	friend class BlendFileFlatStructBlockData;

	BlendFileType(
		BlendFileSDNA* sdna,
		std::size_t type_index,
		std::size_t struct_index
	): _sdna(sdna)
	 , _type_index(type_index)
	 , _struct_index(struct_index)
	{
		assert(_sdna);
	}
public:
	/// The name of the type
	const std::string& Name(void) const
	{
		return _sdna->_type_names[_type_index];
	}

	/// The size of instance of the type in bytes
	std::size_t Size(void) const
	{
		return _sdna->_type_sizes[_type_index];
	}

	/// Returns true if the Blender type matches the type T
	template <typename T>
	bool IsNative(void) const
	{
		return _sdna->_type_matches<T>(_type_index);
	}

	/// Returns true if the type is structured
	bool IsStructure(void) const
	{
		return _struct_index != _sdna->_invalid_struct_index();
	}

	/// returns the structure of the type
	/**
	 *  @note this function must not be called of IsStructure()
	 *  return false.
	 */
	inline BlendFileStruct AsStructure(void) const;
};

} // imports
} // oglplus

#endif // include guard

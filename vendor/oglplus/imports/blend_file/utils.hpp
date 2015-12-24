/**
 *  @file oglplus/imports/blend_file/utils.hpp
 *  @brief Helper classes used implementing .blend-file-related operations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMPORTS_BLEND_FILE_UTILS_1107121519_HPP
#define OGLPLUS_IMPORTS_BLEND_FILE_UTILS_1107121519_HPP

#include <cstddef>

namespace oglplus {
namespace imports {

// Internal helper class used for various .blend file-related operations
// NOTE: implementation detail, do not use
class BlendFileUtils
{
protected:
	std::size_t _ptr_size;

	BlendFileUtils(std::size_t ptr_size)
	 : _ptr_size(ptr_size)
	{ }

	std::streamoff _align_diff(std::streamoff offset, std::size_t size)
	{
		if(size > _ptr_size)
		{
			size = _ptr_size;
		}
		return offset % std::streamoff(size);
	}

	std::size_t _align_diff(std::size_t offset, std::size_t size)
	{
		if(size > _ptr_size)
		{
			size = _ptr_size;
		}
		return offset % size;
	}

	template <typename Int>
	Int _do_align_offset(Int offset, std::size_t size)
	{
		Int diff = _align_diff(offset, size);
		if(diff != 0)
		{
			offset += size - diff;
		}
		return offset;
	}
};

} // imports
} // oglplus

#endif // include guard

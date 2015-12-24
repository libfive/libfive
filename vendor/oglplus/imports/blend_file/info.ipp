/**
 *  @file oglplus/imports/blend_file/info.ipp
 *  @brief Implementation of BlendFileInfo
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

OGLPLUS_LIB_FUNC
bool BlendFileInfo::_read_header(BlendFileReader& bfr)
{
	return _expect(
		bfr,
		"BLENDER", 7,
		"Failed to read header"
	);
}

OGLPLUS_LIB_FUNC
std::size_t BlendFileInfo::_read_pointer_size(BlendFileReader& bfr)
{
	char c = _expect_one_of(
		bfr,
		"_-", 2,
		"Failed to read pointer size"
	);
	std::size_t ptr_size = 0;
	if(c == '_') ptr_size = 4;
	if(c == '-') ptr_size = 8;
	_adjust_ptr_size(bfr, ptr_size);
	assert(ptr_size);
	return ptr_size;
}

OGLPLUS_LIB_FUNC
Endian BlendFileInfo::_read_endianness(BlendFileReader& bfr)
{
	char c = _expect_one_of(
		bfr,
		"vV", 2,
		"Failed to read endianness"
	);
	if(c == 'v') return Endian::Little;
	if(c == 'V') return Endian::Big;
	OGLPLUS_ABORT("Logic error!");
	return Endian();
}

OGLPLUS_LIB_FUNC
int BlendFileInfo::_read_version(BlendFileReader& bfr)
{
	char buffer[4];
	_read(bfr, buffer, 3, "Failed to read version");
	return	(buffer[0]-'0')*100 +
		(buffer[1]-'0')*10 +
		(buffer[2]-'0')*1;
}

} // imports
} // oglplus


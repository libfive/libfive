/**
 *  @file oglplus/imports/blend_file/block.ipp
 *  @brief Implementation of BlendFileBlock
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
BlendFileBlock::BlendFileBlock(
	BlendFileReader& bfr,
	const BlendFileInfo& bfi,
	std::array<char, 4>&& code,
	bool do_skip
): _code(std::move(code))
 , _size(_read_size(bfr, bfi))
 , _old_ptr(_read_old_ptr(bfr, bfi))
 , _sdna_index(_read_index(bfr, bfi))
 , _count(_read_count(bfr, bfi))
 , _data_pos(_position(bfr))
{
	if(do_skip) _skip(bfr, _size, "Error skipping file block data");
}


OGLPLUS_LIB_FUNC
uint32_t BlendFileBlock::_read_size(
	BlendFileReader& bfr,
	const BlendFileInfo& bfi
)
{
	return _read_int<uint32_t>(
		bfr,
		bfi.ByteOrder(),
		"Failed to read file block size"
	);
}

OGLPLUS_LIB_FUNC
uint64_t BlendFileBlock::_read_old_ptr(
	BlendFileReader& bfr,
	const BlendFileInfo& bfi
)
{
	if(bfi.PointerSize() == 4)
		return _read_int<uint32_t>(
			bfr,
			bfi.ByteOrder(),
			"Failed to read file block old pointer"
		);
	else if(bfi.PointerSize() == 8)
		return _read_int<uint64_t>(
			bfr,
			bfi.ByteOrder(),
			"Failed to read file block old pointer"
		);
	else OGLPLUS_ABORT("Logic error!");
	return 0;
}

OGLPLUS_LIB_FUNC
uint32_t BlendFileBlock::_read_index(
	BlendFileReader& bfr,
	const BlendFileInfo& bfi
)
{
	return _read_int<uint32_t>(
		bfr,
		bfi.ByteOrder(),
		"Failed to read file block SDNA index"
	);
}

OGLPLUS_LIB_FUNC
uint32_t BlendFileBlock::_read_count(
	BlendFileReader& bfr,
	const BlendFileInfo& bfi
)
{
	return _read_int<uint32_t>(
		bfr,
		bfi.ByteOrder(),
		"Failed to read file block object count"
	);
}

} // imports
} // oglplus


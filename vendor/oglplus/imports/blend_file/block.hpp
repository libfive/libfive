/**
 *  @file oglplus/imports/blend_file/block.hpp
 *  @brief Helper class providing information about a .blend file block
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2013 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMPORTS_BLEND_FILE_BLOCK_1107121519_HPP
#define OGLPLUS_IMPORTS_BLEND_FILE_BLOCK_1107121519_HPP

#include <oglplus/imports/blend_file/range.hpp>

namespace oglplus {
namespace imports {

class BlendFile;

/// Class for access to a single .blend file block
class BlendFileBlock
 : public BlendFileReaderClient
{
private:
	std::array<char, 4> _code;

	uint32_t _size;
	uint32_t _read_size(BlendFileReader& bfr, const BlendFileInfo& bfi);

	uint64_t _old_ptr;
	uint64_t _read_old_ptr(BlendFileReader& bfr, const BlendFileInfo& bfi);

	uint32_t _sdna_index;
	uint32_t _read_index(BlendFileReader& bfr, const BlendFileInfo& bfi);

	uint32_t _count;
	uint32_t _read_count(BlendFileReader& bfr, const BlendFileInfo& bfi);

	std::streampos _data_pos;

	friend class BlendFile;
public:
	BlendFileBlock(
		BlendFileReader& bfr,
		const BlendFileInfo& bfi,
		std::array<char, 4>&& code,
		bool do_skip
	);

	/// Returns the code of the block
	std::string Code(void) const
	{
		return std::string(_code.data(), _code.size());
	}

	/// Returns the size of the block in bytes
	uint32_t Size(void) const
	{
		return _size;
	}

	/// Returns the number of elements in the block
	uint32_t ElementCount(void) const
	{
		return _count;
	}

	/// Returns the 'old' pointer value of the block as loaded from input
	BlendFilePointer Pointer(void) const
	{
		return BlendFilePointer(_old_ptr, _sdna_index);
	}

	/// Returns the the position of the block data in the input file
	std::streampos DataPosition(void) const
	{
		return _data_pos;
	}
};

/// Class allowing the traversal of a range of blend file blocks
class BlendFileBlockRange
 : public BlendFileRangeTpl<BlendFileBlockRange, const BlendFileBlock&>
{
private:
	const std::vector<BlendFileBlock>& _blocks;

	typedef BlendFileRangeTpl<BlendFileBlockRange, const BlendFileBlock&> Base;

	BlendFileBlockRange(const std::vector<BlendFileBlock>& blocks)
	 : Base(blocks.size())
	 , _blocks(blocks)
	{ }

	friend class BlendFile;
public:
	const BlendFileBlock& Get(std::size_t index) const
	{
		return _blocks[index];
	}
};

} // imports
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/imports/blend_file/block.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

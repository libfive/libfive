/**
 *  @file oglplus/imports/blend_file.ipp
 *  @brief Implementation of BlendFile
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */
#include <oglplus/config/basic.hpp>
#include <cassert>

namespace oglplus {
namespace imports {

OGLPLUS_LIB_FUNC
BlendFile::BlendFile(std::istream& input)
 : _reader(input)
 , _info(_reader)
 , _glob_block_index(std::size_t(-1))
{
	std::size_t block_idx = 0;
	while(!_eof(_reader))
	{
		std::array<char,4> code  = _read_array<4>(
			_reader,
			"Failed to read file block code"
		);
		if(_equal(code, "GLOB"))
			_glob_block_index = block_idx;

		if(_equal(code, "DNA1"))
		{
			_blocks.push_back(
				BlendFileBlock(
					_reader,
					_info,
					std::move(code),
					false
				)
			);
			_sdna = std::make_shared<BlendFileSDNA>(
				_reader,
				_info
			);
		}
		else
		{
			_blocks.push_back(
				BlendFileBlock(
					_reader,
					_info,
					std::move(code),
					true
				)
			);
		}
		_block_map[_blocks.back()._old_ptr] = block_idx++;
	}
	if(_glob_block_index == std::size_t(-1))
	{
		throw std::runtime_error("Blend file does not contain GLOB block");
	}
	if(!_sdna)
	{
		throw std::runtime_error("Blend file does not contain SDNA block");
	}
}

OGLPLUS_LIB_FUNC
const BlendFileBlock& BlendFile::BlockByPointer(
	BlendFilePointerBase pointer,
	bool allow_offset
) const
{
	auto ptr = pointer.Value();
	auto pos = _block_map.find(ptr);
	if(allow_offset && (pos == _block_map.end()))
	{
		auto pos2 = _block_map.lower_bound(ptr);
		if(pos2 != _block_map.end())
		{
			if(pos2 != _block_map.begin())
			{
				--pos2;
				assert(pos2->first < ptr);
				assert(pos2->second < _blocks.size());
				std::size_t size = _blocks[pos2->second].Size();
				if(ptr - pos2->first < size) pos = pos2;
			}
		}
	}
	if(pos == _block_map.end())
	{
		throw std::runtime_error(
			"Unable to find block by pointer"
		);
	}
	assert(pos->second < _blocks.size());

	return _blocks[pos->second];
}

OGLPLUS_LIB_FUNC
BlendFileFlatStructBlockData BlendFile::StructuredBlockByPointer(
	BlendFilePointer pointer,
	bool allow_offset,
	bool use_pointee_struct
)
{
	auto block = BlockByPointer(pointer, allow_offset);
	auto offset = pointer - block.Pointer();
	auto block_data = BlockData(block);
	auto flat_struct =
		(use_pointee_struct)?
		Pointee(pointer).AsStructure().Flattened():
		BlockStructure(block).Flattened();

	assert(!(offset < 0));

	return BlendFileFlatStructBlockData(
		std::move(flat_struct),
		std::move(block),
		std::move(block_data),
		std::size_t(offset)
	);
}

OGLPLUS_LIB_FUNC
BlendFileBlockData BlendFile::BlockData(const BlendFileBlock& block)
{
	std::vector<char> data;
	if(block.Size())
	{
		data.resize(block.Size());
		_go_to(_reader, block.DataPosition());
		_raw_read(
			_reader,
			data.data(),
			data.size(),
			"Failed to read blend file block data"
		);
	}
	return BlendFileBlockData(
		std::move(data),
		_info.ByteOrder(),
		_info.PointerSize(),
		_sdna->_type_sizes[
			_sdna->_structs[block._sdna_index]._type_index
		]
	);
}

OGLPLUS_LIB_FUNC
BlendFileType BlendFile::TypeByIdx(std::size_t type_index) const
{
	if(type_index == _sdna->_invalid_type_index())
	{
		throw std::runtime_error("Unknown blender type");
	}
	return BlendFileType(
		_sdna.get(),
		type_index,
		_sdna->_type_structs[type_index]
	);
}

} // imports
} // oglplus


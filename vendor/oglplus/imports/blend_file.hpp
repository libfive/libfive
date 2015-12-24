/**
 *  @file oglplus/imports/blend_file.hpp
 *  @brief Tools for reading Blender's .blend files
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2013 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMPORTS_BLEND_FILE_1107121519_HPP
#define OGLPLUS_IMPORTS_BLEND_FILE_1107121519_HPP

#include <oglplus/imports/blend_file/utils.hpp>
#include <oglplus/imports/blend_file/reader_client.hpp>
#include <oglplus/imports/blend_file/range.hpp>
#include <oglplus/imports/blend_file/info.hpp>
#include <oglplus/imports/blend_file/sdna.hpp>
#include <oglplus/imports/blend_file/pointer.hpp>
#include <oglplus/imports/blend_file/block.hpp>
#include <oglplus/imports/blend_file/type.hpp>
#include <oglplus/imports/blend_file/structure.hpp>
#include <oglplus/imports/blend_file/flattened.hpp>
#include <oglplus/imports/blend_file/block_data.hpp>
#include <oglplus/imports/blend_file/struct_block_data.hpp>
#include <cstring>

namespace oglplus {
namespace imports {

class BlendFileStructGlobBlock
 : public BlendFileFlatStructBlockData
{
private:

	friend class BlendFile;

	BlendFileStructGlobBlock(BlendFileFlatStructBlockData&& tmp)
	 : BlendFileFlatStructBlockData(std::move(tmp))
	 , curscreen(Field<void*>("curscreen"))
	 , curscene(Field<void*>("curscene"))
	{ }
public:
	BlendFileFlatStructTypedFieldData<void*> curscreen;
	BlendFileFlatStructTypedFieldData<void*> curscene;
};

/// Represents and allows access to the structures and data of a .blend file
/**
 *  @note The objects representing blocks, structures, structure fields, etc.
 *  created directly or indirectly from a BlendFile instance must not be used
 *  after their "parent" BlendFile is destroyed. Doing so results in undefined
 *  behaviour.
 */
class BlendFile
 : public BlendFileReaderClient
{
private:
	BlendFileReader _reader;

	BlendFileInfo _info;

	std::vector<BlendFileBlock> _blocks;
	std::map<BlendFilePointer::ValueType, std::size_t> _block_map;

	std::size_t _glob_block_index;

	std::shared_ptr<BlendFileSDNA> _sdna;

	// internal string equality comparison utility
	template <std::size_t N>
	bool _equal(const std::array<char, N>& a, const char* b)
	{
		return std::strncmp(a.data(), b, N) == 0;
	}

public:
	/// Parses the file from an input stream
	/**
	 *  @note The input stream must exist during the whole lifetime
	 *  of an instance of BlendFile
	 */
	BlendFile(std::istream& input);

	/// Returns the basic file-level information
	const BlendFileInfo& Info(void) const
	{
		return _info;
	}

	/// Returns a range of meta-data describing structures unsed in the file
	BlendFileStructRange Structures(void) const
	{
		return BlendFileStructRange(_sdna.get());
	}


	/// Returns a block by its pointer
	const BlendFileBlock& BlockByPointer(
		BlendFilePointerBase pointer,
		bool allow_offset = false
	) const;

	/// Dereferences a pointer to pointer
	template <unsigned Level>
	BlendFilePointerTpl<Level-1>
	Dereference(BlendFilePointerTpl<Level> pptr)
	{
		auto block = BlockByPointer(pptr, true);
		auto offset = pptr - block.Pointer();
		auto block_data = BlockData(block);
		return block_data.template _do_get_pointer<Level-1>(
			pptr._type_index,
			0, 0, 0,
			offset
		);
	}

	/// Returns a structured block by its pointer
	BlendFileFlatStructBlockData StructuredBlockByPointer(
		BlendFilePointer pointer,
		bool allow_offset = false,
		bool use_pointee_struct = false
	);

	/// Alias for StructuredBlockByPointer
	BlendFileFlatStructBlockData operator[](BlendFilePointer pointer)
	{
		return StructuredBlockByPointer(pointer);
	}

	/// Returns the global block of the file
	const BlendFileBlock& GlobalBlock(void) const
	{
		return _blocks[_glob_block_index];
	}

	/// Returns a pointer to the global block
	BlendFilePointer GlobalBlockPointer(void) const
	{
		return GlobalBlock().Pointer();
	}

	/// Returns a structured global block object
	BlendFileStructGlobBlock StructuredGlobalBlock(void)
	{
		return BlendFileStructGlobBlock(
			StructuredBlockByPointer(
				GlobalBlockPointer()
			)
		);
	}

	/// Returns a range allowing the traversal of file blocks
	BlendFileBlockRange Blocks(void) const
	{
		return BlendFileBlockRange(_blocks);
	}

	/// Returns the structures of a file block
	BlendFileStruct BlockStructure(const BlendFileBlock& block) const
	{
		return BlendFileStruct(_sdna.get(), block._sdna_index);
	}

	/// Returns the data of a block
	BlendFileBlockData BlockData(const BlendFileBlock& block);

	BlendFileType TypeByIdx(std::size_t type_index) const;

	template <typename T>
	BlendFileType Type(void) const
	{
		return TypeByIdx(_sdna->_find_type_index<T>());
	}

	/// Returns the pointee type for a pointer
	BlendFileType Pointee(const BlendFilePointer& pointer) const
	{
		return BlendFileType(
			_sdna.get(),
			pointer._type_index,
			_sdna->_type_structs[pointer._type_index]
		);
	}
};

} // imports
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/imports/blend_file.ipp>
#endif

#endif // include guard

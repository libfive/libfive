/**
 *  @file oglplus/imports/blend_file/reader_client.hpp
 *  @brief Helper base class for .blend file import input operations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMPORTS_BLEND_FILE_READER_CLIENT_1107121519_HPP
#define OGLPLUS_IMPORTS_BLEND_FILE_READER_CLIENT_1107121519_HPP

#include <oglplus/imports/blend_file/reader.hpp>
#include <oglplus/detail/endian.hpp>
#include <array>
#include <cstring>

namespace oglplus {
namespace imports {

using oglplus::aux::Endian;

// base class for classes using the file reader
// provides additional high level functions
// based on the functions implemented by the reader
class BlendFileReaderClient
{
protected:
	static void _adjust_ptr_size(
		BlendFileReader& reader,
		std::size_t ptr_size
	)
	{
		reader._ptr_size = ptr_size;
	}

	// end of input indication
	static bool _eof(BlendFileReader& reader)
	{
		return reader._eof();
	}

	// returns the position in the input stream
	static std::streampos _position(const BlendFileReader& reader)
	{
		return reader._input.tellg();
	}

	// goes to the specified position in the input
	static void _go_to(const BlendFileReader& reader, std::streampos pos)
	{
		reader._input.seekg(pos);
	}

	// reads the specifies number of bytes into buffer
	// terminates the string with zero
	// throws on error
	static void _read(
		BlendFileReader& reader,
		char* buffer,
		std::size_t max,
		const char* error_message
	)
	{
		reader._read(buffer, max, error_message);
	}

	// reads the specifies number of bytes into buffer
	// throws on error
	static void _raw_read(
		BlendFileReader& reader,
		char* buffer,
		std::size_t size,
		const char* error_message
	)
	{
		reader._raw_read(buffer, size, error_message);
	}

	// reads the specified number of bytes from the input
	// throws on error
	template <std::size_t size>
	static std::array<char, size> _read_array(
		BlendFileReader& reader,
		const char* error_message
	)
	{
		std::array<char, size> result;
		reader._raw_read(result.data(), size, error_message);
		return result;
	}

	// reads a zero-terminated string
	// throws on error
	static std::string _read_str(
		BlendFileReader& reader,
		const char* error_message
	)
	{
		std::stringbuf buffer;
		reader._read_until(buffer, '\0', error_message);
		return buffer.str();
	}

	// reads a single character, throws on error
	static char _read_char(
		BlendFileReader& reader,
		const char* error_message
	)
	{
		return reader._read_char(error_message);
	}

	// reads an integer value, reorders to native byte-order
	// throws on error
	template <typename Int>
	static Int _read_int(
		BlendFileReader& reader,
		Endian file_byte_order,
		const char* error_message
	)
	{
		Int values;
		char* buffer = reinterpret_cast<char*>(&values);
		reader._raw_read(buffer, sizeof(Int), error_message);
		return aux::ReorderToNative(file_byte_order, values);
	}

	// skips the specified number of bytes in input
	// throws on error
	static void _skip(
		BlendFileReader& reader,
		const std::size_t size,
		const char* error_message
	)
	{
		reader._skip(size, error_message);
	}

	// aligns the input to the specified size
	// throws on error
	static void _align(
		BlendFileReader& reader,
		const std::size_t size,
		const char* error_message
	)
	{
		reader._align(size, error_message);
	}

	// reads the specified number of bytes and compares it
	// to the expected string, throws on read error
	// or if the read value is different from the expected
	bool _expect(
		BlendFileReader& reader,
		const char* expected,
		const std::size_t size,
		const char* error_message
	);

	// reads a single character and compares it
	// to the charactes in the options string
	// throws on read error or if the read char
	// is not found in the options
	char _expect_one_of(
		BlendFileReader& reader,
		const char* options,
		const std::size_t size,
		const char* error_message
	);
};

} // imports
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/imports/blend_file/reader_client.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

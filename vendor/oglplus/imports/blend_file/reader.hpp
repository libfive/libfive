/**
 *  @file oglplus/imports/blend_file/reader.hpp
 *  @brief Helper class for .blend file import basic input operations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMPORTS_BLEND_FILE_READER_1107121519_HPP
#define OGLPLUS_IMPORTS_BLEND_FILE_READER_1107121519_HPP

#include <oglplus/config/compiler.hpp>
#include <cassert>
#include <stdexcept>
#include <istream>
#include <sstream>
#include <cstddef>
#include <string>
#include <vector>


namespace oglplus {
namespace imports {

// Internal helper class used for .blend file read operations
// Wraps around an istream and implements operations used by
// the loader
// NOTE: implementation detail, do not use
class BlendFileReader
 : public BlendFileUtils
{
private:
	std::istream& _input;

	// peek at the next byte and return true if we are at EOF
	bool _eof(void);

	// composes error message and throws an exception
	OGLPLUS_NORETURN
	void _error(const std::string& message);

	// reads a single char throws on error
	char _read_char(const char* error_message);

	// reads the specified number of bytes throws on error
	void _raw_read(
		char* buffer,
		std::size_t size,
		const char* error_message
	);

	// reads a string of the specified lenght,
	// terminates by zero
	// throws on error
	void _read(
		char* buffer,
		std::size_t max,
		const char* error_message
	);

	// reads until a delimiter is found,
	// throws on error
	void _read_until(
		std::streambuf& sb,
		char delimiter,
		const char* error_message
	);

	// skips the specified number of bytes
	// throws on error
	void _skip(
		std::size_t size,
		const char* error_message
	);

	// aligns the input to the specified size
	// throws on error
	void _align(
		const std::size_t size,
		const char* error_message
	);

	friend class BlendFileReaderClient;
public:
	BlendFileReader(std::istream& input)
	 : BlendFileUtils(4)
	 , _input(input)
	{ }
};

} // imports
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/imports/blend_file/reader.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

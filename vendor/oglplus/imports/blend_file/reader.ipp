/**
 *  @file oglplus/imports/blend_file/reader.ipp
 *  @brief Implementation of helper BlendFile I/O class
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */
#include <oglplus/config/basic.hpp>

namespace oglplus {
namespace imports {

OGLPLUS_LIB_FUNC
bool BlendFileReader::_eof(void)
{
	_input.peek();
	return _input.eof();
}

OGLPLUS_NORETURN
OGLPLUS_LIB_FUNC
void BlendFileReader::_error(const std::string& message)
{
	std::stringstream ss;
	ss << "Blend file read error at byte ";
	ss << _input.tellg();
	ss << ": ";
	ss << message;
	throw std::runtime_error(ss.str());
}

OGLPLUS_LIB_FUNC
char BlendFileReader::_read_char(const char* error_message)
{
	char c = '\0';
	if(_input.get(c).fail()) _error(error_message);
	return c;
}

OGLPLUS_LIB_FUNC
void BlendFileReader::_raw_read(
	char* buffer,
	std::size_t size,
	const char* error_message
)
{
	assert(size != 0);
	if(_input.read(buffer, std::streamsize(size)).fail())
	{
		_error(error_message);
	}
}

OGLPLUS_LIB_FUNC
void BlendFileReader::_read(
	char* buffer,
	std::size_t max,
	const char* error_message
)
{
	assert(max != 0);
	if(_input.read(buffer, std::streamsize(max)).fail())
	{
		_error(error_message);
	}
	else buffer[max] = '\0';
}

OGLPLUS_LIB_FUNC
void BlendFileReader::_read_until(
	std::streambuf& sb,
	char delimiter,
	const char* error_message
)
{
	if(_input.get(sb, delimiter).fail())
	{
		_error(error_message);
	}
	_input.ignore();
}

OGLPLUS_LIB_FUNC
void BlendFileReader::_skip(
	std::size_t size,
	const char* error_message
)
{
	if(_input.ignore(std::streamsize(size)).fail())
	{
		_error(error_message);
	}
}

OGLPLUS_LIB_FUNC
void BlendFileReader::_align(
	const std::size_t size,
	const char* error_message
)
{
	std::streampos input_pos = _input.tellg();
	const std::streamoff mod = _align_diff(input_pos, size);
	if(mod != 0)
	{
		assert(size >= std::size_t(mod));
		_skip(size - std::size_t(mod), error_message);
		assert(std::size_t(_input.tellg()) % size == 0);
	}
}

} // imports
} // oglplus


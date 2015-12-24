/**
 *  @file oglplus/imports/blend_file/reader_client.ipp
 *  @brief Implementation of BlendFileReaderClient
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */
#include <oglplus/config/basic.hpp>

namespace oglplus {
namespace imports {

OGLPLUS_LIB_FUNC
bool BlendFileReaderClient::_expect(
	BlendFileReader& reader,
	const char* expected,
	const std::size_t size,
	const char* error_message
)
{
	char buffer[16] = {'\0'};
	assert(sizeof(buffer) > size);
	reader._read(buffer, size, error_message);
	if(std::strncmp(buffer, expected, size) != 0)
	{
		std::string msg("Expected '");
		msg.append(expected);
		msg.append("' instead of '");
		msg.append(buffer);
		msg.append("' in input");
		reader._error(msg);
	}
	return true;
}

OGLPLUS_LIB_FUNC
char BlendFileReaderClient::_expect_one_of(
	BlendFileReader& reader,
	const char* options,
	const std::size_t size,
	const char* error_message
)
{
	std::size_t i;
	char c = _read_char(reader, error_message);

	for(i=0; i!=size; ++i)
		if(c == options[i])
			return c;
	std::string msg("Expected one of {");
	for(i=0; i!=size; ++i)
	{
		if(i) msg.append(", ", 2);
		const char tmp[3] = {'\'', options[i], '\''};
		msg.append(tmp, 3);
	}
	msg.append("} in input");
	reader._error(msg);
	return '\0';
}

} // imports
} // oglplus


/**
 *  .file oglplus/utils/xml.ipp
 *  .brief Implementation simple xml utilities
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */
#include <iostream>
#include <cstring>

namespace oglplus {
namespace aux {

OGLPLUS_LIB_FUNC
std::ostream& xml_text_to_stream(
	const char* text,
	std::size_t length,
	std::ostream& output
)
{
	if(!text || !length) return output;

	const std::size_t n_ent = 5;
	const char ent_chr[n_ent] = {'\'', '"', '<', '>', '&'};
	const char* ent_name[n_ent] = {"apos", "quot", "lt", "gt", "amp"};

	const char* beg = text;
	const char* pos = text;
	const char* end = text+length;

	while((pos != end) && (*pos != '\0'))
	{
		const char* ent = std::strchr(ent_chr, *pos);
		if(ent && std::size_t(ent-ent_chr)<n_ent)
		{
			const char* name = ent_name[ent-ent_chr];
			output.write(beg, pos-beg);
			output.write("&", 1);
			output.write(name, std::streamsize(std::strlen(name)));
			output.write(";", 1);
			++pos;
			beg = pos;
		}
		else ++pos;
	}
	return output.write(beg, pos-beg);
}

} // namespace aux
} // namespace oglplus


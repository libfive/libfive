/**
 *  @file oglplus/extension.ipp
 *  @brief Implementation of GL extension helper functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/string/def.hpp>
#include <oglplus/context/string_queries.hpp>
#include <oglplus/lib/incl_end.ipp>

namespace oglplus {

OGLPLUS_LIB_FUNC
String MissingExtensionMessage(const GLchar* name)
{
	String message("Missing extension '");
	message.append(name);
	message.append("'");
	return std::move(message);
}

OGLPLUS_LIB_FUNC
void RequireExtension(const GLchar* name, bool available)
{
	OGLPLUS_HANDLE_ERROR_IF(
		!available,
		GL_INVALID_OPERATION,
		MissingExtensionMessage(name).c_str(),
		Error,
		NoInfo()
	);
}

OGLPLUS_LIB_FUNC
bool HasExtension(const GLchar* name)
{
	auto er = oglplus::context::StringQueries::Extensions();
	while(!er.Empty())
	{
		if(er.Front() == name) return true;
		er.Next();
	}
	return false;
}

} // namespace oglplus


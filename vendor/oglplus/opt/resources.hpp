/**
 *  @file oglplus/opt/resources.hpp
 *  @brief Resource (texture, model, etc.) file find functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_OPT_RESOURCES_1107121519_HPP
#define OGLPLUS_OPT_RESOURCES_1107121519_HPP

#include <oglplus/config/basic.hpp>
#include <oglplus/opt/application.hpp>
#include <oglplus/utils/filesystem.hpp>

# include <fstream>

namespace oglplus {
namespace aux {

std::size_t FindResourceFile(
	std::ifstream& file,
	const std::string& path,
	const char** exts,
	std::size_t nexts
);

} // namespace aux

std::size_t FindResourceFile(
	std::ifstream& file,
	const std::string& category,
	const std::string& name,
	const char** exts,
	std::size_t nexts
);

inline bool OpenResourceFile(
	std::ifstream& file,
	const std::string& category,
	const std::string& name,
	const char* ext
)
{
	return FindResourceFile(
		file,
		category,
		name,
		&ext,
		1
	) == 0;
}

class ResourceFile
 : public std::ifstream
{
public:
	std::ifstream& stream(void) { return *this; }

	ResourceFile(
		const std::string& category,
		const std::string& name,
		const char* ext
	);
};

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
# include <oglplus/opt/resources.ipp>
#endif

#endif // include guard

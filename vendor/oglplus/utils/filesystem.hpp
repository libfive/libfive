/**
 *  .file oglplus/utils/filesystem.hpp
 *  .brief Helper filesystem-related utilities
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2011-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#ifndef OGLPLUS_UTILS_FILESYSTEM_1102101236_HPP
#define OGLPLUS_UTILS_FILESYSTEM_1102101236_HPP

#include <oglplus/config/compiler.hpp>
#include <string>
#include <cassert>

namespace oglplus {
namespace aux {

inline std::string FilesysPathSep(void)
{
#if defined(WIN32) || defined(_WIN32) || defined(WIN64) || defined(_WIN64)
	return std::string("\\");
#else
	return std::string("/");
#endif
}

inline std::string FilesysPathParDir(void)
{
	return std::string("..");
}

inline std::string FilesysPathCurDir(void)
{
	return std::string(".");
}

inline bool IsFilesysPathSep(const char* str, size_t size)
{
	assert(size > 0);
	assert(str);
#if defined(WIN32) || defined(_WIN32) || defined(WIN64) || defined(_WIN64)
	return *str == '\\';
#else
	return *str == '/';
#endif
}

} // namespace aux
} // namespace oglplus

#endif

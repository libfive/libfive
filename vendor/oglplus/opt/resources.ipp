/**
 *  @file oglplus/opt/resources.ipp
 *  @brief Implementation of Resource-related functions
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

# include <stdexcept>

namespace oglplus {
namespace aux {

OGLPLUS_LIB_FUNC
std::size_t FindResourceFile(
	std::ifstream& file,
	const std::string& path,
	const char** exts,
	std::size_t nexts
)
{
	for(std::size_t e=0; e!=nexts; ++e)
	{
		file.open(path + exts[e], std::ios::binary);
		if(file.good()) return e;
	}
	return nexts;
}

} // namespace aux

OGLPLUS_LIB_FUNC
std::size_t FindResourceFile(
	std::ifstream& file,
	const std::string& category,
	const std::string& name,
	const char** exts,
	std::size_t nexts
)
{
	const std::string dirsep = aux::FilesysPathSep();
	const std::string pardir(aux::FilesysPathParDir() + dirsep);
	const std::string path = category+dirsep+name;
	const std::string apppath = Application::RelativePath();
	std::string prefix;

	for(std::size_t i=0; i!=5; ++i)
	{
		std::size_t iext = aux::FindResourceFile(
			file,
			apppath+prefix+path,
			exts,
			nexts
		);
		if(iext != nexts) return iext;
		prefix = pardir + prefix;
	}
	return nexts;
}

OGLPLUS_LIB_FUNC
ResourceFile::ResourceFile(
	const std::string& category,
	const std::string& name,
	const char* ext
)
{
	if(!OpenResourceFile(stream(), category, name, ext) || !good())
	{
		throw std::runtime_error(
			std::string("Failed to open resource file '")+
			category +
			aux::FilesysPathSep() +
			name +
			ext +
			std::string("'")
		);
	}
}

} // namespace oglplus


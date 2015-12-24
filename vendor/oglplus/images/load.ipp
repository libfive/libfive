/**
 *  @file oglplus/images/load.ipp
 *  @brief Implementation of Image loader
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/opt/application.hpp>
#include <oglplus/opt/resources.hpp>

#if OGLPLUS_PNG_FOUND
#include <oglplus/images/png.hpp>
#endif
#include <oglplus/images/xpm.hpp>
#include <oglplus/lib/incl_end.ipp>

#include <fstream>
#include <stdexcept>

namespace oglplus {
namespace images {

OGLPLUS_LIB_FUNC
Image LoadByName(
	std::string category,
	std::string name,
	bool y_is_up,
	bool x_is_right
)
{
	std::ifstream file;
	const char* exts[] = {".png", ".xpm"};
	std::size_t nexts = sizeof(exts)/sizeof(exts[0]);
	std::size_t iext = oglplus::FindResourceFile(
		file,
		category,
		name,
		exts,
		nexts
	);

	if(!file.good())
		throw std::runtime_error("Unable to open image: "+name);
	if(iext == 0) //.png
	{
#if OGLPLUS_PNG_FOUND
		return PNGImage(file, y_is_up, x_is_right);
#else
		OGLPLUS_FAKE_USE(y_is_up);
		OGLPLUS_FAKE_USE(x_is_right);
#endif
	}
	else if(iext == 1) //.xpm
	{
		return XPMImage(file, y_is_up, x_is_right);
	}
	throw std::runtime_error("Unable to open this image type");
}

} // images
} // oglplus


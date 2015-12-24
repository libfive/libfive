/**
 *  @file oglplus/images/png.hpp
 *  @brief PNG image loader (based on libpng)
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2013 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_PNG_1107121519_HPP
#define OGLPLUS_IMAGES_PNG_1107121519_HPP

#include <oglplus/images/image.hpp>

#include <istream>

namespace oglplus {
namespace images {

/// Loader of images in the PNG (Portable network graphics) format
/**
 *  @ingroup image_load_gen
 */
class PNGImage
 : public Image
{
public:
	/// Load the image from a file with the specified @p file_path
	PNGImage(
		const char* file_path,
		bool y_is_up = true,
		bool x_is_right = true
	);

	/// Load the image from the specified @p input stream
	PNGImage(
		std::istream& input,
		bool y_is_up = true,
		bool x_is_right = true
	);
};

} // images
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/images/png.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

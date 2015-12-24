/**
 *  @file oglplus/images/xpm.hpp
 *  @brief XPM (X Pix Map) image loader
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_XPM_1107121519_HPP
#define OGLPLUS_IMAGES_XPM_1107121519_HPP

#include <oglplus/images/image.hpp>

#include <istream>

namespace oglplus {
namespace images {

/// Loader of images in the XPM (X Pix Map) format
/**
 *  @ingroup image_load_gen
 */
class XPMImage
 : public Image
{
public:
	/// Load the image from the specified @p input stream
	XPMImage(
		std::istream& input,
		bool y_is_up = true,
		bool x_is_right = true
	);
};

} // images
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/images/xpm.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

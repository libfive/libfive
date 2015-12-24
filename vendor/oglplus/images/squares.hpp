/**
 *  @file oglplus/images/squares.hpp
 *  @brief Generator of a image covered with reqular squares
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_SQUARES_1107121519_HPP
#define OGLPLUS_IMAGES_SQUARES_1107121519_HPP

#include <oglplus/images/image.hpp>

namespace oglplus {
namespace images {

/// Creates a Red (one components per pixel) 2D image with repeating squares
/**
 *  @ingroup image_load_gen
 */
class Squares
 : public Image
{
public:
	/// Creates an image with specified dimensions and pattern repeats
	Squares(
		SizeType width,
		SizeType height,
		GLfloat ratio = 0.8f,
		SizeType xrep = 2,
		SizeType yrep = 2
	);
};

} // images
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/images/squares.ipp>
#endif

#endif // include guard

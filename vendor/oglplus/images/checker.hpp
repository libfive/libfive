/**
 *  @file oglplus/images/checker.hpp
 *  @brief Checker image generator
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_CHECKER_1107121519_HPP
#define OGLPLUS_IMAGES_CHECKER_1107121519_HPP

#include <oglplus/images/image.hpp>

namespace oglplus {
namespace images {

/// Creates a Red (one components per pixel) 2D checker image
/**
 *  @ingroup image_load_gen
 */
class CheckerRedBlack
 : public Image
{
public:
	/// Creates a checker image with specified dimensions and pattern repeats
	CheckerRedBlack(
		SizeType width,
		SizeType height,
		SizeType xrep = 4,
		SizeType yrep = 4
	);
};

} // images
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/images/checker.ipp>
#endif

#endif // include guard

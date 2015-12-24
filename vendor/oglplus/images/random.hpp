/**
 *  @file oglplus/images/random.hpp
 *  @brief Random image generator
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_RANDOM_1107121519_HPP
#define OGLPLUS_IMAGES_RANDOM_1107121519_HPP

#include <oglplus/images/image.hpp>

namespace oglplus {
namespace images {

/// Creates a RED (one component per pixel) white noise image
/**
 *  @ingroup image_load_gen
 */
class RandomRedUByte
 : public Image
{
public:
	RandomRedUByte(SizeType width, SizeType height = 1, SizeType depth = 1);
};


/// Creates a RGB (three components per pixel) white noise image
/**
 *  @ingroup image_load_gen
 */
class RandomRGBUByte
 : public Image
{
public:
	RandomRGBUByte(SizeType width, SizeType height = 1, SizeType depth = 1);
};

} // images
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/images/random.ipp>
#endif

#endif // include guard

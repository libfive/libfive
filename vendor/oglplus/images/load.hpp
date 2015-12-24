/**
 *  @file oglplus/images/load.hpp
 *  @brief Image loader which finds and loads an image from its name
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2013 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_LOAD_1107121519_HPP
#define OGLPLUS_IMAGES_LOAD_1107121519_HPP

#include <oglplus/images/image.hpp>

#include <string>

namespace oglplus {
namespace images {

Image LoadByName(
	std::string category,
	std::string name,
	bool y_is_up,
	bool x_is_right
);

/// Helper function for loading textures that come with @OGLplus in the examples
/**
 *  @ingroup image_load_gen
 */
inline Image LoadTexture(
	std::string name,
	bool y_is_up = true,
	bool x_is_right = true
)
{
	return LoadByName("textures", name, y_is_up, x_is_right);
}

} // images
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/images/load.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

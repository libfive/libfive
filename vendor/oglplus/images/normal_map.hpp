/**
 *  @file oglplus/images/normal_map.hpp
 *  @brief Filter creating a normal+height map out of a height map
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_NORMAL_MAP_1107121519_HPP
#define OGLPLUS_IMAGES_NORMAL_MAP_1107121519_HPP

#include <oglplus/images/filtered.hpp>

namespace oglplus {
namespace images {

/// A filter creating a normal-map/height-map from a height map image
/**
 *  @ingroup image_load_gen
 */
class NormalMap
 : public FilteredImage<GLfloat, 4>
{
public:
	typedef FilteredImage<GLfloat, 4> Filtered;

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Creates a normal-map from the @p input height-map image
	/**
	 *  @param input the height-map image to be filtered
	 *  @param extractor the height map color component extractor (by
	 *    default the RED component of the image is used as the height-map
	 *    value used in normal-map calculation).
	 */
	template <typename Extractor>
	NormalMap(const Image& input, Extractor extractor = Extractor());
#endif
	NormalMap(const Image& input);
	NormalMap(const Image& input, Filtered::FromRed);
	NormalMap(const Image& input, Filtered::FromAlpha);
};

} // images
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/images/normal_map.ipp>
#endif

#endif // include guard

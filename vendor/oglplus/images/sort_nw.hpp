/**
 *  @file oglplus/images/sort_nw.hpp
 *  @brief Generator of an image that encodes a sorting network
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_SORT_NW_1404252143_HPP
#define OGLPLUS_IMAGES_SORT_NW_1404252143_HPP

#include <oglplus/images/image.hpp>

namespace oglplus {
namespace images {

/// Generator of an Image encoding a sorting network of specified size
/** The sorting network for the specified number of elements is encoded
 *  into the Image in the following way:
 *
 *  The rows of the image represent the sorting operation passes, each row
 *  is a single pass. Each pass consists of trivial compare and swap operations
 *  which can be executed in parallel for the individual sorted elements.
 *
 *  The trivial operations consist of the following steps:
 *  1) determining the index of the currently processed element,
 *  2) determining the @c offset to the other element to compare (and swap),
 *  3) getting the values for the current and the other element,
 *  4) determining the @c direction in which the elements should be sorted,
 *  5) comparing the values and swapping the elements if they are not
 *     sorted in the right direction.
 *
 *  The @c offset and @c direction parameters are encoded in the following way:
 *  @code
 *  int pass;
 *  int index;
 *  uint texel = fetchTexel(ivec2(index, pass));
 *  int offset = int(texel >> 2) * (texel & 0x02)?-1:1;
 *  int direction = (texel & 0x01)?-1:1;
 *  int other_index = index+offset;
 *  @endcode
 *
 *  @ingroup image_load_gen
 */
class SortNWMap
 : public Image
{
private:
	typedef GLushort T;
	static unsigned _pot(unsigned n);
	static unsigned _next_log(unsigned n);
	static unsigned _num_steps(unsigned size);
public:
	/// Generates the sorting network encoding for size elements
	SortNWMap(unsigned size);
};

} // images
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/images/sort_nw.ipp>
#endif // OGLPLUS_LINK_LIBRARY

#endif // include guard

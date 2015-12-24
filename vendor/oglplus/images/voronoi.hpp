/**
 *  @file oglplus/images/voronoi.hpp
 *  @brief Voronoi diagram image generators
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_VORONOI_1107121519_HPP
#define OGLPLUS_IMAGES_VORONOI_1107121519_HPP

#include <oglplus/images/image.hpp>

namespace oglplus {
namespace images {

class VoronoiDiagram
 : public Image
{
public:
	VoronoiDiagram(
		SizeType cell_w,
		SizeType cell_h,
		SizeType cell_d,
		const Image& input
	);
};

class VoronoiCells
 : public Image
{
public:
	VoronoiCells(
		SizeType cell_w,
		SizeType cell_h,
		SizeType cell_d,
		const Image& input
	);
};

} // images
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/images/voronoi.ipp>
#endif

#endif // include guard

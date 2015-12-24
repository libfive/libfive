/**
 *  @file oglplus/images/worley.hpp
 *  @brief Worley cells image generators
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_WORLEY_1107121519_HPP
#define OGLPLUS_IMAGES_WORLEY_1107121519_HPP

#include <oglplus/images/image.hpp>
#include <functional>

namespace oglplus {
namespace images {

class WorleyCells
 : public Image
{
public:
	WorleyCells(
		SizeType cell_w,
		SizeType cell_h,
		SizeType cell_d,
		const Image& input
	);

	WorleyCells(
		SizeType cell_w,
		SizeType cell_h,
		SizeType cell_d,
		const Image& input,
		std::function<GLdouble(const std::vector<GLdouble>&)> calc_val,
		unsigned order
	);
};

} // images
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/images/worley.ipp>
#endif

#endif // include guard

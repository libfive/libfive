/**
 *  @file oglplus/images/worley.ipp
 *  @brief Implementation of images::WorleyCells
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/images/cell.hpp>
#include <oglplus/lib/incl_end.ipp>

namespace oglplus {
namespace images {

struct WorleyCellDistance
{
	GLdouble operator()(const std::vector<GLdouble>& d) const
	{
		assert(d.size() >= 2);
		return d[1]*0.7;
	}
};

OGLPLUS_LIB_FUNC
WorleyCells::WorleyCells(
	SizeType cell_w,
	SizeType cell_h,
	SizeType cell_d,
	const Image& input
): Image(static_cast<Image&&>(
	WorleyCellGen(
		cell_w, cell_h, cell_d,
		input,
		WorleyCellDistance(), 2
	)
))
{ }

OGLPLUS_LIB_FUNC
WorleyCells::WorleyCells(
	SizeType cell_w,
	SizeType cell_h,
	SizeType cell_d,
	const Image& input,
	std::function<GLdouble(const std::vector<GLdouble>&)> calc_value,
	unsigned order
): Image(static_cast<Image&&>(
	WorleyCellGen(
		cell_w, cell_h, cell_d,
		input,
		calc_value,
		order
	)
))
{ }

} // namespace images
} // namespace oglplus


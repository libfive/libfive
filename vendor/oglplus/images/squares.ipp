/**
 *  @file oglplus/images/squares.ipp
 *  @brief implementation of images::Squares
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <cassert>
#include <cmath>

namespace oglplus {
namespace images {

OGLPLUS_LIB_FUNC
Squares::Squares(
	SizeType width,
	SizeType height,
	GLfloat ratio,
	SizeType xrep,
	SizeType yrep
): Image(width, height, 1, 1, &TypeTag<GLubyte>())
{
	assert(width != 0 && height != 0);
	assert(ratio > 0.0f && ratio <= 1.0f);
	assert(xrep != 0 && yrep != 0);

	auto p = this->_begin_ub();

	float rmin = (1.0f - ratio) * 0.5f;
	float rmax = rmin + ratio;

	for(GLsizei y=0; y!=height; ++y)
	for(GLsizei x=0; x!=width;  ++x)
	{
		float vx = float((x * xrep)% width)/float(width);
		float vy = float((y * yrep)%height)/float(height);
		bool outside =
			((vx < rmin) || (vx > rmax)) ||
			((vy < rmin) || (vy > rmax));
		*p++ = outside?0x00:0xFF;
	}
	assert(p == this->_end_ub());
}

} // images
} // oglplus


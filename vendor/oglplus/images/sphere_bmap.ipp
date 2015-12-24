/**
 *  @file oglplus/images/sphere_bmap.ipp
 *  @brief Implementation of images::SphereBumpMap
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/math/vector.hpp>
#include <oglplus/lib/incl_end.ipp>

#include <cassert>
#include <cmath>

namespace oglplus {
namespace images {

OGLPLUS_LIB_FUNC
SphereBumpMap::SphereBumpMap(
	SizeType width,
	SizeType height,
	SizeType xrep,
	SizeType yrep
): Image(
	width,
	height,
	1,
	4,
	&TypeTag<GLfloat>(),
	PixelDataFormat::RGBA,
	PixelDataInternalFormat::RGBA16F
)
{
	assert(width != 0 && height != 0);
	assert(xrep != 0 && yrep != 0);

	typedef double number;
	number one = number(1);
	number invw = number(2*xrep)/number(width);
	number invh = number(2*yrep)/number(height);
	GLsizei hi = width/xrep;
	GLsizei hj = height/yrep;

	auto p = this->_begin<GLfloat>();
	for(GLsizei j=0; j<height; ++j)
	{
		number y = number((j % hj) - hj/2)*invh;
		for(GLsizei i=0; i<width; ++i)
		{
			number x = number((i % hi) - hi/2)*invw;
			number l = std::sqrt(x*x + y*y);
			number d = sqrt(one-l*l);
			Vector<number, 3> z(0.0, 0.0, one);
			Vector<number, 3> n(-x, -y, d);
			Vector<number, 3> v = (l >= one)?
				z:
				Normalized(z+n);
			if(l >= one) d = 0;
			assert(p != this->_end<GLfloat>());
			*p = GLfloat(v.x()); ++p;
			*p = GLfloat(v.y()); ++p;
			*p = GLfloat(v.z()); ++p;
			*p = GLfloat(d); ++p;
		}
	}
	assert(p == this->_end<GLfloat>());
}

} // images
} // oglplus


/**
 *  @file oglplus/images/checker.ipp
 *  @brief Implementation of images::Checker
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
CheckerRedBlack::CheckerRedBlack(
	SizeType width,
	SizeType height,
	SizeType xrep,
	SizeType yrep
): Image(
	width,
	height,
	1,
	1,
	&TypeTag<GLubyte>(),
	PixelDataFormat::Red,
	PixelDataInternalFormat::Red
)
{
	assert(width != 0 && height != 0);
	assert(xrep != 0 && yrep != 0);

	GLsizei xdiv = GLsizei(width) / xrep;
	GLsizei ydiv = GLsizei(height)/ yrep;

	auto p = this->_begin<GLubyte>();
	for(GLsizei j=0; j!=height; ++j)
	{
		GLsizei y = j / ydiv;
		for(GLsizei i=0; i!=width; ++i)
		{
			GLsizei x = i / xdiv;
			GLubyte c = ((x+y)%2==0)?0x00:0xFF;
			assert(p != this->_end<GLubyte>());
			*p = c; ++p;
		}
	}
	assert(p == this->_end<GLubyte>());
}

} // namespace images
} // namespace oglplus


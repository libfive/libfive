/**
 *  @file oglplus/images/random.ipp
 *  @brief Implementation of images::Random*
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <cassert>
#include <cstdlib>

namespace oglplus {
namespace images {

OGLPLUS_LIB_FUNC
RandomRedUByte::RandomRedUByte(SizeType width, SizeType height, SizeType depth)
 : Image(width, height, depth, 1, &TypeTag<GLubyte>())
{
	auto p = this->_begin_ub(), e = this->_end_ub();
	for(GLsizei k=0, d=depth; k<d; ++k)
	for(GLsizei j=0, h=height; j<h; ++j)
	for(GLsizei i=0, w=width; i<w; ++i)
	{
		assert(p != e);
		*p = GLubyte(::std::rand() % 0x100);
		++p;
	}
	OGLPLUS_FAKE_USE(e);
	assert(p == e);
}

OGLPLUS_LIB_FUNC
RandomRGBUByte::RandomRGBUByte(SizeType width, SizeType height, SizeType depth)
 : Image(width, height, depth, 3, &TypeTag<GLubyte>())
{
	auto p = this->_begin_ub(), e = this->_end_ub();
	for(GLsizei k=0, d=depth; k<d; ++k)
	for(GLsizei j=0, h=height; j<h; ++j)
	for(GLsizei i=0, w=width; i<w; ++i)
	for(GLsizei c=0; c<3; ++c)
	{
		assert(p != e);
		*p = GLubyte(::std::rand() % 0x100);
		++p;
	}
	OGLPLUS_FAKE_USE(e);
	assert(p == e);
}

} // images
} // oglplus


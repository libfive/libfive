/**
 *  @file oglplus/images/brushed_metal.ipp
 *  @brief Implementation of images::BrushedMetal texture generator
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <cassert>
#include <cstdlib>
#include <cmath>

namespace oglplus {
namespace images {

OGLPLUS_LIB_FUNC
void BrushedMetalUByte::_make_pixel(
	GLubyte* b,
	GLubyte *e,
	GLsizei w,
	GLsizei h,
	GLint x,
	GLint y,
	GLdouble /*c*/,
	GLubyte r,
	GLubyte g
)
{
	while(x < 0) x += w;
	while(y < 0) y += h;
	if(x >= w) x %= w;
	if(y >= h) y %= h;
	GLubyte* p = b + (y*w + x)*3;
	GLubyte* pr = p;
	GLubyte* pg = p+1;
	GLubyte* pb = p+2;

	OGLPLUS_FAKE_USE(e);
	assert((pr < e) && (pg < e) && (pb < e));

	*pr = r;
	*pg = g;
	*pb = (*pb + 8) % 0x100;
}

OGLPLUS_LIB_FUNC
void BrushedMetalUByte::_make_scratch(
	GLubyte* b,
	GLubyte *e,
	GLsizei w,
	GLsizei h,
	GLint x,
	GLint y,
	GLint dx,
	GLint dy
)
{
	if((dx == 0) && (dy == 0)) return;

	GLdouble dd = std::sqrt(GLdouble(dx*dx + dy*dy));

	GLubyte r = GLubyte((dy/dd)*0xFF);
	GLubyte g = GLubyte((dx/dd)*0xFF);

	if(dx > dy)
	{
		if(dx >= 0)
		{
			for(GLint i=0; i<dx; ++i)
			{
				GLdouble c = GLdouble(i)/dx;
				GLint j = GLint(dy*c);
				_make_pixel(b,e,w,h,x+i,y+j,c,r,g);
			}
		}
		else
		{
			for(GLint i=0; i>dx; --i)
			{
				GLdouble c = GLdouble(i)/dx;
				GLint j = GLint(dy*c);
				_make_pixel(b,e,w,h,x+i,y+j,c,r,g);
			}
		}
	}
	else
	{
		if(dy >= 0)
		{
			for(GLint j=0; j<dy; ++j)
			{
				GLdouble c = GLdouble(j)/dy;
				GLint i = GLint(dx*c);
				_make_pixel(b,e,w,h,x+i,y+j,c,r,g);
			}
		}
		else
		{
			for(GLint j=0; j>dy; --j)
			{
				GLdouble c = GLdouble(j)/dy;
				GLint i = GLint(dx*c);
				_make_pixel(b,e,w,h,x+i,y+j,c,r,g);
			}
		}
	}

}

OGLPLUS_LIB_FUNC
BrushedMetalUByte::BrushedMetalUByte(
	SizeType width,
	SizeType height,
	unsigned n_scratches,
	int s_disp_min,
	int s_disp_max,
	int t_disp_min,
	int t_disp_max
): Image(width, height, 1, 3, &TypeTag<GLubyte>())
{
	this->_bzero();

	GLubyte *p = this->_begin_ub(), *e = this->_end_ub();

	while(n_scratches--)
	{
		const GLuint n_segments = 1 + std::rand() % 4;
		GLint x = std::rand() % width;
		GLint y = std::rand() % height;
		for(GLuint seg=0; seg<n_segments; ++seg)
		{
			GLint dx = s_disp_min +
				(std::rand()%(s_disp_max-s_disp_min+1));
			GLint dy = t_disp_min +
				(std::rand()%(t_disp_max-t_disp_min+1));

			_make_scratch(
				p, e,
				width,
				height,
				x, y,
				dx, dy
			);
			x += dx;
			y += dy;
		}
	}
}

} // images
} // oglplus


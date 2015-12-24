/**
 *  @file oglplus/images/brushed_metal.hpp
 *  @brief Brushed metal texture generator
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_BRUSHED_METAL_1107121519_HPP
#define OGLPLUS_IMAGES_BRUSHED_METAL_1107121519_HPP

#include <oglplus/images/image.hpp>

namespace oglplus {
namespace images {

/// Creates a RGB (three components per pixel) image used for brushed metal effect
/**
 *  @ingroup image_load_gen
 */
class BrushedMetalUByte
 : public Image
{
private:
	static void _make_pixel(
		GLubyte* b,
		GLubyte *e,
		GLsizei w,
		GLsizei h,
		GLint x,
		GLint y,
		GLdouble /*c*/,
		GLubyte r,
		GLubyte g
	);

	static void _make_scratch(
		GLubyte* b,
		GLubyte *e,
		GLsizei w,
		GLsizei h,
		GLint x,
		GLint y,
		GLint dx,
		GLint dy
	);
public:
	BrushedMetalUByte(
		SizeType width,
		SizeType height,
		unsigned n_scratches,
		int s_disp_min,
		int s_disp_max,
		int t_disp_min,
		int t_disp_max
	);
};

} // images
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/images/brushed_metal.ipp>
#endif

#endif // include guard

/**
 *  @file oglplus/images/metaballs.hpp
 *  @brief Metaballs texture generator.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_METABALLS_1107121519_HPP
#define OGLPLUS_IMAGES_METABALLS_1107121519_HPP

#include <oglplus/images/image.hpp>

namespace oglplus {
namespace images {

/// Creates a Red (one components per pixel) seamless 2D metaballs image
/**
 *  @ingroup image_load_gen
 */
class BaseMetaballs
 : public Image
{
public:
	/// Creates an image with specified dimensions
	/**
	 *  @param width the width of the image
	 *  @param height the height of the image
	 *  @param balls the metaball data. This array contains the metaball
	 *   parameters: 3, 4 or 5 values per single metaball. The first
	 *   two values are the x, y coordinates of the center of the balls
	 *   normalized to (0, 1). The third value is the radius of the ball,
	 *   also normalized to (0, 1). If four values are specified then
	 *   meta-stars are generated and the fourth value is the number of
 	 *   star's points. If five values are used then the fifth value
	 *   controls the 'pointiness' of the star, normalized to (0, 1).
	 *  @param size the number of values in the balls array.
	 *  @param n_per_ball the number of values per single ball in the balls
	 *   array.
	 *
	 *  @pre (balls) && (size) && (size % n_per_ball == 0)
	 */
	BaseMetaballs(
		SizeType width,
		SizeType height,
		const GLfloat* balls,
		std::size_t size,
		std::size_t n_per_ball
	);
};

/// Creates a Red (one components per pixel) seamless 2D metaballs image
/**
 *  @ingroup image_load_gen
 */
class Metaballs
 : public BaseMetaballs
{
public:
	/// Creates an image with specified dimensions
	/**
	 *  @param width the width of the image
	 *  @param height the height of the image
	 *  @param balls the metaball data. This array contains the metaball
	 *   parameters: 3 values per single metaball. The first
	 *   two values are the x, y coordinates of the center of the balls
	 *   normalized to (0, 1). The third value is the radius of the ball,
	 *   also normalized to (0, 1).
	 *  @param size the number of values in the balls array.
	 */
	Metaballs(
		SizeType width,
		SizeType height,
		const GLfloat* balls,
		std::size_t size
	): BaseMetaballs(width, height, balls, size, 3)
	{ }
};


/// Creates a Red seamless 2D image with random metaballs
/**
 *  @ingroup image_load_gen
 */
class RandomMetaballs
 : public BaseMetaballs
{
private:
	static std::vector<GLfloat> _make_balls(
		std::size_t count,
		GLfloat rad_min,
		GLfloat rad_max
	);
public:
	/// Creates an image with the specified dimensions and params
	/**
	 *  @param width the width of the image
	 *  @param height the height of the image
	 *  @param count the number of metaballs to be generated.
	 *  @param rad_min the minimum radius of a metaball normalized to (0, 1).
	 *  @param rad_max the maximum radius of a metaball normalized to (0, 1).
	 */
	RandomMetaballs(
		SizeType width,
		SizeType height,
		std::size_t count,
		GLfloat rad_min,
		GLfloat rad_max
	);
};

/// Creates a Red (one components per pixel) seamless 2D metastars image
/**
 *  @ingroup image_load_gen
 */
class Metastars
 : public BaseMetaballs
{
public:
	/// Creates an image with specified dimensions
	/**
	 *  @param width the width of the image
	 *  @param height the height of the image
	 *  @param balls the metaball data. This array contains the metaball
	 *   parameters: 5 values per single metaball. The first
	 *   two values are the x, y coordinates of the center of the balls
	 *   normalized to (0, 1). The third value is the radius of the ball,
	 *   also normalized to (0, 1). The fourth value is the number of
 	 *   star's points. The fifth value controls the 'pointiness'
	 *   of the star, normalized to (0, 1).
	 *  @param size the number of values in the balls array.
	 */
	Metastars(
		SizeType width,
		SizeType height,
		const GLfloat* balls,
		std::size_t size
	): BaseMetaballs(width, height, balls, size, 5)
	{ }
};

/// Creates a Red seamless 2D image with random metastars
/**
 *  @ingroup image_load_gen
 */
class RandomMetastars
 : public BaseMetaballs
{
private:
	static std::vector<GLfloat> _make_stars(
		std::size_t count,
		GLfloat rad_min,
		GLfloat rad_max,
		GLfloat dif_min,
		GLfloat dif_max,
		GLuint pt_min,
		GLuint pt_max
	);
public:
	/// Creates an image with the specified dimensions and params
	/**
	 *  @param width the width of the image
	 *  @param height the height of the image
	 *  @param count the number of metastars to be generated.
	 *  @param rad_min the minimum radius of a metaball normalized to (0, 1).
	 *  @param rad_max the maximum radius of a metaball normalized to (0, 1).
	 */
	RandomMetastars(
		SizeType width,
		SizeType height,
		std::size_t count,
		GLfloat rad_min,
		GLfloat rad_max,
		GLfloat dif_min,
		GLfloat dif_max,
		GLuint ptc_min,
		GLuint ptc_max
	);
};

} // images
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/images/metaballs.ipp>
#endif

#endif // include guard

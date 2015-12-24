/**
 *  @file oglplus/images/cloud.hpp
 *  @brief Cloud 3d texture generator
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_IMAGES_CLOUD_1107121519_HPP
#define OGLPLUS_IMAGES_CLOUD_1107121519_HPP

#include <oglplus/images/image.hpp>
#include <oglplus/math/vector.hpp>

namespace oglplus {
namespace images {

/// A simple generator of 3D textures which can be used to render cloud effects
/** This class generates alpha (or RED, i.e. one component per pixel) textures
 *  which represent the density of a vapor cloud or smoke in 3D space.
 *
 *  @ingroup image_load_gen
 */
class Cloud
 : public Image
{
private:
	GLfloat _sub_scale;
	GLfloat _sub_variance;
	GLfloat _min_radius;

	void _adjust_sphere(Vec3f& center, GLfloat& radius) const;
	bool _apply_sphere(const Vec3f& center, GLfloat radius);

	static GLfloat _rand_u(void);
	static GLfloat _rand_s(void);

	void _make_spheres(Vec3f center, GLfloat radius);
public:
	/// Creates a cloud image of given @p width, @p height and @p depth
	Cloud(
		SizeType width,
		SizeType height,
		SizeType depth,
		const Vec3f& origin = Vec3f(0.0f, -0.3f, 0.0f),
		GLfloat init_radius = 0.7f,
		GLfloat sub_scale = 0.333f,
		GLfloat sub_variance = 0.5f,
		GLfloat min_radius = 0.04f
	);
};

class Cloud2D
 : public Image
{
public:
	Cloud2D(const Cloud& cloud);
};

} // images
} // oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/images/cloud.ipp>
#endif

#endif // include guard

/**
 *  @file oglplus/images/cloud.ipp
 *  @brief Implementation of images::Cloud
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#include <oglplus/lib/incl_begin.ipp>
#include <oglplus/math/angle.hpp>
#include <oglplus/lib/incl_end.ipp>

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <cmath>

namespace oglplus {
namespace images {

OGLPLUS_LIB_FUNC
void Cloud::_adjust_sphere(Vec3f& center, GLfloat& radius) const
{
	GLfloat c[3] = {center.x(), center.y(), center.z()};
	for(unsigned i=0; i!=3; ++i)
	{
		if(c[i] < -1.0f)
		{
			radius *= 0.5f;
			c[i] = -1.0f + radius;
		}
		if(c[i] > 1.0f)
		{
			radius *= 0.5f;
			c[i] = 1.0f - radius;
		}
		if((c[i] - radius) < -1.0f)
		{
			GLfloat d = (-1.0f - c[i] + radius)*0.51f;
			assert(d >= 0.0f);
			radius -= d;
			c[i] += d;
		}
		if((c[i] + radius) > 1.0f)
		{
			GLfloat d = (c[i] + radius - 1.0f)*0.51f;
			assert(d >= 0.0f);
			radius -= d;
			c[i] -= d;
		}
	}
	center = Vec3f(c[0], c[1], c[2]);
}

OGLPLUS_LIB_FUNC
bool Cloud::_apply_sphere(const Vec3f& center, GLfloat radius)
{
	assert(radius > 0.0f);
	bool something_updated = false;
	Vec3f c = center*0.5f + Vec3f(0.5f, 0.5f, 0.5f);
	GLfloat r = radius*0.5f;
	GLsizei w = Width(), h = Height(), d = Depth();
	GLubyte* data = _begin_ub();
	for(GLsizei k=GLsizei((c.z()-r)*d), ke=GLsizei((c.z()+r)*d); k!=ke; ++k)
	for(GLsizei j=GLsizei((c.y()-r)*h), je=GLsizei((c.y()+r)*h); j!=je; ++j)
	for(GLsizei i=GLsizei((c.x()-r)*w), ie=GLsizei((c.x()+r)*w); i!=ie; ++i)
	{
		assert(k >= 0 && k < d);
		assert(j >= 0 && j < h);
		assert(i >= 0 && i < w);
		GLsizei n = k*w*h + j*w + i;
		GLubyte b = data[n];
		if(b != 0xFF)
		{
			GLfloat cd = GLfloat(b)/GLfloat(0xFF);
			Vec3f p(GLfloat(i)/w, GLfloat(j)/h, GLfloat(k)/d);
			GLfloat nd = (r - Distance(c, p))/r;
			if(nd < 0.0f) nd = 0.0f;
			nd = std::sqrt(nd);
			nd += cd;
			if(nd > 1.0f) nd = 1.0f;
			data[n] = GLubyte(0xFF * nd);
			something_updated = true;
		}
	}
	return something_updated;
}

OGLPLUS_LIB_FUNC
GLfloat Cloud::_rand_u(void)
{
	return GLfloat(std::rand())/RAND_MAX;
}

OGLPLUS_LIB_FUNC
GLfloat Cloud::_rand_s(void)
{
	return (_rand_u() - 0.5f)*2.0f;
}

OGLPLUS_LIB_FUNC
void Cloud::_make_spheres(Vec3f center, GLfloat radius)
{
	_adjust_sphere(center, radius);
	if(radius < _min_radius) return;
	if(!_apply_sphere(center, radius)) return;
	GLfloat sub_radius = radius * _sub_scale;
	GLsizei i = 0;
	GLsizei n = GLsizei((8.0f*radius*radius)/(sub_radius*sub_radius));
	while(i != n)
	{
		auto rad = radius*(1.0f + _rand_s()*_sub_variance*0.5f);
		auto rho = FullCircles(_rand_u());
		auto phi = RightAngles(_rand_s());
		_make_spheres(
			center + Vec3f(
				rad*Cos(phi)*Cos(rho),
				rad*Sin(phi),
				rad*Cos(phi)*Sin(rho)
			),
			sub_radius*(1.0f + _rand_s()*_sub_variance)
		);
		++i;
	}
}

OGLPLUS_LIB_FUNC
Cloud::Cloud(
	SizeType width,
	SizeType height,
	SizeType depth,
	const Vec3f& origin,
	GLfloat init_radius,
	GLfloat sub_scale,
	GLfloat sub_variance,
	GLfloat min_radius
): Image(width, height, depth, 1, &TypeTag<GLubyte>())
 , _sub_scale(sub_scale)
 , _sub_variance(sub_variance)
 , _min_radius(min_radius)
{
	std::fill(this->_begin_ub(), this->_end_ub(), GLubyte(0));
	_make_spheres(origin, init_radius);
}

OGLPLUS_LIB_FUNC
Cloud2D::Cloud2D(const Cloud& cloud)
 : Image(cloud.Width(), cloud.Height(), 1, 3, &TypeTag<GLubyte>())
{
	auto p = this->_begin_ub();
	auto e = this->_end_ub();
	GLsizei w = Width(), h = Height(), d = cloud.Depth();
	for(GLsizei j=0; j!=h; ++j)
	for(GLsizei i=0; i!=w; ++i)
	{
		GLubyte depth_near = 0;
		GLubyte depth_far = 0;
		GLuint total_density = 0;
		for(GLsizei k=0; k!=d; ++k)
		{
			GLubyte c = cloud.ComponentAs<GLubyte>(i, j, k, 0);
			if(depth_near == 0)
			{
				if(c != 0)
				{
					depth_near = GLubyte((0xFF*k)/d);
					depth_far = depth_near;
				}
			}
			else if(depth_far == depth_near)
			{
				if(c == 0)
				{
					depth_far = GLubyte((0xFF*k)/d);
				}
			}
			total_density += c;
		}
		assert(depth_far >= depth_near);
		GLuint avg_density =
			((depth_far-depth_near) > 0)?
			total_density/(depth_far-depth_near):0;
		assert(p != e);
		*p = depth_near; ++p;
		assert(p != e);
		*p = depth_far; ++p;
		assert(p != e);
		*p = GLubyte(avg_density); ++p;
	}
	OGLPLUS_FAKE_USE(e);
	assert(p == e);
}

} // images
} // oglplus


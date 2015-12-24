/**
 *  @file oglplus/math/sphere.hpp
 *  @brief Sphere utility class
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_MATH_SPHERE_1310040710_HPP
#define OGLPLUS_MATH_SPHERE_1310040710_HPP

#include <oglplus/math/vector.hpp>

#include <cassert>
#include <type_traits>

namespace oglplus {

/// Class implementing sphere-related functionality
/** @c Sphere is a lightweight class for working with spheres.
 *
 *  @ingroup math_utils
 */
template <typename T>
class Sphere
{
private:
	Vector<T, 3> _center;
	T _radius;
public:
	/// Constructs a unit sphere at the origin
	Sphere(void)
	 : _center(T(0))
	 , _radius(T(1))
	{ }

	/// Constructs a sphere at the specified position and the specified radius
	/**
	 *  @pre r >= 0
	 */
	Sphere(T x, T y, T z, T r)
	 : _center(x, y, z)
	 , _radius(r)
	{
		assert(_radius >= T(0));
	}

	/// Constructs a sphere with the specified radius at the position
	/**
	 *  @pre radius >= 0
	 */
	Sphere(const Vector<T, 3>& position, T radius)
	 : _center(position)
	 , _radius(radius)
	{
		assert(_radius >= T(0));
	}

	/// Copy construction from angles using different underlying type
	template <typename U>
	Sphere(const Sphere<U>& that)
	 : _center(that.Center())
	 , _radius(that.Radius())
	{
		assert(_radius >= T(0));
	}

	/// Returns the position of the center of the sphere
	const Vector<T, 3>& Center(void) const
	{
		return _center;
	}

	/// Sets the position of the sphere
	void Center(const Vector<T, 3>& position)
	{
		_center = position;
	}

	/// Translates the sphere to a new position by @p offset
	void Translate(const Vector<T, 3>& offset)
	{
		_center += offset;
	}

	/// Transforms the spheres origin by the specified matrix
	void Transform(const Matrix<T, 4, 4>& matrix)
	{
		_center = (matrix * Vector<T, 4>(_center, 1)).xyz();
	}

	/// Returns true if the sphere is degenerate (has zero radius)
	bool Degenerate(void) const
	{
		return _radius == T(0);
	}

	/// Returns the radius of the sphere
	/**
	 *  @post Radius() >= 0
	 */
	T Radius(void) const
	{
		return _radius;
	}

	/// Sets the radius of the sphere
	/**
	 *  @pre radius >= 0
	 */
	void Radius(T radius)
	{
		assert(_radius >= T(0));
		_radius = radius;
	}

	/// Grows the sphere by the specified @p amount
	/**
	 *  @pre amount >= 0
	 */
	void Grow(T amount)
	{
		assert(amount >= T(0));
		_radius += amount;
	}

	/// Shrinks the sphere by the specified @p amount
	/**
	 *  @pre (amount >= 0) && (amount <= this->Radius())
	 */
	void Shrink(T amount)
	{
		assert(amount >= T(0));
		assert(amount <= Radius());
		_radius -= amount;
	}

	/// Scales the sphere by the specified @p amount
	/**
	 *  @pre amount >= 0
	 */
	void Scale(T amount)
	{
		assert(amount >= T(0));
		_radius *= amount;
	}

	/// Returns the diameter of the sphere
	/**
	 *  @post Diameter() >= 0
	 */
	T Diameter(void) const
	{
		return 2*Radius();
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Returns true if spheres a and b are intersecting
	friend bool Intersecting(const Sphere& a, const Sphere& b);
#endif

	/// Returns true if this sphere intersects with @p that sphere
	bool IntersectsWith(const Sphere& that) const
	{
		T d = this->_radius + that._radius;
		return Distance(this->_center, that._center) < d;
	}
};

#if OGLPLUS_DOCUMENTATION_ONLY || defined(GL_FLOAT)
/// Instantiation of Sphere using GL floating-point as underlying type
typedef Sphere<GLfloat> Spheref;
#endif

template <typename T>
bool Intersecting(const Sphere<T>& a, const Sphere<T>& b)
{
	return a.IntersectsWith(b);
}

} // namespace oglplus

#endif // include guard

/**
 *  @file oglplus/math/plane.hpp
 *  @brief Plane in 3D space utility class
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_PLANE_1107121519_HPP
#define OGLPLUS_PLANE_1107121519_HPP

#include <cmath>

namespace oglplus {

/// Class implementing plane-related functionality
/** @c Plane is a lightweight class allowing more natural construction and
 *  usage of 2D planes in 3D space.
 *
 *  @ingroup math_utils
 */
template <typename T>
class Plane
{
private:
	Vector<T, 4> _equation;
public:
	/// construction from parameters
	Plane(T a, T b, T c, T d)
	 : _equation(a, b, c, d)
	{ }

	/// Constructions from parameter vector
	Plane(const Vector<T, 4>& v)
	 : _equation(v)
	{ }

	struct FromTriangle_ { };

	Plane(
		FromTriangle_,
		const Vector<T, 3>& p0,
		const Vector<T, 3>& p1,
		const Vector<T, 3>& p2
	): _equation(
		Normalized(Cross(p1-p0, p2-p0)),
		-Dot(Normalized(Cross(p1-p0, p2-p0)), p0)
	)
	{ }


	/// Constructs a plane defined by points p0, p1, p2
	static inline Plane FromTriangle(
		const Vector<T, 3>& p0,
		const Vector<T, 3>& p1,
		const Vector<T, 3>& p2
	)
	{
		return Plane(
			FromTriangle_(),
			p0, p1, p2
		);
	}

	struct FromPointAndVectors_ { };

	Plane(
		FromPointAndVectors_,
		const Vector<T, 3>& p,
		const Vector<T, 3>& v1,
		const Vector<T, 3>& v2
	): _equation(
		Normalized(Cross(v1, v2)),
		-Dot(Normalized(Cross(v1, v2)), p)
	)
	{ }

	/// Constructs a plane defined by point p0 and vectors v1 and v2
	static inline Plane FromPointAndVectors(
		const Vector<T, 3>& p,
		const Vector<T, 3>& v1,
		const Vector<T, 3>& v2
	)
	{
		return Plane(
			FromPointAndVectors_(),
			p, v1, v2
		);
	}

	struct FromNormal_ { };

	Plane(FromNormal_, const Vector<T, 3>& normal)
	 : _equation(normal, T(0))
	{ }

	/// Constructs a plane going through the origin from its normal vector
	static inline Plane FromNormal(const Vector<T, 3>& normal)
	{
		return Plane(FromNormal_(), normal);
	}

	struct FromPointAndNormal_ { };

	Plane(
		FromPointAndNormal_,
		const Vector<T, 3>& point,
		const Vector<T, 3>& normal
	): _equation(normal, -Dot(normal, point))
	{ }

	/// Constructs a plane from a point on in and its normal vector
	static inline Plane FromPointAndNormal(
		const Vector<T, 3>& point,
		const Vector<T, 3>& normal
	)
	{
		return Plane(FromPointAndNormal_(), point, normal);
	}

	/// Retuns the plane's equation parameters
	const Vector<T, 4>& Equation(void) const
	{
		return _equation;
	}
};

#if OGLPLUS_DOCUMENTATION_ONLY || defined(GL_FLOAT)
/// Instantiation of Plane using GL floating-point as underlying type
typedef Plane<GLfloat> Planef;
#endif

} // namespace oglplus

#endif // include guard

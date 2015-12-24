/**
 *  @file oglplus/math/quaternion.hpp
 *  @brief A quaternion class
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_MATH_QUATERNION_1310291021_HPP
#define OGLPLUS_MATH_QUATERNION_1310291021_HPP

#include <oglplus/math/angle.hpp>
#include <oglplus/math/vector.hpp>
#include <oglplus/math/slerp.hpp>

namespace oglplus {

template <typename T>
class Quaternion;

template <typename T>
class QuaternionSLERP;

#if OGLPLUS_DOCUMENTATION_ONLY || defined(GL_FLOAT)
/// Float quaternion
/**
 *  @ingroup math_utils
 */
typedef Quaternion<GLfloat> Quatf;
typedef QuaternionSLERP<GLfloat> QuatfSLERP;
#endif

#if OGLPLUS_DOCUMENTATION_ONLY || defined(GL_DOUBLE)
/// Double-precision quaternion
/**
 *  @ingroup math_utils
 */
typedef Quaternion<GLdouble> Quatd;
typedef QuaternionSLERP<GLfloat> QuatdSLERP;
#endif

/// Template class for quaternions
/**
 *  @ingroup math_utils
 */
template <typename T>
class Quaternion
{
private:
	T _a, _x, _y, _z;
public:
	/// Constructs a quaternion
	Quaternion(T a, T x, T y, T z)
	 : _a(a)
	 , _x(x)
	 , _y(y)
	 , _z(z)
	{ }

	/// Construct a Quaternion from an @p axis and an @p angle
	Quaternion(Vector<T, 3> axis, Angle<T> angle)
	{
		axis.Normalize();
		angle /= T(2);
		T sx = Sin(angle);
		_a = Cos(angle);
		_x = sx*axis.x();
		_y = sx*axis.y();
		_z = sx*axis.z();
	}

	/// Construct q Quaternion from the @p real and @p imag parts
	Quaternion(T real, const Vector<T, 3>& imag)
	 : _a(real)
	 , _x(imag.x())
	 , _y(imag.y())
	 , _z(imag.z())
	{ }

	/// Returns the real scalar part of the quaternion
	T Real(void) const
	{
		return _a;
	}

	/// Returns the imaginary vector part of the quaternion
	Vector<T, 3> Imag(void) const
	{
		return Vector<T, 3>(_x, _y, _z);
	}

	T At(std::size_t index) const
	{
		assert(index < 4);
		if(index == 0) return _a;
		if(index == 1) return _x;
		if(index == 2) return _y;
		if(index == 3) return _z;
		return T(0);
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	friend T Dot(const Quaternion& q1, const Quaternion& q2);
#endif

	static T DotProduct(const Quaternion& q1, const Quaternion& q2)
	{
		return q1._a*q2._a + q1._x*q2._x + q1._y*q2._y + q1._z*q2._z;
	}

	/// Returns the magnitude of the quaternion
	T Magnitude(void) const
	{
		return std::sqrt(DotProduct(*this, *this));
	}

	/// Returns true if the quaternion has unit Magnitude
	/**
	 *  @see IsNormal
	 *  @see Normalize
	 */
	bool IsUnit(T eps = T(0)) const
	{
		return std::abs(DotProduct(*this, *this) - T(1)) <= eps;
	}

	/// Synonym for IsUnit
	/**
	 *  @see IsUnit
	 *  @see Normalize
	 */
	bool IsNormal(T eps = T(0)) const
	{
		return IsUnit(eps);
	}

	/// Returns true if the quaternion has zero Magnitude
	bool IsDegenerate(T eps = T(0)) const
	{
		return DotProduct(*this, *this) <= eps;
	}

	/// Normalizes this quaternion
	/**
	 *  @pre !this->IsDegenerate();
	 *  @post this->IsUnit()
	 */
	Quaternion& Normalize(void)
	{
		T im = Magnitude();
		assert(im != T(0));
		im = T(1)/im;
		_a *= im;
		_x *= im;
		_y *= im;
		_z *= im;
		return *this;
	}

	/// Equality comparison
	friend bool Equal(const Quaternion& q1, const Quaternion& q2)
	{
		return	(q1._a == q2._a) &&
			(q1._x == q2._x) &&
			(q1._y == q2._y) &&
			(q1._z == q2._z);
	}

	/// Near-equality comparison
	friend bool Close(const Quaternion& q1, const Quaternion& q2, T eps)
	{
		for(std::size_t i=0; i!=4; ++i)
		{
			T u = q1.At(i);
			T v = q2.At(i);
			T d = std::abs(u-v);
			bool ca = d <= std::abs(u)*eps;
			bool cb = d <= std::abs(v)*eps;
			if(!ca && !cb) return false;
		}
		return true;
	}

	/// Equality comparison
	friend bool operator == (const Quaternion& q1, const Quaternion& q2)
	{
		return Equal(q1, q2);
	}

	/// Non-equality comparison
	friend bool operator != (const Quaternion& q1, const Quaternion& q2)
	{
		return !Equal(q1, q2);
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Conjugate quaternion
	friend Quaternion Conjugate(const Quaternion& q1);
#endif

	static Quaternion Conjugate(const Quaternion& q1)
	{
		return Quaternion(q1._a,-q1._x,-q1._y,-q1._z);
	}

	/// Conjugation
	friend Quaternion operator ~ (const Quaternion& q1)
	{
		return Conjugate(q1);
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Inverse quaternion
	friend Quaternion Inverse(const Quaternion& q1);
#endif

	static Quaternion Inverse(const Quaternion& q1)
	{
		T id = DotProduct(q1, q1);
		assert(id != T(0));
		id *= T(1)/id;
		return Quaternion(
			+q1._a*id,
			-q1._x*id,
			-q1._y*id,
			-q1._z*id
		);
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Quaternion addition
	friend Quaternion Add(const Quaternion& q1, const Quaternion& q2);
#endif

	static Quaternion Added(const Quaternion& q1, const Quaternion& q2)
	{
		return Quaternion(
			q1._a+q2._a,
			q1._x+q2._x,
			q1._y+q2._y,
			q1._z+q2._z
		);
	}

	/// Addition operator
	friend Quaternion operator + (const Quaternion& q1, const Quaternion& q2)
	{
		return Added(q1, q2);
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Quaternion multiplication
	friend Quaternion Multiply(const Quaternion& q1, const Quaternion& q2);
#endif

	static Quaternion Multiplied(const Quaternion& q1, const Quaternion& q2)
	{
		return Quaternion(
			q1._a*q2._a - q1._x*q2._x - q1._y*q2._y - q1._z*q2._z,
			q1._a*q2._x + q1._x*q2._a + q1._y*q2._z - q1._z*q2._y,
			q1._a*q2._y - q1._x*q2._z + q1._y*q2._a + q1._z*q2._x,
			q1._a*q2._z + q1._x*q2._y - q1._y*q2._x + q1._z*q2._a
		);
	}

	/// Multiplication operator
	friend Quaternion operator * (const Quaternion& q1, const Quaternion& q2)
	{
		return Multiplied(q1, q2);
	}

	static Quaternion Multiplied(const Quaternion& q1, T t)
	{
		return Quaternion(q1._a*t, q1._x*t, q1._y*t, q1._z*t);
	}

	/// Multiplication by scalar operator
	friend Quaternion operator * (const Quaternion& q1, T t)
	{
		return Multiplied(q1, t);
	}

	/// Multiplication by scalar operator
	friend Quaternion operator * (T t, const Quaternion& q1)
	{
		return Multiplied(q1, t);
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Rotate a vector by this quaternion
	/**
	 *  @pre this->IsNormal()
	 */
	friend Vector<T, 3> Rotate(const Quaternion& q, const Vector<T, 3>& v);
#endif

	static Vector<T, 3> RotateVector(
		const Quaternion& q,
		const Vector<T, 3>& v
	)
	{
		return (q*Quaternion(T(0), v)*Conjugate(q)).Imag();
	}
};

template <typename T>
inline T Dot(const Quaternion<T>& q1, const Quaternion<T>& q2)
{
	return Quaternion<T>::DotProduct(q1, q2);
}

template <typename T>
inline T Magnitude(const Quaternion<T>& q1)
{
	return q1.Magnitude();
}

template <typename T>
inline Quaternion<T> Conjugate(const Quaternion<T>& q1)
{
	return Quaternion<T>::Conjugate(q1);
}

template <typename T>
inline Quaternion<T> Inverse(const Quaternion<T>& q1)
{
	return Quaternion<T>::Inverse(q1);
}

template <typename T>
inline Quaternion<T> Add(const Quaternion<T>& q1, const Quaternion<T>& q2)
{
	return Quaternion<T>::Added(q1, q2);
}

template <typename T>
inline Quaternion<T> Multiply(const Quaternion<T>& q1, const Quaternion<T>& q2)
{
	return Quaternion<T>::Multiplied(q1, q2);
}

template <typename T>
inline Vector<T, 3> Rotate(const Quaternion<T>& q, const Vector<T, 3>& v)
{
	return Quaternion<T>::RotateVector(q, v);
}

/// Functor template for quaternion spherical-linear interpolation
/** QuaternionSLERP can be used to smoothly interpolate between two
 *  unit quaternions.
 *
 *  @ingroup math_utils
 */
template <typename T>
class QuaternionSLERP
 : public BaseSLERP<Quaternion<T>, T>
{
public:
	/// Constructs a SLERP functor from two unit quaternions
	/**
	 *  @pre q1.IsNormal() && q2.IsNormal()
	 */
	QuaternionSLERP(
		const Quaternion<T>& q1,
		const Quaternion<T>& q2,
		T eps = 0.001
	): BaseSLERP<Quaternion<T>, T>(q1, q2, eps)
	{ }
};

} // namespace oglplus

#endif // include guard

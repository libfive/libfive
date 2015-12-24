/**
 *  @file oglplus/math/angle.hpp
 *  @brief Angle utility class
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_MATH_ANGLE_1107121519_HPP
#define OGLPLUS_MATH_ANGLE_1107121519_HPP

#include <oglplus/config/compiler.hpp>
#include <oglplus/math/constants.hpp>

#include <cassert>
#include <type_traits>

namespace oglplus {

/// Class implementing planar angle-related functionality
/** @c Angle is a lightweight class allowing more natural construction and
 *  usage of planar angle values. The storage requirements are the same
 *  as for the template parameter type @c T, but the @c Angle template gives
 *  the @c T type special meaning and implements a set of angle-related member
 *  and friend functions. There are also several associated free functions
 *  for creating new instances of @c Angle.
 *
 *  @see oglplus::Radians
 *  @see oglplus::Degrees
 *  @see oglplus::FullCircles
 *  @see oglplus::RightAngles
 *  @see oglplus::ArcSin
 *  @see oglplus::ArcCos
 *  @see oglplus::ArcTan
 *
 *  @ingroup math_utils
 */
template <typename T>
class Angle
{
private:
	// the angle value in radians
	T _val_rad;

	struct Radians_ { };
	Angle(T val_rad, Radians_)
	 : _val_rad(val_rad)
	{ }

	struct Degrees_ { };
	Angle(T val_deg, Degrees_)
	 : _val_rad(T(val_deg * (math::Pi() / T(180))))
	{ }
public:
	/// Constructs a zero angle
	Angle(void)
	 : _val_rad(T(0))
	{ }

#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS || OGLPLUS_DOCUMENTATION_ONLY
	/// Angle is copy constructible
	Angle(const Angle&) = default;

	Angle& operator = (const Angle&) = default;

	/// Angle is move constructible
	Angle(Angle&&) = default;
#endif

	/// Copy construction from angles using different underlying type
	template <typename U>
	Angle(const Angle<U>& other)
	 : _val_rad(T(other.Value()))
	{ }

	/// Constructs a new angle from value in radians
	static inline Angle Radians(T val_rad)
	{
		return Angle(val_rad, Radians_());
	}

	/// Constructs a new angle from value in degrees
	static inline Angle Degrees(T val_deg)
	{
		return Angle(val_deg, Degrees_());
	}

	/// Constructs a new angle using arc sine
	static inline Angle ArcSin(T x)
	{
		assert(-1.0f <= x && x <= 1.0f);
		return Angle(::std::asin(x), Radians_());
	}

	/// Constructs a new angle using arc cosine
	static inline Angle ArcCos(T x)
	{
		assert(-1.0f <= x && x <= 1.0f);
		return Angle(::std::acos(x), Radians_());
	}

	/// Returns the value of the angle in radians
	inline T Value(void) const
	{
		return _val_rad;
	}

	/// Returns the value of the angle in degrees
	inline T ValueInDegrees(void) const
	{
		return _val_rad * T(180 / math::Pi());
	}

	/// Returns the value of the angle in number of right angles
	inline T ValueInRightAngles(void) const
	{
		return _val_rad * T(2.0 / math::Pi());
	}

	/// Returns the value of the angle in number of full circles
	inline T ValueInFullCircles(void) const
	{
		return _val_rad * T(0.5 / math::Pi());
	}

	/// Equality comparison
	friend bool operator == (const Angle& a, const Angle& b)
	{
		return a._val_rad == b._val_rad;
	}

	/// Inequality comparison
	friend bool operator != (const Angle& a, const Angle& b)
	{
		return a._val_rad != b._val_rad;
	}

	/// Less than comparison
	friend bool operator <  (const Angle& a, const Angle& b)
	{
		return a._val_rad <  b._val_rad;
	}

	/// Greater than comparison
	friend bool operator >  (const Angle& a, const Angle& b)
	{
		return a._val_rad >  b._val_rad;
	}

	/// Less than/equal comparison
	friend bool operator <= (const Angle& a, const Angle& b)
	{
		return a._val_rad <= b._val_rad;
	}

	/// Greater than/equal comparison
	friend bool operator >= (const Angle& a, const Angle& b)
	{
		return a._val_rad >=  b._val_rad;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Negation
	friend Angle Negate(const Angle& angle);
#endif

	/// Negation
	Angle Negated(void) const
	{
		return Angle(-this->_val_rad, Radians_());
	}

	/// Negation operator
	friend Angle operator - (const Angle& angle)
	{
		return angle.Negated();
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Addition
	friend Angle Add(const Angle& a, const Angle& b);
#endif

	static Angle Added(const Angle& a, const Angle& b)
	{
		return Angle(a._val_rad + b._val_rad, Radians_());
	}

	/// Addition operator
	friend Angle operator + (const Angle& a, const Angle& b)
	{
		return Added(a, b);
	}

	/// Addition operator
	Angle& operator += (const Angle& b)
	{
		*this = Add(*this, b);
		return *this;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Subtraction
	friend Angle Subtract(const Angle& a, const Angle& b);
#endif

	static Angle Subtracted(const Angle& a, const Angle& b)
	{
		return Angle(a._val_rad - b._val_rad, Radians_());
	}

	/// Subtraction operator
	friend Angle operator - (const Angle& a, const Angle& b)
	{
		return Subtracted(a, b);
	}

	/// Subtraction operator
	Angle& operator -= (const Angle& b)
	{
		*this = Subtracted(*this, b);
		return *this;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Multiplication by constant
	friend Angle Multiply(const Angle& a, T mult);
#endif

	static Angle Multiplied(const Angle& a, T mult)
	{
		return Angle(a._val_rad * mult, Radians_());
	}

	/// Multiplication by constant operator
	friend Angle operator * (const Angle& a, T mult)
	{
		return Multiplied(a, mult);
	}

	/// Multiplication by constant operator
	friend Angle operator * (T mult, const Angle& a)
	{
		return Multiplied(a, mult);
	}

	/// Multiplication by constant operator
	Angle& operator *= (T mult)
	{
		*this = Multiplied(*this, mult);
		return *this;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Division by constant
	friend Angle Divide(const Angle& a, T div);
#endif

	static Angle Divided(const Angle& a, T div)
	{
		assert(div != T(0));
		return Angle(a._val_rad / div, Radians_());
	}

	/// Division by constant operator
	friend Angle operator / (const Angle& a, T div)
	{
		return Divided(a, div);
	}

	/// Division by constant operator
	Angle& operator /= (T div)
	{
		*this = Divided(*this, div);
		return *this;
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Angle ratio
	friend T Ratio(const Angle& a, const Angle& b);
#endif

	static T Ratio(const Angle& a, const Angle& b)
	{
		assert(b._val_rad > T(0) || b._val_rad < T(0));
		return a._val_rad / b._val_rad;
	}

	/// Ratio operator
	friend T operator / (const Angle& a, const Angle& b)
	{
		return Ratio(a, b);
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Returns the sine of the angle
	friend inline T Sin(const Angle& a);
#endif

	/// Returns the sine of the angle
	T Sin(void) const
	{
		return ::std::sin(this->_val_rad);
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Returns the cosine of the angle
	friend inline T Cos(const Angle& a);
#endif

	/// Returns the cosine of the angle
	T Cos(void) const
	{
		return ::std::cos(this->_val_rad);
	}

#if OGLPLUS_DOCUMENTATION_ONLY
	/// Returns the tangent of the angle
	friend inline T Tan(const Angle& a);
#endif

	/// Returns the tangent of the angle
	T Tan(void) const
	{
		return ::std::tan(this->_val_rad);
	}
};

template <typename T>
inline Angle<T> Negate(const Angle<T>& a)
{
	return a.Negated();
}

template <typename T>
inline Angle<T> Add(const Angle<T>& a, const Angle<T>& b)
{
	return Angle<T>::Added(a, b);
}

template <typename T>
inline Angle<T> Subtract(const Angle<T>& a, const Angle<T>& b)
{
	return Angle<T>::Subtracted(a, b);
}

template <typename T>
inline Angle<T> Multiply(const Angle<T>& a, T v)
{
	return Angle<T>::Multiplied(a, v);
}

template <typename T>
inline Angle<T> Divide(const Angle<T>& a, T v)
{
	return Angle<T>::Divided(a, v);
}

template <typename T>
inline T Ratio(const Angle<T>& a, const Angle<T>& b)
{
	return Angle<T>::Ratio(a, b);
}

template <typename T>
inline T Radians(const Angle<T>& a)
{
	return a.Value();
}

template <typename T>
inline T Degrees(const Angle<T>& a)
{
	return a.ValueInDegrees();
}

template <typename T>
inline T Sin(const Angle<T>& a)
{
	return a.Sin();
}

template <typename T>
inline T Cos(const Angle<T>& a)
{
	return a.Cos();
}

template <typename T>
inline T Tan(const Angle<T>& a)
{
	return a.Tan();
}

#if OGLPLUS_DOCUMENTATION_ONLY || defined(GL_FLOAT)
/// Instantiation of Angle using GL floating-point as underlying type
typedef Angle<GLfloat> Anglef;

typedef GLfloat AngleValueType;
#elif defined(AL_VERSION_1_1)
typedef ALfloat AngleValueType;
#else
typedef double AngleValueType;
#endif

/// Creates a new angle from a value in radians
/** This function creates a new instance of @c Angle<T>
 *  from a floating-point value in radians.
 *
 *  @param val_rad a value in radians
 *
 *  @see Degrees
 *  @see FullCircles
 *  @see RightAngles
 *  @see ArcSin
 *  @see ArcCos
 *  @see ArcTan
 *
 *  @ingroup math_utils
 */
template <typename T>
static inline
Angle<AngleValueType> Radians(T val_rad)
{
	return Angle<AngleValueType>::Radians(
		AngleValueType(val_rad)
	);
}

/// Creates a new angle from a value in degrees
/** This function creates a new instance of @c Angle<AngleValueType>
 *  from a floating-point value in degrees.
 *  Examples:
 *  @code
 *  // create a 30 degree angle
 *  Degrees(30);
 *  // create a right angle
 *  Degrees(90);
 *  @endcode
 *
 *  @param val_deg a value in degrees
 *
 *  @see Radians
 *  @see FullCircles
 *  @see RightAngles
 *  @see ArcSin
 *  @see ArcCos
 *  @see ArcTan
 *
 *  @ingroup math_utils
 */
template <typename T>
static inline
Angle<AngleValueType> Degrees(T val_deg)
{
	return Angle<AngleValueType>::Degrees(
		AngleValueType(val_deg)
	);
}

/// Creates a new angle from a value in "full circles" (i.e. 360 degrees)
/** This function creates a new angle from a value specifying the fraction
 *  of a full 360 degree (2 pi radians) angle. For example the following
 *  is true:
 *  @code
 *  FullCircles(0.125) == Degrees(45);
 *  FullCircles(0.25) == Degrees(90);
 *  FullCircles(0.25) == Radians(PI / 2);
 *  FullCircles(0.5) == Degrees(180);
 *  FullCircles(0.5) == Radians(PI);
 *  FullCircles(0.9) == Radians(2 * PI * 0.9);
 *  @endcode
 *
 *  @param value a value in 360-degree units
 *
 *  @see Radians
 *  @see Degrees
 *  @see ArcSin
 *  @see ArcCos
 *  @see ArcTan
 *
 *  @ingroup math_utils
 */
template <typename T>
static inline
Angle<AngleValueType> FullCircles(T value)
{
	return Angle<AngleValueType>::Radians(
		AngleValueType(value * math::TwoPi())
	);
}

static inline
Angle<AngleValueType> FullCircle(void)
{
	return Angle<AngleValueType>::Radians(
		AngleValueType(math::TwoPi())
	);
}

/// Creates a new angle from a value in "right angles" (i.e. 90 deg.)
/** This function creates a new angle from a value specifying the fraction
 *  of a quarter 90 degree (pi/2 radians) angle. For example the following
 *  is true:
 *  @code
 *  RightAngles(0.5) == Degrees(45);
 *  RightAngles(1.0) == Degrees(90);
 *  RightAngles(1.0) == Radians(PI / 2);
 *  RightAngles(2.0) == Degrees(180);
 *  RightAngles(2.0) == Radians(PI);
 *  RightAngles(0.9) == Radians(0.9 * PI / 2);
 *  @endcode
 *
 *  @param value a value in 90-degree units
 *
 *  @see Radians
 *  @see Degrees
 *  @see ArcSin
 *  @see ArcCos
 *  @see ArcTan
 *
 *  @ingroup math_utils
 */
template <typename T>
static inline
Angle<AngleValueType> RightAngles(T value)
{
	return Angle<AngleValueType>::Radians(
		AngleValueType(value * math::HalfPi())
	);
}

static inline
Angle<AngleValueType> RightAngle(void)
{
	return Angle<AngleValueType>::Radians(
		AngleValueType(math::HalfPi())
	);
}

/// Creates a new angle using the arc sine function
/**
 *  @param x the value must be between -1.0 and 1.0
 *
 *  @see Radians
 *  @see Degrees
 *  @see FullCircles
 *  @see RightAngles
 *  @see ArcCos
 *  @see ArcTan
 *
 *  @ingroup math_utils
 */
template <typename T>
static inline
Angle<AngleValueType> ArcSin(T x)
{
	return Angle<AngleValueType>::ArcSin(
		AngleValueType(x)
	);
}

/// Creates a new angle using the arc cosine function
/**
 *  @param x the value must be between -1.0 and 1.0
 *
 *  @see Radians
 *  @see Degrees
 *  @see FullCircles
 *  @see RightAngles
 *  @see ArcSin
 *  @see ArcTan
 *
 *  @ingroup math_utils
 */
template <typename T>
static inline
Angle<AngleValueType> ArcCos(T x)
{
	return Angle<AngleValueType>::ArcCos(
		AngleValueType(x)
	);
}

/// Creates a new angle using the arc tangent function
/**
 *
 *  @see Radians
 *  @see Degrees
 *  @see FullCircles
 *  @see RightAngles
 *  @see ArcSin
 *  @see ArcTan
 *
 *  @ingroup math_utils
 */
template <typename T>
static inline
Angle<AngleValueType> ArcTan(T x)
{
	return Angle<AngleValueType>::Radians(
		::std::atan(AngleValueType(x))
	);
}

/// Creates a new angle using the arc tangent function with 2 parameters
/**
 *
 *  @see Radians
 *  @see Degrees
 *  @see FullCircles
 *  @see RightAngles
 *  @see ArcSin
 *  @see ArcTan
 *
 *  @ingroup math_utils
 */
template <typename TY, typename TX>
inline Angle<AngleValueType> ArcTan(TY y, TX x)
{
	return Angle<AngleValueType>::Radians(
		::std::atan2(AngleValueType(y), AngleValueType(x))
	);
}

/// Returns a value on a sine wave at the specified point
/** This function returns the value of sin(2.PI.@p t), i.e.
 *  integer values of @p t are the ends of the previous full
 *  sine wave and the begining of the next "iteration".
 *  The following is true:
 *  @code
 *  SineWave(t) == sin(2.0*PI*t);
 *  SineWave(0.00) ==  0.0;
 *  SineWave(0.25) ==  1.0;
 *  SineWave(0.50) ==  0.0;
 *  SineWave(0.75) == -1.0;
 *  SineWave(1.00) ==  0.0;
 *  @endcode
 *
 *  @param t the point for which to calculate the value on the wave.
 *
 *  @see SineWave01
 *  @see CosineWave
 *
 *  @ingroup math_utils
 */
template <typename T>
inline T SineWave(T t)
{
	return ::std::sin(T(math::TwoPi() * t));
}

/// Returns a value on a sine wave transformed to range <0, 1>
/** This function returns the value of (sin(2.PI.@p t)+1)/2, i.e.
 *  integer values of @p t are the ends of the previous full
 *  sine wave and the begining of the next "iteration".
 *  The following is true:
 *  @code
 *  SineWave01(t) == (sin(2.0*PI*t)+1)/2;
 *  SineWave01(0.00) ==  0.5;
 *  SineWave01(0.25) ==  1.0;
 *  SineWave01(0.50) ==  0.5;
 *  SineWave01(0.75) ==  0.0;
 *  SineWave01(1.00) ==  0.5;
 *  @endcode
 *
 *  @param t the point for which to calculate the value on the wave.
 *
 *  @see SineWave
 *  @see CosineWave01
 *
 *  @ingroup math_utils
 */
template <typename T>
inline T SineWave01(T t)
{
	return (SineWave(t)+T(1))/T(2);
}

/// Returns a value on a cosine wave at the specified point
/** This function returns the value of cos(2.PI.@p t), i.e.
 *  integer values of @p t are the ends of the previous full
 *  cosine wave and the begining of the next "iteration".
 *  The following is true:
 *  @code
 *  CosineWave(t) == cos(2.0*PI*t);
 *  CosineWave(0.00) ==  1.0;
 *  CosineWave(0.25) ==  0.0;
 *  CosineWave(0.50) == -1.0;
 *  CosineWave(0.75) ==  0.0;
 *  CosineWave(1.00) ==  1.0;
 *  @endcode
 *
 *  @param t the point for which to calculate the value on the wave.
 *
 *  @see SineWave
 *  @see CosineWave01
 *
 *  @ingroup math_utils
 */
template <typename T>
inline T CosineWave(T t)
{
	return ::std::cos(T(math::TwoPi() * t));
}

/// Returns a value on a cosine wave transformed to range <0, 1>
/** This function returns the value of (cos(2.PI.@p t)+1)/2, i.e.
 *  integer values of @p t are the ends of the previous full
 *  cosine wave and the begining of the next "iteration".
 *  The following is true:
 *  @code
 *  CosineWave(t) == (cos(2.0*PI*t)+1)/2;
 *  CosineWave(0.00) ==  1.0;
 *  CosineWave(0.25) ==  0.5;
 *  CosineWave(0.50) ==  0.0;
 *  CosineWave(0.75) ==  0.5;
 *  CosineWave(1.00) ==  1.0;
 *  @endcode
 *
 *  @param t the point for which to calculate the value on the wave.
 *
 *  @see CosineWave
 *  @see SineWave01
 *
 *  @ingroup math_utils
 */
template <typename T>
inline T CosineWave01(T t)
{
	return (CosineWave(t)+T(1))/T(2);
}

} // namespace oglplus

#endif // include guard

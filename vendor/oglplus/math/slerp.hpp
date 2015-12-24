/**
 *  @file oglplus/math/slerp.hpp
 *  @brief A template for spherical-linear interpolation
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_MATH_SLERP_1310291021_HPP
#define OGLPLUS_MATH_SLERP_1310291021_HPP

#include <oglplus/config/compiler.hpp>
#include <oglplus/math/angle.hpp>

namespace oglplus {

/// Base template for spherical-linear interpolation functors
/** BaseSLERP implements the basic sperical-linear implementation
 *  for Quaternion or Vector pairs.
 *
 *  @ingroup math_utils
 */
template <typename Value, typename T>
class BaseSLERP
{
private:
	Value _v1, _v2;
	Angle<T> _omega;
	T _inv_sin_omega;

	Value _first(T) const
	{
		return _v1;
	}

	Value _lerp(T t) const
	{
		return _v1*(T(1)-t) + _v2*t;
	}

	Value _slerp(T t) const
	{
		return	_v1*Sin((T(1)-t)*_omega)*_inv_sin_omega+
			_v2*Sin(t*_omega)*_inv_sin_omega;
	}

	Value (BaseSLERP::*_func)(T) const;
public:
	/// Constructs a SLERP functor from two values
	/**
	 *  @pre v1.IsNormal() && v2.IsNormal()
	 */
	BaseSLERP(
		const Value& v1,
		const Value& v2,
		T eps
	): _v1(v1)
	 , _v2(v2)
	 , _omega(Angle<T>::ArcCos(Dot(_v1, _v2)))
	 , _inv_sin_omega(Sin(_omega))
	 , _func(nullptr)
	{
		if(_inv_sin_omega == T(0))
		{
			_func = &BaseSLERP::_first;
		}
		else if(std::abs(_inv_sin_omega) < eps)
		{
			_func = &BaseSLERP::_lerp;
		}
		else
		{
			_func = &BaseSLERP::_slerp;
			_inv_sin_omega = T(1)/_inv_sin_omega;
		}
		assert(_func != nullptr);
	}

	/// Interpolates between the values passed to constructor
	/**
	 *  @pre (param >= 0) && (param <= 1)
	 */
	Value operator()(T param) const
	{
		assert(_func != nullptr);
		return (this->*_func)(param);
	}
};

} // namespace oglplus

#endif // include guard

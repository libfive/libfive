/**
 *  .file oglplus/detail/size.hpp
 *  .brief Implementation of wrapper for sizei
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_DETAIL_SIZE_1501311633_HPP
#define OGLPLUS_DETAIL_SIZE_1501311633_HPP

#include <oglplus/config/compiler.hpp>
#include <oglplus/config/basic.hpp>
#include <type_traits>
#include <stdexcept>
#include <cstddef>
#include <limits>
#include <new>

namespace oglplus {

#if OGLPLUS_LOW_PROFILE

template <typename T>
struct SizeImpl
{
	typedef T Type;
};

#else
template <typename T>
struct SizeImpl
{
private:
	T _v;

	template <typename R, typename X>
	static
	typename std::enable_if<(sizeof(R) <= sizeof(X)), R>::type
	_check2(X v)
	{
		if(v > X(std::numeric_limits<T>::max()))
		{
			throw std::domain_error("Size value too big");
		}
		return R(v);
	}

	template <typename R, typename X>
	static
	typename std::enable_if<(sizeof(R) > sizeof(X)), R>::type
	_check2(X v)
	{
		return R(v);
	}

	template <typename R, typename X>
	static inline
	typename std::enable_if<std::is_signed<X>::value, R>::type
	_check1(X v)
	{
		if(v < X(0))
		{
			throw std::domain_error("Negative size value");
		}
		return _check2<R>(v);
	}

	template <typename R, typename X>
	static inline
	typename std::enable_if<!std::is_signed<X>::value, R>::type
	_check1(X v)
	{
		return _check2<R>(v);
	}

	template <typename X>
	static inline
	T _check(X v)
	{
		return _check1<T>(v);
	}

	template <typename X>
	static
	typename std::enable_if<(sizeof(T) <= sizeof(X)), T>::type
	_conv1(X v)
	OGLPLUS_NOEXCEPT(true)
	{
		if(v > X(std::numeric_limits<T>::max()))
		{
			return T(-1);
		}
		return T(v);
	}

	template <typename X>
	static
	typename std::enable_if<(sizeof(T) > sizeof(X)), T>::type
	_conv1(X v)
	OGLPLUS_NOEXCEPT(true)
	{
		return T(v);
	}

	template <typename X>
	static inline
	T _conv(X v)
	OGLPLUS_NOEXCEPT(true)
	{
		return _conv1(v);
	}
public:
	typedef SizeImpl Type;

	SizeImpl(void)
	OGLPLUS_NOEXCEPT(true)
	 : _v(T(0))
	{ }

	template <typename X>
	SizeImpl(
		X v,
		typename std::enable_if<
			std::is_integral<X>::value
		>::type* = nullptr
	): _v(_check(v))
	{ }

	SizeImpl(T v, std::nothrow_t)
	OGLPLUS_NOEXCEPT(true)
	 : _v(v)
	{ }

	template <typename X>
	SizeImpl(
		X v,
		std::nothrow_t,
		typename std::enable_if<
			std::is_integral<X>::value
		>::type* = nullptr
	) OGLPLUS_NOEXCEPT(true)
	 : _v(_conv(v))
	{ }

	T get(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v;
	}

	OGLPLUS_EXPLICIT
	operator bool (void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v >= T(0);
	}

	bool operator ! (void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _v < T(0);
	}

	operator T (void) const
	{
		return _check<T>(_v);
	}

	template <typename X>
	OGLPLUS_EXPLICIT
	operator X (void) const
	{
		return _check1<X>(_v);
	}

	std::size_t st(void) const
	{
		return _check1<std::size_t>(_v);
	}

	unsigned u(void) const
	{
		return _check1<unsigned>(_v);
	}

	friend bool operator == (SizeImpl s1, SizeImpl s2)
	{
		return _check<T>(s1._v) == _check<T>(s2._v);
	}

	friend bool operator == (T v, SizeImpl s)
	{
		return _check<T>(v) == _check<T>(s._v);
	}

	friend bool operator == (SizeImpl s, T v)
	{
		return _check<T>(s._v) == _check<T>(v);
	}


	friend bool operator != (SizeImpl s1, SizeImpl s2)
	{
		return _check<T>(s1._v) != _check<T>(s2._v);
	}

	friend bool operator != (T v, SizeImpl s)
	{
		return _check<T>(v) != _check<T>(s._v);
	}

	friend bool operator != (SizeImpl s, T v)
	{
		return _check<T>(s._v) != _check<T>(v);
	}


	friend bool operator <  (SizeImpl s1, SizeImpl s2)
	{
		return _check<T>(s1._v) <  _check<T>(s2._v);
	}

	friend bool operator <  (T v, SizeImpl s)
	{
		return _check<T>(v) <  _check<T>(s._v);
	}

	friend bool operator <  (SizeImpl s, T v)
	{
		return _check<T>(s._v) <  _check<T>(v);
	}


	friend bool operator <= (SizeImpl s1, SizeImpl s2)
	{
		return _check<T>(s1._v) <= _check<T>(s2._v);
	}

	friend bool operator <= (T v, SizeImpl s)
	{
		return _check<T>(v) <= _check<T>(s._v);
	}

	friend bool operator <= (SizeImpl s, T v)
	{
		return _check<T>(s._v) <= _check<T>(v);
	}


	friend bool operator >  (SizeImpl s1, SizeImpl s2)
	{
		return _check<T>(s1._v) >  _check<T>(s2._v);
	}

	friend bool operator >  (T v, SizeImpl s)
	{
		return _check<T>(v) >  _check<T>(s._v);
	}

	friend bool operator >  (SizeImpl s, T v)
	{
		return _check<T>(s._v) >  _check<T>(v);
	}


	friend bool operator >= (SizeImpl s1, SizeImpl s2)
	{
		return _check<T>(s1._v) >= _check<T>(s2._v);
	}

	friend bool operator >= (T v, SizeImpl s)
	{
		return _check<T>(v) >= _check<T>(s._v);
	}

	friend bool operator >= (SizeImpl s, T v)
	{
		return _check<T>(s._v) >= _check<T>(v);
	}


	friend T operator + (SizeImpl s1, SizeImpl s2)
	{
		return _check<T>(s1._v) + _check<T>(s2._v);
	}

	friend T operator + (SizeImpl s, T v)
	{
		return _check<T>(s._v) + _check<T>(v);
	}

	friend T operator + (T v, SizeImpl s)
	{
		return _check<T>(v) + _check<T>(s._v);
	}

	friend T operator - (SizeImpl s1, SizeImpl s2)
	{
		return _check<T>(s1._v) - _check<T>(s2._v);
	}

	friend T operator - (SizeImpl s, T v)
	{
		return _check<T>(s._v) - _check<T>(v);
	}

	friend T operator - (T v, SizeImpl s)
	{
		return _check<T>(v) - _check<T>(s._v);
	}

	friend T operator * (SizeImpl s1, SizeImpl s2)
	{
		return _check<T>(s1._v) * _check<T>(s2._v);
	}

	friend T operator * (SizeImpl s, T v)
	{
		return _check<T>(s._v) * _check<T>(v);
	}

	friend T operator * (T v, SizeImpl s)
	{
		return _check<T>(v) * _check<T>(s._v);
	}

	friend T operator / (SizeImpl s1, SizeImpl s2)
	{
		return _check<T>(s1._v) / _check<T>(s2._v);
	}

	friend T operator / (SizeImpl s, T v)
	{
		return _check<T>(s._v) / _check<T>(v);
	}

	friend T operator / (T v, SizeImpl s)
	{
		return _check<T>(v) / _check<T>(s._v);
	}

	friend T operator % (SizeImpl s1, SizeImpl s2)
	{
		return _check<T>(s1._v) % _check<T>(s2._v);
	}

	friend T operator % (SizeImpl s, T v)
	{
		return _check<T>(s._v) % _check<T>(v);
	}

	friend T operator % (T v, SizeImpl s)
	{
		return _check<T>(v) % _check<T>(s._v);
	}
};
#endif // OGLPLUS_LOW_PROFILE

} // namespace oglplus

#endif // include guard

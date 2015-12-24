/**
 *  .file oglplus/math/compile_time.hpp
 *  .brief Compile-time math utilities
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_MATH_COMPILE_TIME_1107121519_HPP
#define OGLPLUS_MATH_COMPILE_TIME_1107121519_HPP

#include <type_traits>

namespace oglplus {
namespace math {

template <unsigned N>
struct Factorial
{
	static const unsigned value = Factorial<N-1>::value*N;
};

template <>
struct Factorial<0u>
{
	static const unsigned value = 1;
};

template <unsigned N, unsigned K>
struct Binomial
{
	static_assert(N > 0, "N must be non-zero");
	static_assert(K <= N, "K must be between 0 and N-1");
	static const unsigned value =
		(Factorial<N>::value) /
		(Factorial<N-K>::value * Factorial<K>::value);
};

template <typename T>
static T Pow(T, std::integral_constant<unsigned, 0u>)
{
	return T(1);
}

template <typename T, unsigned N>
static T Pow(T v, std::integral_constant<unsigned, N>)
{
	return v * Pow<T>(v, std::integral_constant<unsigned, N-1>());
}


// T - interpolated type
// P - parameter type [0, 1]
// N - degree, i.e. linear = 1, quadratic = 2, cubic = 3, ...
template <typename T, typename P, unsigned N>
struct Bezier
{
private:
	template <unsigned M, unsigned I>
	static P _bi(
		std::integral_constant<unsigned, M>,
		std::integral_constant<unsigned, I>,
		P t
	)
	{
		return Binomial<M, I>::value *
			Pow<P>(t, std::integral_constant<unsigned, I>()) *
			Pow<P>(P(1)-t, std::integral_constant<unsigned, M-I>());
	}

	// f(t)
	template <unsigned I>
	static T _f(
		std::integral_constant<unsigned, 0> /*derivation*/,
		std::integral_constant<unsigned, I> i,
		const T* v,
		P t
	)
	{
		std::integral_constant<unsigned, N> n;
		return _bi(n, i, t) * v[I];
	}


	// f'(t)
	template <unsigned I>
	static T _f(
		std::integral_constant<unsigned, 1> /*derivation*/,
		std::integral_constant<unsigned, I> i,
		const T* v,
		P t
	)
	{
		std::integral_constant<unsigned, N-1> n_1;
		return _bi(n_1, i, t) * N * (v[I+1] - v[I]);
	}


	// f''(t)
	template <unsigned I>
	static T _f(
		std::integral_constant<unsigned, 2> /*derivation*/,
		std::integral_constant<unsigned, I> i,
		const T* v,
		P t
	)
	{
		std::integral_constant<unsigned, N-2> n_2;
		return _bi(n_2, i, t) * N*(N - 1) * (v[I+2] - 2*v[I+1] + v[I]);
	}

	template <unsigned D>
	static T _sum(
		std::integral_constant<unsigned, D> d,
		std::integral_constant<unsigned, 0> i,
		const T* v,
		P t
	)
	{
		return _f(d, i, v, t);
	}

	template <unsigned D, unsigned I>
	static T _sum(
		std::integral_constant<unsigned, D> d,
		std::integral_constant<unsigned, I> i,
		const T* v,
		P t
	)
	{
		std::integral_constant<unsigned, I-1> i_1;
		return _sum(d, i_1, v, t) + _f(d, i, v, t);
	}

public:
	template <unsigned D>
	static T B(
		std::integral_constant<unsigned, D> d,
		const T* v,
		size_t s,
		P t
	)
	{
		OGLPLUS_FAKE_USE(s);
		assert(s >= N);
		std::integral_constant<unsigned, N-D> n_d;
		return _sum(d, n_d, v, t);
	}

	static T Position(const T* v, size_t s, P t)
	{
		std::integral_constant<unsigned, 0> d;
		return B(d, v, s, t);
	}

	static T Derivative1(const T* v, size_t s, P t)
	{
		std::integral_constant<unsigned, 1> d;
		return B(d, v, s, t);
	}

	static T Derivative2(const T* v, size_t s, P t)
	{
		std::integral_constant<unsigned, 2> d;
		return B(d, v, s, t);
	}
};

// T - interpolated type
// W - weight type
// P - parameter type [0, 1]
// N - degree, i.e. linear = 1, quadratic = 2, cubic = 3, ...
template <typename T, typename W, typename P, unsigned N>
struct NURBS
{
private:
	template <unsigned I>
	static T _part(
		std::integral_constant<unsigned, I>,
		const T* v,
		const W* w,
		P t
	)
	{
		return Binomial<N, I>::value *
			Pow<P>(t, std::integral_constant<unsigned, I>()) *
			Pow<P>(P(1)-t, std::integral_constant<unsigned, N-I>())*
			v[I]*w[I];
	}

	static T _sum(
		std::integral_constant<unsigned, 0> i,
		const T* v,
		const W* w,
		P t
	)
	{
		return _part(i, v, w, t);
	}

	template <unsigned I>
	static T _sum(
		std::integral_constant<unsigned, I> i,
		const T* v,
		const W* w,
		P t
	)
	{
		std::integral_constant<unsigned, I-1> i_1;
		return _sum(i_1, v, w, t) + _part(i, v, w, t);
	}
	template <unsigned I>
	static W _part(std::integral_constant<unsigned, I>, const W* w, P t)
	{
		return Binomial<N, I>::value *
			Pow<P>(t, std::integral_constant<unsigned, I>()) *
			Pow<P>(P(1)-t, std::integral_constant<unsigned, N-I>())*
			w[I];
	}

	static W _sum(std::integral_constant<unsigned, 0> i, const W* w, P t)
	{
		return _part(i, w, t);
	}

	template <unsigned I>
	static W _sum(std::integral_constant<unsigned, I> i, const W* w, P t)
	{
		std::integral_constant<unsigned, I-1> i_1;
		return _sum(i_1, w, t) + _part(i, w, t);
	}
public:
	static T Calc(const T* v, const W* w, size_t s, P t)
	{
		assert(s >= N);
		std::integral_constant<unsigned, N> n;
		return _sum(n, v, w, t)*(W(1)/_sum(n, w, t));
	}
};

} // namespace math
} // namespace oglplus

#endif // include guard

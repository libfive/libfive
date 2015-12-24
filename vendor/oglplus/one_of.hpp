/**
 *  @file oglplus/one_of.hpp
 *  @brief Variant helper class used mostly with enums
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_ONE_OF_1107121519_HPP
#define OGLPLUS_ONE_OF_1107121519_HPP

#include <oglplus/config/compiler.hpp>

#include <type_traits>
#include <tuple>

namespace oglplus {
namespace aux {

template <typename Common, typename T>
class OneOfBase
{
	static_assert(
		!std::is_same<T, void>::value,
		"The passed value has not one of the allowed types!"
	);
protected:
	static inline Common Accept(T value)
	{
		return Common(value);
	}
};

} // namespace aux


#if OGLPLUS_DOCUMENTATION_ONLY
/// Stores a value having one of the listed types in a common representation.
/**
 *  Applications rarely need to use this class directly. Instantiations of this
 *  template are used as types for parameters in functions taking values with
 *  variant types.
 *  When constructing a one-of value the application simply passes a value
 *  whose type must be one of the types listed in the @c Variants template
 *  parameter pack, otherwise the construction will fail.
 *
 *  @code
 *  Context gl;
 *  ...
 *  gl.DrawBuffer(ColorBuffer::FrontLeft);
 *  ...
 *  gl.DrawBuffer(FramebufferColorAttachment::_0);
 *  @endcode
 */
template <typename Common, typename Variants>
class OneOf
{
public:
	/// OneOf is convertible to the Common representation of the Variants
	operator Common(void) const;
};
#else
template <typename Common, typename TypeList>
class OneOf;
#endif

// Specialization for the common case of two variants
template <typename Common, typename V1, typename V2>
class OneOf<Common, std::tuple<V1, V2>>
{
private:
	Common _value;
public:
	OneOf(V1 value)
	 : _value(Common(value))
	{ }

	OneOf(V2 value)
	 : _value(Common(value))
	{ }

	template <typename T>
	OneOf(
		T value,
		typename std::enable_if<
			std::is_convertible<T, V1>::value
		>::type* = nullptr
	): _value(Common(V1(value)))
	{ }

	template <typename T>
	OneOf(
		T value,
		typename std::enable_if<
			std::is_convertible<T, V2>::value
		>::type* = nullptr
	): _value(Common(V2(value)))
	{ }

	OGLPLUS_EXPLICIT operator Common (void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _value;
	}
};

// Specialization for the common case of three variants
template <typename Common, typename V1, typename V2, typename V3>
class OneOf<Common, std::tuple<V1, V2, V3>>
{
private:
	Common _value;
public:
	OneOf(V1 value)
	 : _value(Common(value))
	{ }

	OneOf(V2 value)
	 : _value(Common(value))
	{ }

	OneOf(V3 value)
	 : _value(Common(value))
	{ }

	template <typename T>
	OneOf(
		T value,
		typename std::enable_if<
			std::is_convertible<T, V1>::value
		>::type* = nullptr
	): _value(Common(V1(value)))
	{ }

	template <typename T>
	OneOf(
		T value,
		typename std::enable_if<
			std::is_convertible<T, V2>::value
		>::type* = nullptr
	): _value(Common(V2(value)))
	{ }

	template <typename T>
	OneOf(
		T value,
		typename std::enable_if<
			std::is_convertible<T, V3>::value
		>::type* = nullptr
	): _value(Common(V3(value)))
	{ }

	OGLPLUS_EXPLICIT operator Common (void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _value;
	}
};

#if !OGLPLUS_NO_VARIADIC_TEMPLATES
template <typename Common, typename ... Variants>
class OneOf<Common, std::tuple<Variants...>>
 : public aux::OneOfBase<Common, Variants>...
{
private:
	Common _value;

	template <typename T>
	struct is_one_of
	 : std::is_convertible<
		OneOf,
		aux::OneOfBase<Common, T>
	>
	{ };

	template <typename T, typename V, typename ... Vi>
	static V _do_find_one_of(std::true_type);

	template <typename T, typename V>
	static void _do_find_one_of(std::false_type);

	template <typename T, typename V, typename ... Vi>
	static auto _find_one_of(void) ->
	decltype(_do_find_one_of<T, V, Vi...>(std::is_convertible<T, V>()));

	template <typename T, typename V, typename V1, typename ... Vi>
	static auto _do_find_one_of(std::false_type) ->
	decltype(_find_one_of<T, V1, Vi...>());

	template <typename T>
	struct find
	{
		typedef decltype(_find_one_of<T, Variants...>()) type;
	};
public:
	template <typename T>
	OneOf(
		T value,
		typename std::enable_if<is_one_of<T>::value>::type* = nullptr
	): _value(aux::OneOfBase<Common, T>::Accept(value))
	{ }

	template <typename T>
	OneOf(
		T value,
		typename std::enable_if<!is_one_of<T>::value>::type* = nullptr
	): _value(aux::OneOfBase<Common, typename find<T>::type>::Accept(value))
	{ }

	OGLPLUS_EXPLICIT operator Common (void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _value;
	}
};
#endif // !OGLPLUS_NO_VARIADIC_TEMPLATES

} // namespace oglplus

#endif // include guard

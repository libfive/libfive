/**
 *  @file oglplus/detail/typechecked.hpp
 *  @brief Default implementation of the Typechecked<X> wrapper
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_DETAIL_TYPECHECKED_1405052234_HPP
#define OGLPLUS_DETAIL_TYPECHECKED_1405052234_HPP

#include <oglplus/fwd.hpp>
#include <oglplus/config/compiler.hpp>
#include <utility>

namespace oglplus {

template <typename Tag, typename X>
class TypecheckedImpl;

template <typename X>
class Typechecked
 : public TypecheckedImpl<typename Classify<X>::Tag, X>
{
private:
	typedef TypecheckedImpl<typename Classify<X>::Tag, X> Base;
public:
#if !OGLPLUS_NO_INHERITED_CONSTRUCTORS
	using Base::Base;
#else
	Typechecked(void) { }

	template <typename T1>
	Typechecked(T1&& p1)
	 : Base(std::forward<T1>(p1))
	{ }

	template <typename T1, typename T2>
	Typechecked(T1&& p1, T2&& p2)
	 : Base(std::forward<T1>(p1), std::forward<T2>(p2))
	{ }

	template <typename T1, typename T2, typename T3>
	Typechecked(T1&& p1, T2&& p2, T3&& p3)
	 : Base(std::forward<T1>(p1), std::forward<T2>(p2), std::forward<T3>(p3))
	{ }
#endif
};

} // namespace oglplus

#endif // include guard

/**
 *  @file oglplus/detail/lazy.hpp
 *  @brief Default implementation of the Lazy<X> wrapper
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_DETAIL_LAZY_1405052234_HPP
#define OGLPLUS_DETAIL_LAZY_1405052234_HPP

#include <oglplus/fwd.hpp>
#include <oglplus/config/compiler.hpp>
#include <utility>

namespace oglplus {

template <typename Tag, typename X>
class LazyImpl;

template <typename X>
class Lazy
 : public LazyImpl<typename Classify<X>::Tag, X>
{
private:
	typedef LazyImpl<typename Classify<X>::Tag, X> Base;
public:
#if !OGLPLUS_NO_INHERITED_CONSTRUCTORS
	using Base::Base;
#else
	template <typename T1, typename T2>
	Lazy(T1&& p1, T2&& p2)
	 : Base(std::forward<T1>(p1), std::forward<T2>(p2))
	{ }
#endif
};

} // namespace oglplus

#endif // include guard

/**
 *  @file oglplus/opt/list_init.hpp
 *  @brief Helper classes and function for compile-time container initialization
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_OPT_LIST_INIT_HPP
#define OGLPLUS_OPT_LIST_INIT_HPP

#include <oglplus/utils/type_tag.hpp>
#include <type_traits>
#include <utility>
#include <array>
#include <vector>

namespace oglplus {
namespace aux {

template <typename T, std::size_t I>
class ListInitializerBase
{
private:
	T _value;
	const ListInitializerBase<T, I-1>* _prev;

	friend class ListInitializerBase<T, I+1>;
protected:
	template <typename RandomAccessContainer>
	void _init(RandomAccessContainer& dest) const
	{
		_prev->_init(dest);
		dest[I] = _value;
	}

	template <typename BackInsertionContainer>
	void _push_back(BackInsertionContainer& dest) const
	{
		_prev->_push_back(dest);
		dest.push_back(_value);
	}

	ListInitializerBase(T value, const ListInitializerBase<T, I-1>* prev)
	 : _value(value)
	 , _prev(prev)
	{ }
};

template <typename T>
class ListInitializerBase<T, 0>
{
private:
	T _value;

	friend class ListInitializerBase<T, 1>;

protected:
	template <typename RandomAccessContainer>
	void _init(RandomAccessContainer& dest) const
	{
		dest[0] = _value;
	}

	template <typename BackInsertionContainer>
	void _push_back(BackInsertionContainer& dest) const
	{
		dest.push_back(_value);
	}

	ListInitializerBase(T value)
	 : _value(value)
	{ }
};

template <typename T, std::size_t I>
class ListInitializer
 : public ListInitializerBase<T, I>
{
private:
	typedef ListInitializerBase<T, I> _base;

	friend class ListInitializer<T, I-1>;

	template <typename X>
	static decltype(X(), std::true_type()) _has_def_ctr(TypeTag<X>);

	static std::false_type _has_def_ctr(...);

	static std::array<T, I+1> _result_of_get(std::true_type);

	static std::vector<T> _result_of_get(std::false_type);

	typedef decltype(_result_of_get(_has_def_ctr(TypeTag<T>()))) ResultOfGet;

	template <typename X>
	void _do_get(std::array<X, I+1>& result) const
	{
		this->_init(result);
	}

	void _do_get(std::vector<T>& result) const
	{
		this->_push_back(result);
	}

	template <typename StdContainer>
	StdContainer _do_get_as(TypeTag<StdContainer>, std::true_type) const
	{
		return StdContainer(Get());
	}

	template <typename StdContainer>
	StdContainer _do_get_as(TypeTag<StdContainer>, std::false_type) const
	{
		auto tmp = Get();
		return StdContainer(tmp.begin(), tmp.end());
	}

	// non copyable
	ListInitializer(const ListInitializer&);

	ListInitializer(T value, const ListInitializer<T, I-1>* prev)
	 : _base(value, prev)
	{ }
public:
	ListInitializer(T value)
	 : _base(value)
	{ }

	ListInitializer(ListInitializer&& temp)
	 : _base(temp)
	{ }

	ListInitializer<T, I+1> operator()(T value) const
	{
		return ListInitializer<T, I+1>(value, this);
	}

	ResultOfGet Get(void) const
	{
		ResultOfGet result;
		this->_do_get(result);
		return result;
	}

	template <typename StdContainer>
	StdContainer As(void) const
	{
		return this->_do_get_as(
			TypeTag<StdContainer>(),
			typename std::is_convertible<
				ResultOfGet,
				StdContainer
			>::type()
		);
	}

	std::vector<T> AsVector(void) const
	{
		return As<std::vector<T>>();
	}
};

} // namespace aux

/// Helper class template that can be used for static container initialization
/**
 *  @ingroup utility_classes
 *
 *  Usage:
 *  @code
 *  // creates a standard RandomAccessContainer of std::strings
 *  // containing the values "A", "B", "C", "D", "E"
 *  auto list = ListOf<std::string>("A")("B")("C")("D")("E").Get();
 *  for(auto i=list.begin(), e=list.end(); i!=e; ++i)
 *  {
 *    std::cout << *i << std::endl;
 *  }
 *
 *  // creates a vector of doubles
 *  std::vector<double> v = ListOf<double>(1)(2)(3)(4)(5)(6).As<std::vector<double>>();
 *  @endcode
 *
 *  @see List()
 */
template <typename T>
class ListOf
 : public aux::ListInitializer<T, 0>
{
public:
	ListOf(T value)
	 : aux::ListInitializer<T, 0>(value)
	{ }
};


/// Helper function template that can be used for static container initialization
/**
 *  @ingroup utility_classes
 *
 *  Usage:
 *  @code
 *  // creates a standard RandomAccessContainer of const char*
 *  // containing the values "A", "B", "C", "D", "E"
 *  auto list = List("A")("B")("C")("D")("E").Get();
 *  for(auto i=list.begin(), e=list.end(); i!=e; ++i)
 *  {
 *    std::cout << *i << std::endl;
 *  }
 *
 *  std::vector<int> v = List(1)(2)(3)(4)(5)(6).As<std::vector<int>>();
 *  @endcode
 *
 *  @see ListOf
 */
template <typename T>
inline aux::ListInitializer<T, 0> List(T value)
{
	return aux::ListInitializer<T, 0>(value);
}

} // namespace oglplus

#endif // include guard

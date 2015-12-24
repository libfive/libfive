/**
 *  @file oglplus/opt/ranges.hpp
 *  @brief Range-related algorithms
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_OPT_RANGE_HPP
#define OGLPLUS_OPT_RANGE_HPP

#include <oglplus/config/basic.hpp>
#include <oglplus/detail/base_range.hpp>

namespace oglplus {

/** @defgroup ranges Ranges and range algorithms
 *  In situations where immutable ranges of elements need to be handled,
 *  OGLplus uses classes conforming to the Range concept. The main advantage
 *  of ranges is that basic algorithms working on them can be composed
 *  into complex ones more easily.
 *
 *  OGLplus also implements several algorithms for traversing or manipulating
 *  ranges and a type erasure for ranges.
 *
 *  @note Range algorithms and utilities are not included by @c oglplus/all.hpp.
 *  To use them the @c oglplus/opt/ranges.hpp file must be included.
 */

#if OGLPLUS_DOCUMENTATION_ONLY
/// Wrapper class for all ranges that can be used for Element traversal
/** This class cannot be instantiated and used directly in end-user
 *  code. It is here for documentation purposes only.
 */
class Range
{
public:
	/// The type of value returned by Front
	typedef const Unspecified ValueType;

	/// Copy constructor
	Range(const Range&);

	/// Returns true if the range is empty
	bool Empty(void) const;

	/// Returns the number of Elements in the range
	size_t Size(void) const;

	/// Goes to the next element in the range.
	/** This function moves the front of the range one element ahead.
	 *  The range must not be empty when calling Next, otherwise the
	 *  result is undefined and the application may be aborted.
	 *
	 *  @see Empty
	 */
	void Next(void);

	/// Returns the element at the front of the range
	/** The range must not be empty when calling Front, otherwise the
	 *  result is undefined and the application may be aborted.
	 *
	 *  @see Empty
	 *  @see Next
	 */
	ValueType Front(void);
};
#endif

/// The range algorithms are defined in this namespace
namespace ranges {

/// Metafunction for checking if a type conforms to the oglplus::Range concept.
/**
 *  @ingroup ranges
 */
template <typename Range>
struct IsRange
{
	typedef decltype(oglplus::aux::IsRange((Range*)nullptr)) Type;
};


/// A type erasure for types conforming to the oglplus::Range concept.
/**
 *  @ingroup ranges
 */
template <typename Element>
class AnyRange
{
private:
	struct _intf
	{
		virtual ~_intf(void){ }

		virtual _intf* _clone(void) const = 0;

		virtual bool _empty(void) const = 0;
		virtual std::size_t _size(void) const = 0;
		virtual void _next(void) = 0;
		virtual Element _front(void) const = 0;
	};

	template <class Rng>
	class _impl : public _intf
	{
	private:
		static_assert(IsRange<Rng>::Type::value, "Range is expected");
		Rng _rng;
	public:
		_impl(Rng rng)
		 : _rng(rng)
		{ }

		_intf* _clone(void) const
		OGLPLUS_OVERRIDE
		{
			return new _impl(*this);
		}

		bool _empty(void) const
		OGLPLUS_OVERRIDE
		{
			return _rng.Empty();
		}

		std::size_t _size(void) const
		OGLPLUS_OVERRIDE
		{
			return _rng.Size();
		}

		void _next(void)
		OGLPLUS_OVERRIDE
		{
			_rng.Next();
		}

		Element _front(void) const
		OGLPLUS_OVERRIDE
		{
			return _rng.Front();
		}
	};

	_intf* _pimpl;

	static _intf* _clone(_intf* pimpl)
	{
		assert(pimpl);
		return pimpl->_clone();
	}
public:
	typedef Element ValueType;

	AnyRange(void)
	 : _pimpl(nullptr)
	{ }

	template <typename Range>
	AnyRange(Range range)
	 : _pimpl(new _impl<Range>(range))
	{ }

	AnyRange(const AnyRange& other)
	 : _pimpl(_clone(other._pimpl))
	{ }

	AnyRange(AnyRange&& temp)
	 : _pimpl(temp._pimpl)
	{
		temp._pimpl = nullptr;
	}

	~AnyRange(void)
	{
		if(_pimpl) delete _pimpl;
	}

	template <typename Range>
	AnyRange& operator = (const Range& range)
	{
		if(_pimpl) delete _pimpl;
		_pimpl = new _impl<Range>(range);
		return *this;
	}

	AnyRange& operator = (const AnyRange& other)
	{
		if(this != &other)
		{
			if(_pimpl) delete _pimpl;
			_pimpl = _clone(other._pimpl);
		}
		return *this;
	}

	AnyRange& operator = (AnyRange&& other)
	{
		if(this != &other)
		{
			if(_pimpl) delete _pimpl;
			_pimpl = other._pimpl;
			other._pimpl = nullptr;
		}
		return *this;
	}

	/// Returns true if the range is empty
	bool Empty(void) const
	{
		if(!_pimpl) return true;
		return _pimpl->_empty();
	}

	/// Returns the number of Elements in the range
	size_t Size(void) const
	{
		if(!_pimpl) return 0;
		return _pimpl->_size();
	}

	/// Goes to the next element in the range.
	void Next(void)
	{
		assert(!Empty());
		_pimpl->_next();
	}

	/// Returns the element at the front of the range
	ValueType Front(void) const
	{
		assert(!Empty());
		return _pimpl->_front();
	}
};

template <typename Range>
inline AnyRange<typename Range::ValueType> EraseType(Range range)
{
	return AnyRange<typename Range::ValueType>(range);
}

/// Executes a functor on every element in a @p range
/**
 *  @ingroup ranges
 */
template <typename Range, typename Func>
inline Func ForEach(Range range, Func func)
{
	static_assert(
		IsRange<Range>::Type::value,
		"A Range is expected as the first argument"
	);
	while(!range.Empty())
	{
		func(range.Front());
		range.Next();
	}
	return func;
}

/// Finds the specified value in a range
/** This function traverses a range and stops either if the range
 *  is empty or if the specified value is found. The resulting
 *  range (either empty or having the specified value as the
 *  element at the front) is returned.
 *
 *  @ingroup ranges
 */
template <typename Range>
inline Range Find(Range range, typename Range::ValueType value)
{
	static_assert(
		IsRange<Range>::Type::value,
		"A Range is expected as the first argument"
	);
	while(!range.Empty())
	{
		if(range.Front() == value) break;
		range.Next();
	}
	return range;
}

template <typename Range>
inline bool Contains(Range range, typename Range::ValueType value)
{
	static_assert(
		IsRange<Range>::Type::value,
		"A Range is expected as the first argument"
	);
	while(!range.Empty())
	{
		if(range.Front() == value) return true;
		range.Next();
	}
	return false;
}

template <typename Range, typename Predicate>
inline Range& AdvanceUntil(Range& range, Predicate predicate)
{
	static_assert(
		IsRange<Range>::Type::value,
		"A Range is expected as the first argument"
	);
	while(!range.Empty())
	{
		if(predicate(range.Front())) break;
		range.Next();
	}
	return range;
}

template <typename Range, typename Predicate>
inline std::size_t CountIf(Range range, Predicate predicate)
{
	static_assert(
		IsRange<Range>::Type::value,
		"A Range is expected as the first argument"
	);
	std::size_t result = 0;
	while(!range.Empty())
	{
		if(predicate(range.Front())) ++result;
		range.Next();
	}
	return result;
}

/// Finds the first a value satisfying a predicate in a range
/** This function traverses a range and stops either if the range
 *  is empty or if a value satisfying the predicate is found.
 *  The resulting range (either empty or having the found value
 *  as the element at the front) is returned.
 *
 *  @ingroup ranges
 */
template <typename Range, typename Predicate>
inline Range FindIf(Range range, Predicate predicate)
{
	return AdvanceUntil(range, predicate);
}

template <typename Range, typename Predicate>
inline bool Has(Range range, Predicate predicate)
{
	return !AdvanceUntil(range, predicate).Empty();
}

template <typename Range, typename Transf>
class Transformed
{
private:
	Range _range;
	Transf _transf;
public:
	typedef decltype(std::declval<Transf>()(
		std::declval<typename Range::ValueType>())
	) ValueType;

	Transformed(Range range, Transf transf)
	 : _range(range)
	 , _transf(transf)
	{ }

	bool Empty(void) const
	{
		return _range.Empty();
	}

	size_t Size(void) const
	{
		return _range.Size();
	}

	void Next(void)
	{
		_range.Next();
	}

	ValueType Front(void) const
	{
		return _transf(_range.Front());
	}
};

/// Transforms a range by an unary function
/** Transform returns a range whose Front function returns
 *  the Front value of the original range transformed by transf.
 *
 *  @ingroup ranges
 */
template <typename Range, typename Transf>
inline Transformed<Range, Transf> Transform(Range range, Transf transf)
{
	static_assert(
		IsRange<Range>::Type::value,
		"A Range is expected as the first argument"
	);
	return Transformed<Range, Transf>(range, transf);
}

/// Folds the range by using a binary functor and a state value
/** Fold updates the initial state by calling the binary operation
 *  on it and all elements in the range.
 *
 *  @code
 *  State state;
 *  while(!range.Empty())
 *  {
 *      state = op(state, range.Front());
 *      range.Next();
 *  }
 *  @endcode
 *
 *  @ingroup ranges
 */
template <typename Range, typename State, typename Op>
inline State Fold(Range range, State state, Op op)
{
	static_assert(
		IsRange<Range>::Type::value,
		"A Range is expected as the first argument"
	);
	while(!range.Empty())
	{
		state = op(state, range.Front());
		range.Next();
	}
	return state;
}


template <typename Range, typename Predicate>
class Filtered
{
private:
	Range _range;
	Predicate _pred;
public:
	typedef typename Range::ValueType ValueType;

	Filtered(Range range, Predicate pred)
	 : _range(range)
	 , _pred(pred)
	{
		oglplus::ranges::AdvanceUntil(_range, _pred);
	}

	bool Empty(void) const
	{
		return _range.Empty();
	}

	size_t Size(void) const
	{
		return CountIf(*this, _pred);
	}

	void Next(void)
	{
		_range.Next();
		oglplus::ranges::AdvanceUntil(_range, _pred);
	}

	ValueType Front(void) const
	{
		return _range.Front();
	}
};

/// Returns a range containing only elements satisfying a predicate
/** OnlyIf returns a range that contains only those elements of the
 *  original range, which satisfy the specified predicate.
 *
 *  @ingroup ranges
 */
template <typename Range, typename Predicate>
inline Filtered<Range, Predicate> OnlyIf(Range range, Predicate pred)
{
	static_assert(
		IsRange<Range>::Type::value,
		"A Range is expected as the first argument"
	);
	return Filtered<Range, Predicate>(range, pred);
}

template <typename Element, typename R1, typename R2>
class Concatenated
{
private:
	R1 _r1;
	R2 _r2;
public:
	typedef Element ValueType;

	Concatenated(R1 r1, R2 r2)
	 : _r1(r1)
	 , _r2(r2)
	{ }

	bool Empty(void) const
	{
		return _r1.Empty() && _r2.Empty();
	}

	size_t Size(void) const
	{
		return _r1.Size() + _r2.Size();
	}

	void Next(void)
	{
		if(!_r1.Empty())
			_r1.Next();
		else if(!_r2.Empty())
			_r2.Next();
		else assert(!Empty());
	}

	ValueType Front(void) const
	{
		if(!_r1.Empty())
			return ValueType(_r1.Front());
		else if(!_r2.Empty())
			return ValueType(_r2.Front());
		else assert(!Empty());
		return *((ValueType*)nullptr);
	}
};

template <typename Element, typename R1, typename R2>
inline Concatenated<Element, R1, R2> Concatenate(R1 r1, R2 r2)
{
	static_assert(
		IsRange<R1>::Type::value,
		"A Range is expected as the first argument"
	);
	static_assert(
		IsRange<R2>::Type::value,
		"A Range is expected as the second argument"
	);
	return Concatenated<Element, R1, R2>(r1, r2);
}

} // namespace ranges
} // namespace oglplus

#endif // include guard

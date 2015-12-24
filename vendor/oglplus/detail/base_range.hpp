/**
 *  .file oglplus/detail/base_range.hpp
 *  .brief Base class for ranges
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_AUX_BASE_RANGE_1107121519_HPP
#define OGLPLUS_AUX_BASE_RANGE_1107121519_HPP

#include <cassert>
#include <vector>
#include <iterator>
#include <type_traits>

namespace oglplus {
namespace aux {

template <class Range>
std::true_type IsRange(
	const Range*,
	bool (Range::*)(void) const = &Range::Empty,
	std::size_t (Range::*)(void) const = &Range::Size,
	void (Range::*)(void) = &Range::Next,
	typename Range::ValueType (Range::*)(void) const = &Range::Front
);

std::false_type IsRange(...);

template <typename Context, typename Element>
class ContextElementRange
{
private:
	Context _context;
	unsigned _current, _count;
public:
	typedef Element ValueType;

	ContextElementRange(Context&& context, unsigned current, unsigned count)
	 : _context(std::move(context))
	 , _current(current)
	 , _count(count)
	{
		assert(current <= count);
	}

	ContextElementRange(Context&& context, unsigned count)
	 : _context(std::move(context))
	 , _current(0)
	 , _count(count)
	{ }

	ContextElementRange(ContextElementRange&& tmp)
	 : _context(std::move(tmp._context))
	 , _current(tmp._current)
	 , _count(tmp._count)
	{ }

	bool Empty(void) const
	{
		return _current == _count;
	}

	std::size_t Size(void) const
	{
		return _count - _current;
	}

	void Next(void)
	{
		assert(!Empty());
		++_current;
	}

	ValueType Front(void)
	{
		assert(!Empty());
		return Element(_context, _current);
	}

	ValueType At(std::size_t offset)
	{
		assert(offset < Size());
		return Element(_context, _current+offset);
	}
};

template <typename Iterator>
class IterRange
{
private:
	Iterator _pos, _end;
public:
	typedef const typename std::iterator_traits<Iterator>::reference
		ValueType;

	IterRange(Iterator begin, Iterator end)
	 : _pos(begin)
	 , _end(end)
	{ }

	bool Empty(void) const
	{
		return _pos == _end;
	}

	std::size_t Size(void) const
	{
		return _end - _pos;
	}

	void Next(void)
	{
		assert(!Empty());
		++_pos;
	}

	ValueType Front(void) const
	{
		assert(!Empty());
		return *_pos;
	}
};

template <typename Iterator, typename Element>
class CastIterRange
 : public IterRange<Iterator>
{
public:
	typedef Element ValueType;

	CastIterRange(Iterator begin, Iterator end)
	 : IterRange<Iterator>(begin, end)
	{ }

	ValueType Front(void) const
	{
		return Element(IterRange<Iterator>::Front());
	}
};

template <typename Iterator, typename Element, typename Converter>
class ConvIterRange
 : public IterRange<Iterator>
{
private:
	Converter _conv;
public:
	typedef Element ValueType;

	ConvIterRange(Converter conv, Iterator begin, Iterator end)
	 : IterRange<Iterator>(begin, end)
	 , _conv(conv)
	{ }

	Element Front(void) const
	{
		return _conv(IterRange<Iterator>::Front());
	}
};

template <typename Element>
class ArrayRange
 : public CastIterRange<
	std::vector<unsigned>::const_iterator,
	Element
>
{
private:
	typedef std::vector<unsigned>::const_iterator iterator;
	typedef CastIterRange<iterator, Element> _base;
public:
	ArrayRange(iterator i, iterator e)
	 : _base(i, e)
	{ }
};

} // namespace aux
} // namespace oglplus

#endif // include guard

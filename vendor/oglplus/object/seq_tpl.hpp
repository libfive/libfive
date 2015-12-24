/**
 *  @file oglplus/object/sequence.hpp
 *  @brief Sequence of Object names
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_OBJECT_SEQ_TPL_1405011014_HPP
#define OGLPLUS_OBJECT_SEQ_TPL_1405011014_HPP

#include <oglplus/object/name_tpl.hpp>
#include <cstddef>
#include <cassert>

namespace oglplus {

/// Object sequence iterator template
template <typename ObjectT>
class SeqIterator
{
private:
	typedef typename Classify<ObjectT>::ObjTag ObjTag;
	typedef typename ObjTag::NameType NameT;
	const NameT* _pos;
public:
	SeqIterator(const NameT* pos)
	 : _pos(pos)
	{ }

	/// Equality comparison
	friend bool operator == (SeqIterator a, SeqIterator b)
	{
		return a._pos == b._pos;
	}

	/// Inequality comparison
	friend bool operator != (SeqIterator a, SeqIterator b)
	{
		return a._pos != b._pos;
	}

	/// Ordering
	friend bool operator <  (SeqIterator a, SeqIterator b)
	{
		return a._pos <  b._pos;
	}

	/// Value type
	typedef ObjectT value_type;

	/// Difference type
	typedef std::ptrdiff_t difference_type;

	/// Distance
	friend difference_type operator - (SeqIterator a, SeqIterator b)
	{
		return a._pos - b._pos;
	}

	/// Dereference
	value_type operator * (void) const
	{
		assert(_pos != nullptr);
		return ObjectT(ObjectName<ObjTag>(*_pos));
	}

	/// Array access
	value_type operator [](std::size_t index) const
	{
		assert(_pos != nullptr);
		return ObjectT(ObjectName<ObjTag>(_pos[index]));
	}

	/// Preincrement
	SeqIterator& operator ++ (void)
	{
		++_pos;
		return *this;
	}

	/// Postincrement
	SeqIterator operator ++ (int)
	{
		return SeqIterator(_pos++);
	}

	/// Predecrement
	SeqIterator& operator -- (void)
	{
		--_pos;
		return *this;
	}

	/// Postdecrement
	SeqIterator operator -- (int)
	{
		return SeqIterator(_pos--);
	}

	/// Positive offset
	friend SeqIterator operator + (SeqIterator a, difference_type d)
	{
		return SeqIterator(a._pos+d);
	}

	/// Positive offset
	SeqIterator& operator += (difference_type d)
	{
		_pos += d;
		return *this;
	}

	/// Negative offset
	friend SeqIterator operator - (SeqIterator a, difference_type d)
	{
		return SeqIterator(a._pos-d);
	}

	/// Negative offset
	SeqIterator& operator -= (difference_type d)
	{
		_pos -= d;
		return *this;
	}
};

template <typename ObjectT>
const typename Classify<ObjectT>::ObjTag::NameType*
GetNames(const Sequence<ObjectT>&);

/// Common base class for Object name sequences
template <typename ObjectT>
class Sequence
{
private:
	typedef typename Classify<ObjectT>::ObjTag ObjTag;
	typedef typename ObjTag::NameType NameT;

	friend const NameT* GetNames<ObjectT>(const Sequence&);

	const NameT* _names;
	std::size_t _size;
public:
	Sequence(void)
	 : _names(nullptr)
	 , _size(0)
	{ }

	Sequence(const NameT* names, std::size_t count)
	 : _names(names)
	 , _size(count)
	{ }

	/// Returns true if the sequence is empty
	bool empty(void) const
	{
		return _size == 0;
	}

	/// Returns the size of the sequence
	std::size_t size(void) const
	{
		return _size;
	}

	/// Returns the object name at the specified @p index
	ObjectT at(std::size_t index) const
	{
		assert(index < _size);
		return ObjectT(_names[index]);
	}

	/// Returns the object name at the specified @p index
	ObjectT operator[](std::size_t index) const
	{
		return at(index);
	}

	/// Returns a subsequence starting at @p start
	Sequence slice(std::size_t start) const
	{
		assert(start <= _size);
		return Sequence(_names+start, _size-start);
	}

	/// Returns a subsequence with the specified @p count starting at @p start
	Sequence slice(std::size_t start, std::size_t count) const
	{
		assert(start + count <= _size);
		return Sequence(_names+start, count);
	}

	/// Iterator type
	typedef SeqIterator<ObjectT> iterator;
	/// Const iterator type
	typedef SeqIterator<ObjectT> const_iterator;

	/// Position at the beginning of the sequence
	const_iterator begin(void) const
	{
		return const_iterator(_names);
	}

	/// Position past the end of the sequence
	const_iterator end(void) const
	{
		return const_iterator(_names+_size);
	}
};

/// Returns a pointer to array of names stored in a @p sequence
template <typename ObjectT>
inline const typename Classify<ObjectT>::ObjTag::NameType*
GetNames(const Sequence<ObjectT>& sequence)
{
	return sequence._names;
}

} // namespace oglplus

#endif // include guard

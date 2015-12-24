/**
 *  @file oglplus/detail/attrib_list.hpp
 *  @brief OpenGL/OpenAL/EGL/... configuration attribute list
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2013 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_AUX_ATTRIB_LIST_1303292057_HPP
#define OGLPLUS_AUX_ATTRIB_LIST_1303292057_HPP

#include <oglplus/bitfield.hpp>

#include <cassert>
#include <vector>

namespace oglplus {

/// Stores a finished attribute list create by AttributeList
template <typename AttribKind, class Traits> // <- do not remove this is intentional
class FinishedAttributeList
{
private:
	typedef typename Traits::Int Int;
	Traits _traits;
	std::vector<Int> _attribs;
public:
	FinishedAttributeList(const std::vector<Int>& attribs)
	 : _attribs(attribs)
	{
		if(!_attribs.empty() && (_attribs.back() != _traits.ListEnd()))
		{
			_attribs.push_back(_traits.ListEnd());
		}

		assert(
			(_attribs.empty()) || (
				(_attribs.size() % 2 == 1) &&
				(_attribs.back() == _traits.ListEnd())
			)
		);
	}

	FinishedAttributeList(const FinishedAttributeList& that)
	 : _attribs(that._attribs)
	{ }

	FinishedAttributeList(FinishedAttributeList&& tmp)
	 : _attribs(std::move(tmp._attribs))
	{ }

	/// Returns the terminated array of attribute/value pairs
	const Int* Get(void) const
	{
		return _attribs.data();
	}
};

/// Specifies the list of attribute values for configuration selection
template <typename AttribKind, class ValueToAttribMap, class Traits>
class AttributeList
{
private:
	typedef typename Traits::Int Int;
	Traits _traits;
	ValueToAttribMap _value_to_attrib_map;
	std::vector<Int> _attribs;
public:
	/// Creates an empty list of attributes
	AttributeList(void)
	{
		_attribs.reserve(2*10+1);
	}

	/// Adds a new attribute/value pair
	/**
	 *  @pre !Finished()
	 */
	AttributeList& Add(AttribKind attrib, Int value)
	{
		assert(!Finished());
		typedef typename Traits::template EnumBaseType<
			AttribKind
		>::Type Enum;
		_attribs.push_back(Int(Enum(attrib)));
		_attribs.push_back(value);
		return *this;
	}

	/// Adds a new attribute/value pair
	/**
	 *  @pre !Finished()
	 */
	AttributeList& Add(AttribKind attrib, bool value)
	{
		assert(!Finished());
		typedef typename Traits::template EnumBaseType<
			AttribKind
		>::Type Enum;
		_attribs.push_back(Int(Enum(attrib)));
		_attribs.push_back(
			value?
			_traits.TrueValue():
			_traits.FalseValue()
		);
		return *this;
	}

	/// Adds a new enumerated attribute value
	/**
	 *  @pre !Finished()
	 */
	template <typename AttribValueType>
	AttributeList& Add(AttribValueType value)
	{
		typedef typename Traits::template EnumBaseType<
			AttribValueType
		>::Type Enum;
		return Add(
			_value_to_attrib_map(value),
			Int(Enum(value))
		);
	}

	template <typename AttribValueType>
	AttributeList& Add(Bitfield<AttribValueType> bits)
	{
		typedef typename Traits::template EnumBaseType<
			AttribValueType
		>::Type Enum;
		return Add(
			_value_to_attrib_map(AttribValueType()),
			Int(Enum(bits))
		);
	}

	/// Sets the attribute value to don't care
	/**
	 *  @pre !Finished()
	 */
	AttributeList& DontCare(AttribKind attrib)
	{
		assert(!Finished());
		typedef typename Traits::template EnumBaseType<
			AttribKind
		>::Type Enum;
		_attribs.push_back(Int(Enum(attrib)));
		_attribs.push_back(_traits.DontCare());
		return *this;
	}

	/// Returns true if the list of attributes has been finished
	/**
	 *  @see Finish()
	 */
	bool Finished(void) const
	{
		if(_attribs.empty()) return false;
		return _attribs.back() == _traits.ListEnd();
	}

	/// Finishes the list of attributes
	/**
	 *  @post Finished()
	 *  @see Get()
	 *  @see Finished()
	 */
	AttributeList& Finish(void)
	{
		if(!Finished()) _attribs.push_back(_traits.ListEnd());
		return *this;
	}

	/// Returns a finished attribute list
	/**
	 *  @note this function does not call the Finish function
	 *  on this attribute list
	 */
	FinishedAttributeList<AttribKind, Traits> Get(void) const
	{
		return FinishedAttributeList<AttribKind, Traits>(_attribs);
	}
};

} // namespace oglplus

#endif // include guard

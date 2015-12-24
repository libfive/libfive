/**
 *  @file oglplus/object/name_tpl.hpp
 *  @brief Base template for all "named" objects
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_OBJECT_NAME_TPL_1107121519_HPP
#define OGLPLUS_OBJECT_NAME_TPL_1107121519_HPP

#include <oglplus/config/compiler.hpp>

#include <type_traits>
#include <utility>
#include <cassert>

namespace oglplus {
namespace tag {

struct ObjectName;

} // namespace tag

template <typename ObjTag, typename NameHolder>
class ObjectTpl;

template <typename ObjTag>
class ObjHandle;

template <typename ObjTag>
class ObjectName;

template <typename ObjTag>
typename ObjTag::NameType
GetName(ObjectName<ObjTag>);

/// A common template for "named" objects like textures, buffers, etc.
/** This is a common template for all GL/AL/VG/etc. object wrappers which are
 *  identified by a (uint typed) name, i.e. object like Textures, Buffer, VAOs,
 *  Queries, etc. but also Shaders, Programs, and so on.
 *  @c ObjectName adds static object type information and allows to distinguish
 *  between objects of different type with the same name value.
 *
 *  @note Do not use this class directly, it is used by the object wrappers
 *  for basic initialization, error checking and access restriction.
 */
template <typename ObjTag>
class ObjectName
{
protected:
	typedef typename ObjTag::NameType NameT;

private:
	friend NameT GetName<ObjTag>(ObjectName);
	NameT _name;
protected:

	inline
	NameT _obj_name(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return _name;
	}

	inline
	NameT* _name_ptr(void)
	OGLPLUS_NOEXCEPT(true)
	{
		return &_name;
	}

	inline
	const NameT* _name_ptr(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return &_name;
	}

	static inline
	NameT _invalid_name(void)
	OGLPLUS_NOEXCEPT(true)
	{
		return ~NameT(0);
	}

	inline
	bool _has_deletable_name(void)
	OGLPLUS_NOEXCEPT(true)
	{
		// TODO: fix this for objects where Gen*
		// returns a valid name 0
		return (_name > 0u) && (_name != _invalid_name());
	}
public:
	static
	ObjectName InvalidName(void)
	OGLPLUS_NOEXCEPT(true)
	{
		return ObjectName(_invalid_name());
	}

	/// Constructs uninitialized (invalid) name wrapper
	ObjectName(void)
	OGLPLUS_NOEXCEPT(true)
	 : _name(_invalid_name())
	{ }

	/// Constructs wrapper for the specified @p name.
	explicit ObjectName(NameT name)
	OGLPLUS_NOEXCEPT(true)
	 : _name(name)
	{ }

	ObjectName(const ObjectName& that)
	OGLPLUS_NOEXCEPT(true)
	 : _name(that._name)
	{ }

	ObjectName(ObjectName&& temp)
	OGLPLUS_NOEXCEPT(true)
	 : _name(temp._name)
	{
		temp._name = _invalid_name();
	}

	ObjectName& operator = (const ObjectName& that)
	OGLPLUS_NOEXCEPT(true)
	{
		_name = that._name;
		return *this;
	}

	ObjectName& operator = (ObjectName&& temp)
	OGLPLUS_NOEXCEPT(true)
	{
		if(this != &temp)
		{
			_name = temp._name;
			temp._name = _invalid_name();
		}
		return *this;
	}

	/// Returns true if the object name is valid, false otherwise
	bool HasValidName(void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return this->_name != _invalid_name();
	}

	/// Equality comparison
	friend bool operator == (ObjectName a, ObjectName b)
	{
		return a._name == b._name;
	}

	/// Inequality comparison
	friend bool operator != (ObjectName a, ObjectName b)
	{
		return a._name != b._name;
	}

	/// Ordering
	friend bool operator <  (ObjectName a, ObjectName b)
	{
		return a._name < b._name;
	}
};

/// Returns the base name assigned to @p named object
template <typename ObjTag>
inline typename ObjTag::NameType
GetName(ObjectName<ObjTag> named)
{
	return named._name;
}

template <typename X>
struct Classify;

template <typename ObjTg>
struct Classify<ObjectName<ObjTg>>
{
	typedef ObjectName<ObjTg> Base;
	typedef tag::ObjectName Tag;
	typedef ObjTg ObjTag;
};

} // namespace oglplus

#endif // include guard

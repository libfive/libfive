/**
 *  @file oglplus/object/group.hpp
 *  @brief A group of object references
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_OBJECT_GROUP_1405011014_HPP
#define OGLPLUS_OBJECT_GROUP_1405011014_HPP

#include <oglplus/object/seq_tpl.hpp>
#include <oglplus/config/compiler.hpp>
#include <array>
#include <vector>
#include <cassert>

namespace oglplus {

template <typename ObjName>
class Group;

/// A dynamic group (convertible to a @c Sequence) of object names
template <typename ObjTag>
class Group<ObjectName<ObjTag>>
{
private:
	typedef typename ObjTag::NameType NameT;
	std::vector<NameT> _names;
public:
	/// Constructs an empty Group
	Group(void) { }

	Group(ObjectName<ObjTag> a, ObjectName<ObjTag> b)
	{
		_names.push_back(GetName(a));
		_names.push_back(GetName(b));
	}

	Group(const Group& that)
	 : _names(that._names)
	{ }

	Group(Group&& temp)
	 : _names(std::move(temp._names))
	{ }

#if !OGLPLUS_NO_INITIALIZER_LISTS
	/// Constructs the Group from an initializer list
	Group(std::initializer_list<ObjectName<ObjTag>> names)
	{
		_names.reserve(names.size());

		auto i = names.begin();
		while(i != names.end())
		{
			_names.push_back(GetName(*i));
			++i;
		}
	}
#endif

	/// Constructs the Group from a std range supporting begin and end
	template <typename StdRange>
	Group(const StdRange& range)
	{
		using std::begin;
		using std::end;
		using std::distance;

		auto b = begin(range);
		auto e = end(range);

		_names.reserve(distance(b, e));

		auto i = b;
		while(i != e)
		{
			_names.push_back(GetName(*i));
			++i;
		}
	}

	/// Add a new name to this group
	Group& Add(ObjectName<ObjTag> name)
	{
		_names.push_back(GetName(name));
		return *this;
	}


	Sequence<ObjectName<ObjTag>> seq(void) const
	{
		return Sequence<ObjectName<ObjTag>>(
			_names.data(),
			_names.size()
		);
	}

	/// Returns a sequence referencing the names in this group
	/** Note that the returned sequence must not be used after
	 *  this group has been destroyed.
	 */
	operator Sequence<ObjectName<ObjTag>> (void) const
	{
		return seq();
	}
};

template <typename ObjTag>
inline Group<ObjectName<ObjTag>>
operator , (ObjectName<ObjTag> a, ObjectName<ObjTag> b)
{
	return Group<ObjectName<ObjTag>>(a, b);
}

template <typename ObjTag>
inline Group<ObjectName<ObjTag>>&&
operator , (Group<ObjectName<ObjTag>>&& g, ObjectName<ObjTag> n)
{
	g.Add(n);
	return std::move(g);
}

template <typename ObjName, std::size_t N>
class StaticGroup;

template <typename ObjTag, std::size_t N>
class StaticGroup<ObjectName<ObjTag>, N>
{
private:
	typedef typename ObjTag::NameType NameT;

	std::array<NameT, N> _names;

#if !OGLPLUS_NO_VARIADIC_TEMPLATES
	void _init(std::size_t) { }

	template <typename ... Tags>
	void _init(
		std::size_t i,
		ObjectName<ObjTag> name,
		ObjectName<Tags> ... names
	)
	{
		_names[i] = GetName(name);
		_init(i+1, names...);
	}
#endif
public:
	StaticGroup(const ObjectName<ObjTag> (&names)[N])
	{
		for(std::size_t i=0; i!=N; ++i)
		{
			_names[i] = GetName(names[i]);
		}
	}

#if !OGLPLUS_NO_VARIADIC_TEMPLATES
	template <typename ... Tag>
	StaticGroup(ObjectName<Tag>... names)
	{
		_init(0, names...);
	}
#else
	StaticGroup(
		ObjectName<ObjTag> n0,
		ObjectName<ObjTag> n1
	)
	{
		_names[0] = GetName(n0);
		_names[1] = GetName(n1);
	}

	StaticGroup(
		ObjectName<ObjTag> n0,
		ObjectName<ObjTag> n1,
		ObjectName<ObjTag> n2
	)
	{
		_names[0] = GetName(n0);
		_names[1] = GetName(n1);
		_names[2] = GetName(n2);
	}
#endif

	Sequence<ObjectName<ObjTag>> seq(void) const
	{
		return Sequence<ObjectName<ObjTag>>(
			_names.data(),
			_names.size()
		);
	}

	/// Returns a sequence referencing the names in this group
	/** Note that the returned sequence must not be used after
	 *  this group has been destroyed.
	 */
	operator Sequence<ObjectName<ObjTag>> (void) const
	{
		return seq();
	}
};

#if !OGLPLUS_NO_VARIADIC_TEMPLATES
template <typename ObjTag, typename ... ObjTags>
inline StaticGroup<ObjectName<ObjTag>, 1+sizeof...(ObjTags)>
MakeGroup(ObjectName<ObjTag> name, ObjectName<ObjTags>... names)
{
	return StaticGroup<ObjectName<ObjTag>, 1+sizeof...(ObjTags)>(
		name,
		names...
	);
}
#else
template <typename ObjTag>
inline StaticGroup<ObjectName<ObjTag>, 2>
MakeGroup(ObjectName<ObjTag> n0, ObjectName<ObjTag> n1)
{
	return StaticGroup<ObjectName<ObjTag>, 2>(n0, n1);
}

template <typename ObjTag>
inline StaticGroup<ObjectName<ObjTag>, 3>
MakeGroup(ObjectName<ObjTag> n0, ObjectName<ObjTag> n1, ObjectName<ObjTag> n2)
{
	return StaticGroup<ObjectName<ObjTag>, 3>(n0, n1, n2);
}
#endif

} // namespace oglplus

#endif // include guard

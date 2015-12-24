/**
 *  @file oglplus/object/reference.hpp
 *  @brief Reference to an existing Object
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_OBJECT_REFERENCE_1405011014_HPP
#define OGLPLUS_OBJECT_REFERENCE_1405011014_HPP

#include <oglplus/object/name_tpl.hpp>

namespace oglplus {

template <typename ObjectOps>
class Object;

/// Allows to make managed copies of instances of Object
/** For obvious reasons @ref oglplus_object "objects" are not copyable,
 *  only movable. There may however be situations where a temporary reference
 *  to the "master" object (with the knowledge that the original will
 *  be kept alive during the whole lifetime of the copy) is needed.
 *  The Reference template class allows to do such temporary references
 *  which have the same members and friend functions as the original
 *  object, and can be use in the same way, provided that the original
 *  instance is not destroyed before the managed copy.
 *
 *  Instances of Reference are may be for example created when accessing
 *  or iterating through the elements of an @ref oglplus::Sequence "Sequence".
 *
 *  @ingroup modifier_classes
 */
template <typename Object>
class Reference
 : public Object
{
private:
	typedef typename Classify<Object>::ObjTag ObjTag;
public:
	Reference(ObjectName<ObjTag> obj_name)
	 : Object(obj_name)
	{ }
};

template <typename ObjectOps>
struct Reference<Object<ObjectOps>>
 : public Reference<ObjectOps>
{
private:
	typedef typename Classify<Object<ObjectOps>>::ObjTag ObjTag;
public:
	Reference(ObjectName<ObjTag> obj_name)
	 : Reference<ObjectOps>(obj_name)
	{ }
};

} // namespace oglplus

#endif // include guard

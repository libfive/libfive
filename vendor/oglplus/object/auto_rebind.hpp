/**
 *  @file oglplus/object/auto_rebind.hpp
 *  @brief Object-target binding utilities
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_OBJECT_AUTO_REBIND_1107121519_HPP
#define OGLPLUS_OBJECT_AUTO_REBIND_1107121519_HPP

#include <oglplus/object/wrapper.hpp>

namespace oglplus {

template <typename Object>
class AutoRebind;

/// Class that remembers the currently bound Object and rebinds it when destroyed
template <typename OpsTag, typename ObjTag>
class AutoRebind<Object<ObjectOps<OpsTag, ObjTag>>>
{
private:
	typedef typename ObjectOps<OpsTag, ObjTag>::Target Target;
	ObjectName<ObjTag> _object;
	Target _target;
public:
	/// Remembers the object currently bound to target
	AutoRebind(Target target)
	 : _object(ObjBindingOps<ObjTag>::Binding(target))
	 , _target(target)
	{ }

	/// Re-binds the object to the target specified in constructor
	~AutoRebind(void)
	{
		ObjBindingOps<ObjTag>::Bind(_target, _object);
	}
};

} // namespace oglplus

#endif // include guard

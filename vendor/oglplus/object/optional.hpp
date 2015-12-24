/**
 *  @file oglplus/object/optional.hpp
 *  @brief Template wrapper for Objects, making them optional
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_OBJECT_OPTIONAL_1107121519_HPP
#define OGLPLUS_OBJECT_OPTIONAL_1107121519_HPP

#include <oglplus/detail/optional.hpp>
#include <oglplus/object/wrap_tpl.hpp>

namespace oglplus {

/// Modifier that allows to create uninitialized Object(s)
/** The Optional template class is a modifier for @ref oglplus_object "Objects"
 *  and in contrast to Objects it does allow to construct uninitialized
 *  instances which can be initialized later.
 *
 *  An Optional<Object> can be used everywhere an Object could be
 *  used, but it must be initialized (the HasValidName() member function
 *  must return true), otherwise usage results in undefined behavior.
 *
 *  @ingroup modifier_classes
 */
template <class Object>
class OptionalImpl<tag::Object, Object>
 : public Object
{
public:
	/// Construction of an uninitialized instance
	/** The only things that can safely be done with
	 *  an uninitialized Optional<Object> is assignment,
	 *  moving, destruction and checking whether it is
	 *  initialized.
	 *
	 *  @see HasValidName
	 *  @see Assign
	 */
	OptionalImpl(void)
	OGLPLUS_NOEXCEPT(true)
	 : Object(typename Object::Uninitialized_())
	{ }

	/// Construction of an initialized instance
	/** Initialized Optional<Object> can be used everywhere where
	 *  a plain Object could be used, furthermore Optional<Object>
	 *  can also be cleared (this brings it in uninitialized state)
	 *
	 *  @see HasValidName
	 *  @see Clear
	 */
	OptionalImpl(Object&& temp)
	OGLPLUS_NOEXCEPT(true)
	 : Object(std::move(temp))
	{ }

	/// Move constructor
	OptionalImpl(OptionalImpl&& temp)
	OGLPLUS_NOEXCEPT(true)
	 : Object(static_cast<Object&&>(temp))
	{ }

	OptionalImpl& operator = (Object&& temp)
	{
		Object::operator=(std::move(temp));
		return *this;
	}

	OGLPLUS_EXPLICIT
	operator bool (void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return this->HasValidName();
	}

	/// Releases the stored object and makes this Optional uninitialized
	Object Release(void)
	OGLPLUS_NOEXCEPT(true)
	{
		return Object(static_cast<Object&&>(*this));
	}
};

} // namespace oglplus

#endif // include guard

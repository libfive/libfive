/**
 *  @file oglplus/prog_var/lazy.hpp
 *  @brief Lazy program variable wrapper
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_PROG_VAR_LAZY_1405052234_HPP
#define OGLPLUS_PROG_VAR_LAZY_1405052234_HPP

#include <oglplus/detail/lazy.hpp>
#include <oglplus/prog_var/wrapper.hpp>
#include <oglplus/string/def.hpp>
#include <oglplus/string/ref.hpp>

namespace oglplus {

template <typename ProgVar_>
class LazyImpl<tag::ProgVar, ProgVar_>
 : public ProgVar_
{
private:
	String _identifier;
public:
	LazyImpl(ProgramName program, String&& identifier)
	 : ProgVar_(program)
	 , _identifier(std::move(identifier))
	{ }

	LazyImpl& Init(void)
	{
		if(!this->IsActive())
		{
			this->BindTo(_identifier);
			ProgVar_::RequireActive(_identifier);
			_identifier.clear();
		}
		return *this;
	}

	LazyImpl& TryInit(void)
	{
		if(!this->IsActive())
		{
			if(this->BindTo(_identifier, false).IsActive())
			{
				_identifier.clear();
			}
		}
		return *this;
	}

	ProgVar_ operator[](std::size_t offset)
	{
		Init();
		return ProgVar_(
			ProgramName(this->_program),
			GLuint(this->_location+offset)
		);
	}

	template <typename T>
	void Set(T&& value)
	{
		Init();
		ProgVar_::Set(std::forward<T>(value));
	}

	template <typename T0, typename T1>
	void Set(T0&& v0, T1&& v1)
	{
		Init();
		ProgVar_::Set(
			std::forward<T0>(v0),
			std::forward<T1>(v1)
		);
	}

	template <typename T0, typename T1, typename T2>
	void Set(T0&& v0, T1&& v1, T2&& v2)
	{
		Init();
		ProgVar_::Set(
			std::forward<T0>(v0),
			std::forward<T1>(v1),
			std::forward<T1>(v2)
		);
	}

	template <typename T0, typename T1, typename T2, typename T3>
	void Set(T0&& v0, T1&& v1, T2&& v2, T3&& v3)
	{
		Init();
		ProgVar_::Set(
			std::forward<T0>(v0),
			std::forward<T1>(v1),
			std::forward<T1>(v2),
			std::forward<T1>(v3)
		);
	}
};

} // namespace oglplus

#endif // include guard

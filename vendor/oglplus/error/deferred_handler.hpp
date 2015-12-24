/**
 *  @file oglplus/error/deferred_handler.hpp
 *  @brief Enumeration the DeferredHandler template
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_ERROR_DEFERRED_HANDLER_1405082311_HPP
#define OGLPLUS_ERROR_DEFERRED_HANDLER_1405082311_HPP

#include <oglplus/config/compiler.hpp>
#include <memory>

namespace oglplus {

class DeferredHandler
{
private:
	struct _handler_intf
	{
		virtual
		~_handler_intf(void)
		OGLPLUS_NOTHROW
		{ }

		virtual
		void execute(bool destroying) = 0;
	};

	struct _abort_handler : _handler_intf
	{
		void execute(bool)
		OGLPLUS_OVERRIDE;
	};

	template <typename Func>
	struct _handler_impl : _handler_intf
	{
		Func _func;

		_handler_impl(Func&& func)
		 : _func(std::move(func))
		{ }

		void execute(bool)
		OGLPLUS_OVERRIDE
		{
			_func();
		}
	};

	typedef void(*_intf_deleter)(_handler_intf*);

	static
	void _impl_delete(_handler_intf* x) { delete x; }

	static
	void _fake_delete(_handler_intf*) { }

	typedef std::unique_ptr<_handler_intf, _intf_deleter>
		_unique_handler_ptr;

	_unique_handler_ptr _handler;

	template <typename Func>
	static
	_unique_handler_ptr _wrap_func(Func func)
	{
		return _unique_handler_ptr(
			new _handler_impl<Func>(
				std::move(func)
			), &_impl_delete
		);
	}

	_unique_handler_ptr _release_handler(void)
	OGLPLUS_NOEXCEPT(true);
public:
	DeferredHandler(void)
	OGLPLUS_NOEXCEPT(true)
	 : _handler(nullptr, &_fake_delete)
	{ }

#if !OGLPLUS_NO_DEFAULTED_FUNCTIONS
	DeferredHandler(DeferredHandler&&) = default;
#else
	DeferredHandler(DeferredHandler&& temp)
	OGLPLUS_NOEXCEPT(true)
	 : _handler(std::move(temp._handler))
	{ }
#endif

	template <typename Func>
	explicit
	DeferredHandler(Func func)
	 : _handler(_wrap_func(func))
	{ }

	~DeferredHandler(void)
	{
		if(_handler)
		{
			if(!std::uncaught_exception())
			{
				_release_handler()->execute(true);
			}
		}
	}

	void trigger(void)
	{
		if(_handler)
		{
			_release_handler()->execute(false);
		}
	}

	bool cancel(void)
	OGLPLUS_NOEXCEPT(true)
	{
		return bool(_release_handler());
	}

	OGLPLUS_EXPLICIT
	operator bool (void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return bool(_handler);
	}

	bool operator ! (void) const
	OGLPLUS_NOEXCEPT(true)
	{
		return !_handler;
	}
};

} // namespace oglplus

#if !OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)
#include <oglplus/error/deferred_handler.ipp>
#endif

#endif // include guard

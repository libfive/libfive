/**
 *  @file oglplus/text/pango_cairo/handle.hpp
 *  @brief Pango/Cairo-based text rendering - helper class.
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2014 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_TEXT_PANGO_CAIRO_HANDLE_HPP
#define OGLPLUS_TEXT_PANGO_CAIRO_HANDLE_HPP

#include <oglplus/text/pango_cairo/fwd.hpp>

#include <cassert>

namespace oglplus {
namespace text {

template <typename Handle, typename DeleterParam = Handle>
class PangoCairoHandle
{
private:
	Handle _handle;
	void (*_deleter)(DeleterParam);

	PangoCairoHandle(const PangoCairoHandle&);
public:
	PangoCairoHandle(
		Handle handle,
		void(*deleter)(DeleterParam)
	): _handle(handle)
	 , _deleter(deleter)
	{ }

	PangoCairoHandle(PangoCairoHandle&& tmp)
	 : _handle(tmp._handle)
	 , _deleter(tmp._deleter)
	{
		tmp._handle = nullptr;
		tmp._deleter = nullptr;
	}

	~PangoCairoHandle(void)
	{
		if(_handle && _deleter)
			_deleter(static_cast<DeleterParam>(_handle));
	}

	void replace(Handle handle)
	{
		if(_handle && _deleter)
			_deleter(static_cast<DeleterParam>(_handle));
		_handle = handle;
	}

	PangoCairoHandle& operator = (Handle handle)
	{
		replace(handle);
		return *this;
	}

	Handle get(void)
	{
		return _handle;
	}

	operator Handle (void)
	{
		return get();
	}
};

} // namespace text
} // namespace oglplus

#endif // include guard

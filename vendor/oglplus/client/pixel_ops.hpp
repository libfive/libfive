/**
 *  @file oglplus/client/pixel_ops.hpp
 *  @brief Client current pixel transfer/storage setting stack
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_CLIENT_PIXEL_OPS_1412071213_HPP
#define OGLPLUS_CLIENT_PIXEL_OPS_1412071213_HPP

#include <oglplus/client/setting.hpp>
#include <oglplus/context/pixel_ops.hpp>

namespace oglplus {
namespace client {
namespace aux {

#if GL_VERSION_3_0

template <PixelParameter Parameter>
class PixelStore
 : public SettingStack<
	typename oglplus::enums::EnumAssocGLType<
		PixelParameter,
		Parameter
	>::Type,
	Nothing
>
{
private:
	typedef	typename oglplus::enums::EnumAssocGLType<
		PixelParameter,
		Parameter
	>::Type _value_t;

	static
	_value_t _do_get(Nothing)
	{
		return context::PixelState::PixelStoreValue<Parameter>();
	}

	static
	void _do_set(_value_t value, Nothing)
	{
		context::PixelState::PixelStore<Parameter>(value);
	}
public:
	PixelStore(void)
	 : SettingStack<_value_t, Nothing>(&_do_get, &_do_set)
	{ }
};
#endif

} // namespace aux

class PixelState
{
public:
#if GL_VERSION_3_0
	oglplus::enums::EnumToClass<
		Nothing,
		PixelParameter,
		aux::PixelStore
	> PixelStore;
#endif
};

using context::PixelOps;

} // namespace client
} // namespace oglplus

#endif // include guard

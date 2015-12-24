//  File implement/oglplus/enums/reset_notif_strategy_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/reset_notif_strategy.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLenum*,
	ResetNotificationStrategy
> ValueRange_(ResetNotificationStrategy*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_RESETNOTIFICATIONSTRATEGY)
#define OGLPLUS_IMPL_EVR_RESETNOTIFICATIONSTRATEGY
{
static const GLenum _values[] = {
#if defined GL_NO_RESET_NOTIFICATION
GL_NO_RESET_NOTIFICATION,
#endif
#if defined GL_LOSE_CONTEXT_ON_RESET
GL_LOSE_CONTEXT_ON_RESET,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	ResetNotificationStrategy
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


//  File implement/oglplus/enums/sync_wait_result_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/sync_wait_result.txt'
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
	SyncWaitResult
> ValueRange_(SyncWaitResult*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_SYNCWAITRESULT)
#define OGLPLUS_IMPL_EVR_SYNCWAITRESULT
{
static const GLenum _values[] = {
#if defined GL_CONDITION_SATISFIED
GL_CONDITION_SATISFIED,
#endif
#if defined GL_ALREADY_SIGNALED
GL_ALREADY_SIGNALED,
#endif
#if defined GL_TIMEOUT_EXPIRED
GL_TIMEOUT_EXPIRED,
#endif
#if defined GL_WAIT_FAILED
GL_WAIT_FAILED,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	SyncWaitResult
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


//  File implement/oglplus/enums/sync_wait_result_names.ipp
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
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	SyncWaitResult*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_SYNCWAITRESULT)
#define OGLPLUS_IMPL_EVN_SYNCWAITRESULT
{
switch(value)
{
#if defined GL_CONDITION_SATISFIED
	case GL_CONDITION_SATISFIED: return StrCRef("CONDITION_SATISFIED");
#endif
#if defined GL_ALREADY_SIGNALED
	case GL_ALREADY_SIGNALED: return StrCRef("ALREADY_SIGNALED");
#endif
#if defined GL_TIMEOUT_EXPIRED
	case GL_TIMEOUT_EXPIRED: return StrCRef("TIMEOUT_EXPIRED");
#endif
#if defined GL_WAIT_FAILED
	case GL_WAIT_FAILED: return StrCRef("WAIT_FAILED");
#endif
	default:;
}
OGLPLUS_FAKE_USE(value);
return StrCRef();
}
#else
;
#endif
} // namespace enums


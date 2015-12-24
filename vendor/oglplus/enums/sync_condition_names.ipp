//  File implement/oglplus/enums/sync_condition_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/sync_condition.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	SyncCondition*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_SYNCCONDITION)
#define OGLPLUS_IMPL_EVN_SYNCCONDITION
{
switch(value)
{
#if defined GL_SYNC_GPU_COMMANDS_COMPLETE
	case GL_SYNC_GPU_COMMANDS_COMPLETE: return StrCRef("SYNC_GPU_COMMANDS_COMPLETE");
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


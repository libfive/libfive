//  File implement/oglplus/enums/ext/nv_path_dash_offset_reset_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_dash_offset_reset.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	PathNVDashOffsetReset*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PATHNVDASHOFFSETRESET)
#define OGLPLUS_IMPL_EVN_PATHNVDASHOFFSETRESET
{
switch(value)
{
#if defined GL_MOVE_TO_RESET_NV
	case GL_MOVE_TO_RESET_NV: return StrCRef("MOVE_TO_RESET_NV");
#endif
#if defined GL_MOVE_TO_CONTINUES_NV
	case GL_MOVE_TO_CONTINUES_NV: return StrCRef("MOVE_TO_CONTINUES_NV");
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


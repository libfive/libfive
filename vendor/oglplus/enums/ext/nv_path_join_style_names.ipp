//  File implement/oglplus/enums/ext/nv_path_join_style_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_join_style.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	PathNVJoinStyle*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PATHNVJOINSTYLE)
#define OGLPLUS_IMPL_EVN_PATHNVJOINSTYLE
{
switch(value)
{
#if defined GL_NONE
	case GL_NONE: return StrCRef("NONE");
#endif
#if defined GL_ROUND_NV
	case GL_ROUND_NV: return StrCRef("ROUND_NV");
#endif
#if defined GL_BEVEL_NV
	case GL_BEVEL_NV: return StrCRef("BEVEL_NV");
#endif
#if defined GL_MITER_REVERT_NV
	case GL_MITER_REVERT_NV: return StrCRef("MITER_REVERT_NV");
#endif
#if defined GL_MITER_TRUNCATE_NV
	case GL_MITER_TRUNCATE_NV: return StrCRef("MITER_TRUNCATE_NV");
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


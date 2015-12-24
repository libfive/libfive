//  File implement/oglplus/enums/ext/nv_path_color_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_color.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	PathNVColor*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PATHNVCOLOR)
#define OGLPLUS_IMPL_EVN_PATHNVCOLOR
{
switch(value)
{
#if defined GL_PRIMARY_COLOR_NV
	case GL_PRIMARY_COLOR_NV: return StrCRef("PRIMARY_COLOR_NV");
#endif
#if defined GL_SECONDARY_COLOR_NV
	case GL_SECONDARY_COLOR_NV: return StrCRef("SECONDARY_COLOR_NV");
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


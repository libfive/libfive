//  File implement/oglplus/enums/ext/nv_path_font_target_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_font_target.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	PathNVFontTarget*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PATHNVFONTTARGET)
#define OGLPLUS_IMPL_EVN_PATHNVFONTTARGET
{
switch(value)
{
#if defined GL_STANDARD_FONT_NAME_NV
	case GL_STANDARD_FONT_NAME_NV: return StrCRef("STANDARD_FONT_NAME_NV");
#endif
#if defined GL_SYSTEM_FONT_NAME_NV
	case GL_SYSTEM_FONT_NAME_NV: return StrCRef("SYSTEM_FONT_NAME_NV");
#endif
#if defined GL_FILE_NAME_NV
	case GL_FILE_NAME_NV: return StrCRef("FILE_NAME_NV");
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


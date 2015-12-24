//  File implement/oglplus/enums/ext/nv_path_fill_mode_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_fill_mode.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	PathNVFillMode*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PATHNVFILLMODE)
#define OGLPLUS_IMPL_EVN_PATHNVFILLMODE
{
switch(value)
{
#if defined GL_INVERT
	case GL_INVERT: return StrCRef("INVERT");
#endif
#if defined GL_COUNT_UP_NV
	case GL_COUNT_UP_NV: return StrCRef("COUNT_UP_NV");
#endif
#if defined GL_COUNT_DOWN_NV
	case GL_COUNT_DOWN_NV: return StrCRef("COUNT_DOWN_NV");
#endif
#if defined GL_PATH_FILL_MODE_NV
	case GL_PATH_FILL_MODE_NV: return StrCRef("PATH_FILL_MODE_NV");
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


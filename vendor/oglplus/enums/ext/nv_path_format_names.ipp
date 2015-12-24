//  File implement/oglplus/enums/ext/nv_path_format_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_format.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	PathNVFormat*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PATHNVFORMAT)
#define OGLPLUS_IMPL_EVN_PATHNVFORMAT
{
switch(value)
{
#if defined GL_PATH_FORMAT_SVG_NV
	case GL_PATH_FORMAT_SVG_NV: return StrCRef("PATH_FORMAT_SVG_NV");
#endif
#if defined GL_PATH_FORMAT_PS_NV
	case GL_PATH_FORMAT_PS_NV: return StrCRef("PATH_FORMAT_PS_NV");
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


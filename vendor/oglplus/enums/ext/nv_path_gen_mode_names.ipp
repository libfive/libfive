//  File implement/oglplus/enums/ext/nv_path_gen_mode_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_gen_mode.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	PathNVGenMode*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PATHNVGENMODE)
#define OGLPLUS_IMPL_EVN_PATHNVGENMODE
{
switch(value)
{
#if defined GL_NONE
	case GL_NONE: return StrCRef("NONE");
#endif
#if defined GL_EYE_LINEAR
	case GL_EYE_LINEAR: return StrCRef("EYE_LINEAR");
#endif
#if defined GL_OBJECT_LINEAR
	case GL_OBJECT_LINEAR: return StrCRef("OBJECT_LINEAR");
#endif
#if defined GL_PATH_OBJECT_BOUNDING_BOX_NV
	case GL_PATH_OBJECT_BOUNDING_BOX_NV: return StrCRef("PATH_OBJECT_BOUNDING_BOX_NV");
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


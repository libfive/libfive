//  File implement/oglplus/enums/patch_parameter_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/patch_parameter.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	PatchParameter*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PATCHPARAMETER)
#define OGLPLUS_IMPL_EVN_PATCHPARAMETER
{
switch(value)
{
#if defined GL_PATCH_VERTICES
	case GL_PATCH_VERTICES: return StrCRef("PATCH_VERTICES");
#endif
#if defined GL_PATCH_DEFAULT_OUTER_LEVEL
	case GL_PATCH_DEFAULT_OUTER_LEVEL: return StrCRef("PATCH_DEFAULT_OUTER_LEVEL");
#endif
#if defined GL_PATCH_DEFAULT_INNER_LEVEL
	case GL_PATCH_DEFAULT_INNER_LEVEL: return StrCRef("PATCH_DEFAULT_INNER_LEVEL");
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


//  File implement/oglplus/enums/string_query_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/string_query.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	StringQuery*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_STRINGQUERY)
#define OGLPLUS_IMPL_EVN_STRINGQUERY
{
switch(value)
{
#if defined GL_RENDERER
	case GL_RENDERER: return StrCRef("RENDERER");
#endif
#if defined GL_VENDOR
	case GL_VENDOR: return StrCRef("VENDOR");
#endif
#if defined GL_VERSION
	case GL_VERSION: return StrCRef("VERSION");
#endif
#if defined GL_SHADING_LANGUAGE_VERSION
	case GL_SHADING_LANGUAGE_VERSION: return StrCRef("SHADING_LANGUAGE_VERSION");
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


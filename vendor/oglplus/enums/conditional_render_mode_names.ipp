//  File implement/oglplus/enums/conditional_render_mode_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/conditional_render_mode.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	ConditionalRenderMode*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_CONDITIONALRENDERMODE)
#define OGLPLUS_IMPL_EVN_CONDITIONALRENDERMODE
{
switch(value)
{
#if defined GL_QUERY_WAIT
	case GL_QUERY_WAIT: return StrCRef("QUERY_WAIT");
#endif
#if defined GL_QUERY_NO_WAIT
	case GL_QUERY_NO_WAIT: return StrCRef("QUERY_NO_WAIT");
#endif
#if defined GL_QUERY_BY_REGION_WAIT
	case GL_QUERY_BY_REGION_WAIT: return StrCRef("QUERY_BY_REGION_WAIT");
#endif
#if defined GL_QUERY_BY_REGION_NO_WAIT
	case GL_QUERY_BY_REGION_NO_WAIT: return StrCRef("QUERY_BY_REGION_NO_WAIT");
#endif
#if defined GL_QUERY_WAIT_INVERTED
	case GL_QUERY_WAIT_INVERTED: return StrCRef("QUERY_WAIT_INVERTED");
#endif
#if defined GL_QUERY_NO_WAIT_INVERTED
	case GL_QUERY_NO_WAIT_INVERTED: return StrCRef("QUERY_NO_WAIT_INVERTED");
#endif
#if defined GL_QUERY_BY_REGION_WAIT_INVERTED
	case GL_QUERY_BY_REGION_WAIT_INVERTED: return StrCRef("QUERY_BY_REGION_WAIT_INVERTED");
#endif
#if defined GL_QUERY_BY_REGION_NO_WAIT_INVERTED
	case GL_QUERY_BY_REGION_NO_WAIT_INVERTED: return StrCRef("QUERY_BY_REGION_NO_WAIT_INVERTED");
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


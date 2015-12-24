//  File implement/oglplus/enums/conditional_render_mode_range.ipp
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
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLenum*,
	ConditionalRenderMode
> ValueRange_(ConditionalRenderMode*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_CONDITIONALRENDERMODE)
#define OGLPLUS_IMPL_EVR_CONDITIONALRENDERMODE
{
static const GLenum _values[] = {
#if defined GL_QUERY_WAIT
GL_QUERY_WAIT,
#endif
#if defined GL_QUERY_NO_WAIT
GL_QUERY_NO_WAIT,
#endif
#if defined GL_QUERY_BY_REGION_WAIT
GL_QUERY_BY_REGION_WAIT,
#endif
#if defined GL_QUERY_BY_REGION_NO_WAIT
GL_QUERY_BY_REGION_NO_WAIT,
#endif
#if defined GL_QUERY_WAIT_INVERTED
GL_QUERY_WAIT_INVERTED,
#endif
#if defined GL_QUERY_NO_WAIT_INVERTED
GL_QUERY_NO_WAIT_INVERTED,
#endif
#if defined GL_QUERY_BY_REGION_WAIT_INVERTED
GL_QUERY_BY_REGION_WAIT_INVERTED,
#endif
#if defined GL_QUERY_BY_REGION_NO_WAIT_INVERTED
GL_QUERY_BY_REGION_NO_WAIT_INVERTED,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	ConditionalRenderMode
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


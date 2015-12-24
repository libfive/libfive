//  File implement/oglplus/enums/string_query_range.ipp
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
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLenum*,
	StringQuery
> ValueRange_(StringQuery*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_STRINGQUERY)
#define OGLPLUS_IMPL_EVR_STRINGQUERY
{
static const GLenum _values[] = {
#if defined GL_RENDERER
GL_RENDERER,
#endif
#if defined GL_VENDOR
GL_VENDOR,
#endif
#if defined GL_VERSION
GL_VERSION,
#endif
#if defined GL_SHADING_LANGUAGE_VERSION
GL_SHADING_LANGUAGE_VERSION,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	StringQuery
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


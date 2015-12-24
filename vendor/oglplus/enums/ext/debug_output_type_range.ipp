//  File implement/oglplus/enums/ext/debug_output_type_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/debug_output_type.txt'
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
	DebugOutputARBType
> ValueRange_(DebugOutputARBType*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_DEBUGOUTPUTARBTYPE)
#define OGLPLUS_IMPL_EVR_DEBUGOUTPUTARBTYPE
{
static const GLenum _values[] = {
#if defined GL_DEBUG_TYPE_ERROR_ARB
GL_DEBUG_TYPE_ERROR_ARB,
#endif
#if defined GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB
GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB,
#endif
#if defined GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB
GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB,
#endif
#if defined GL_DEBUG_TYPE_PORTABILITY_ARB
GL_DEBUG_TYPE_PORTABILITY_ARB,
#endif
#if defined GL_DEBUG_TYPE_PERFORMANCE_ARB
GL_DEBUG_TYPE_PERFORMANCE_ARB,
#endif
#if defined GL_DEBUG_TYPE_OTHER_ARB
GL_DEBUG_TYPE_OTHER_ARB,
#endif
#if defined GL_DONT_CARE
GL_DONT_CARE,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	DebugOutputARBType
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


//  File implement/oglplus/enums/ext/debug_output_type_names.ipp
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
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	DebugOutputARBType*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_DEBUGOUTPUTARBTYPE)
#define OGLPLUS_IMPL_EVN_DEBUGOUTPUTARBTYPE
{
switch(value)
{
#if defined GL_DEBUG_TYPE_ERROR_ARB
	case GL_DEBUG_TYPE_ERROR_ARB: return StrCRef("DEBUG_TYPE_ERROR_ARB");
#endif
#if defined GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB
	case GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB: return StrCRef("DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB");
#endif
#if defined GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB
	case GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB: return StrCRef("DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB");
#endif
#if defined GL_DEBUG_TYPE_PORTABILITY_ARB
	case GL_DEBUG_TYPE_PORTABILITY_ARB: return StrCRef("DEBUG_TYPE_PORTABILITY_ARB");
#endif
#if defined GL_DEBUG_TYPE_PERFORMANCE_ARB
	case GL_DEBUG_TYPE_PERFORMANCE_ARB: return StrCRef("DEBUG_TYPE_PERFORMANCE_ARB");
#endif
#if defined GL_DEBUG_TYPE_OTHER_ARB
	case GL_DEBUG_TYPE_OTHER_ARB: return StrCRef("DEBUG_TYPE_OTHER_ARB");
#endif
#if defined GL_DONT_CARE
	case GL_DONT_CARE: return StrCRef("DONT_CARE");
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


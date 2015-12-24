//  File implement/oglplus/enums/ext/debug_output_severity_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/debug_output_severity.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	DebugOutputARBSeverity*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_DEBUGOUTPUTARBSEVERITY)
#define OGLPLUS_IMPL_EVN_DEBUGOUTPUTARBSEVERITY
{
switch(value)
{
#if defined GL_DEBUG_SEVERITY_HIGH_ARB
	case GL_DEBUG_SEVERITY_HIGH_ARB: return StrCRef("DEBUG_SEVERITY_HIGH_ARB");
#endif
#if defined GL_DEBUG_SEVERITY_MEDIUM_ARB
	case GL_DEBUG_SEVERITY_MEDIUM_ARB: return StrCRef("DEBUG_SEVERITY_MEDIUM_ARB");
#endif
#if defined GL_DEBUG_SEVERITY_LOW_ARB
	case GL_DEBUG_SEVERITY_LOW_ARB: return StrCRef("DEBUG_SEVERITY_LOW_ARB");
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


//  File implement/oglplus/enums/ext/graphics_reset_status_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/graphics_reset_status.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	GraphicsResetStatusARB*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_GRAPHICSRESETSTATUSARB)
#define OGLPLUS_IMPL_EVN_GRAPHICSRESETSTATUSARB
{
switch(value)
{
#if defined GL_NO_ERROR
	case GL_NO_ERROR: return StrCRef("NO_ERROR");
#endif
#if defined GL_GUILTY_CONTEXT_RESET_ARB
	case GL_GUILTY_CONTEXT_RESET_ARB: return StrCRef("GUILTY_CONTEXT_RESET_ARB");
#endif
#if defined GL_INNOCENT_CONTEXT_RESET_ARB
	case GL_INNOCENT_CONTEXT_RESET_ARB: return StrCRef("INNOCENT_CONTEXT_RESET_ARB");
#endif
#if defined GL_UNKNOWN_CONTEXT_RESET_ARB
	case GL_UNKNOWN_CONTEXT_RESET_ARB: return StrCRef("UNKNOWN_CONTEXT_RESET_ARB");
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


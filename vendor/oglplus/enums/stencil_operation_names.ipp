//  File implement/oglplus/enums/stencil_operation_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/stencil_operation.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	StencilOperation*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_STENCILOPERATION)
#define OGLPLUS_IMPL_EVN_STENCILOPERATION
{
switch(value)
{
#if defined GL_KEEP
	case GL_KEEP: return StrCRef("KEEP");
#endif
#if defined GL_ZERO
	case GL_ZERO: return StrCRef("ZERO");
#endif
#if defined GL_REPLACE
	case GL_REPLACE: return StrCRef("REPLACE");
#endif
#if defined GL_INCR
	case GL_INCR: return StrCRef("INCR");
#endif
#if defined GL_DECR
	case GL_DECR: return StrCRef("DECR");
#endif
#if defined GL_INVERT
	case GL_INVERT: return StrCRef("INVERT");
#endif
#if defined GL_INCR_WRAP
	case GL_INCR_WRAP: return StrCRef("INCR_WRAP");
#endif
#if defined GL_DECR_WRAP
	case GL_DECR_WRAP: return StrCRef("DECR_WRAP");
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


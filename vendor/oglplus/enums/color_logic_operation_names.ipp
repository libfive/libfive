//  File implement/oglplus/enums/color_logic_operation_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/color_logic_operation.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	ColorLogicOperation*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_COLORLOGICOPERATION)
#define OGLPLUS_IMPL_EVN_COLORLOGICOPERATION
{
switch(value)
{
#if defined GL_CLEAR
	case GL_CLEAR: return StrCRef("CLEAR");
#endif
#if defined GL_AND
	case GL_AND: return StrCRef("AND");
#endif
#if defined GL_AND_REVERSE
	case GL_AND_REVERSE: return StrCRef("AND_REVERSE");
#endif
#if defined GL_COPY
	case GL_COPY: return StrCRef("COPY");
#endif
#if defined GL_AND_INVERTED
	case GL_AND_INVERTED: return StrCRef("AND_INVERTED");
#endif
#if defined GL_NOOP
	case GL_NOOP: return StrCRef("NOOP");
#endif
#if defined GL_XOR
	case GL_XOR: return StrCRef("XOR");
#endif
#if defined GL_OR
	case GL_OR: return StrCRef("OR");
#endif
#if defined GL_NOR
	case GL_NOR: return StrCRef("NOR");
#endif
#if defined GL_EQUIV
	case GL_EQUIV: return StrCRef("EQUIV");
#endif
#if defined GL_INVERT
	case GL_INVERT: return StrCRef("INVERT");
#endif
#if defined GL_OR_REVERSE
	case GL_OR_REVERSE: return StrCRef("OR_REVERSE");
#endif
#if defined GL_COPY_INVERTED
	case GL_COPY_INVERTED: return StrCRef("COPY_INVERTED");
#endif
#if defined GL_OR_INVERTED
	case GL_OR_INVERTED: return StrCRef("OR_INVERTED");
#endif
#if defined GL_NAND
	case GL_NAND: return StrCRef("NAND");
#endif
#if defined GL_SET
	case GL_SET: return StrCRef("SET");
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


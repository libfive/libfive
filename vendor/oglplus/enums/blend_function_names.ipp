//  File implement/oglplus/enums/blend_function_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/blend_function.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	BlendFunction*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_BLENDFUNCTION)
#define OGLPLUS_IMPL_EVN_BLENDFUNCTION
{
switch(value)
{
#if defined GL_ZERO
	case GL_ZERO: return StrCRef("ZERO");
#endif
#if defined GL_ONE
	case GL_ONE: return StrCRef("ONE");
#endif
#if defined GL_SRC_COLOR
	case GL_SRC_COLOR: return StrCRef("SRC_COLOR");
#endif
#if defined GL_ONE_MINUS_SRC_COLOR
	case GL_ONE_MINUS_SRC_COLOR: return StrCRef("ONE_MINUS_SRC_COLOR");
#endif
#if defined GL_DST_COLOR
	case GL_DST_COLOR: return StrCRef("DST_COLOR");
#endif
#if defined GL_ONE_MINUS_DST_COLOR
	case GL_ONE_MINUS_DST_COLOR: return StrCRef("ONE_MINUS_DST_COLOR");
#endif
#if defined GL_SRC_ALPHA
	case GL_SRC_ALPHA: return StrCRef("SRC_ALPHA");
#endif
#if defined GL_ONE_MINUS_SRC_ALPHA
	case GL_ONE_MINUS_SRC_ALPHA: return StrCRef("ONE_MINUS_SRC_ALPHA");
#endif
#if defined GL_DST_ALPHA
	case GL_DST_ALPHA: return StrCRef("DST_ALPHA");
#endif
#if defined GL_ONE_MINUS_DST_ALPHA
	case GL_ONE_MINUS_DST_ALPHA: return StrCRef("ONE_MINUS_DST_ALPHA");
#endif
#if defined GL_CONSTANT_COLOR
	case GL_CONSTANT_COLOR: return StrCRef("CONSTANT_COLOR");
#endif
#if defined GL_ONE_MINUS_CONSTANT_COLOR
	case GL_ONE_MINUS_CONSTANT_COLOR: return StrCRef("ONE_MINUS_CONSTANT_COLOR");
#endif
#if defined GL_CONSTANT_ALPHA
	case GL_CONSTANT_ALPHA: return StrCRef("CONSTANT_ALPHA");
#endif
#if defined GL_ONE_MINUS_CONSTANT_ALPHA
	case GL_ONE_MINUS_CONSTANT_ALPHA: return StrCRef("ONE_MINUS_CONSTANT_ALPHA");
#endif
#if defined GL_SRC_ALPHA_SATURATE
	case GL_SRC_ALPHA_SATURATE: return StrCRef("SRC_ALPHA_SATURATE");
#endif
#if defined GL_SRC1_COLOR
	case GL_SRC1_COLOR: return StrCRef("SRC1_COLOR");
#endif
#if defined GL_ONE_MINUS_SRC1_COLOR
	case GL_ONE_MINUS_SRC1_COLOR: return StrCRef("ONE_MINUS_SRC1_COLOR");
#endif
#if defined GL_SRC1_ALPHA
	case GL_SRC1_ALPHA: return StrCRef("SRC1_ALPHA");
#endif
#if defined GL_ONE_MINUS_SRC1_ALPHA
	case GL_ONE_MINUS_SRC1_ALPHA: return StrCRef("ONE_MINUS_SRC1_ALPHA");
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


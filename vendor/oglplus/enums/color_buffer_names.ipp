//  File implement/oglplus/enums/color_buffer_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/color_buffer.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	ColorBuffer*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_COLORBUFFER)
#define OGLPLUS_IMPL_EVN_COLORBUFFER
{
switch(value)
{
#if defined GL_NONE
	case GL_NONE: return StrCRef("NONE");
#endif
#if defined GL_FRONT_LEFT
	case GL_FRONT_LEFT: return StrCRef("FRONT_LEFT");
#endif
#if defined GL_FRONT_RIGHT
	case GL_FRONT_RIGHT: return StrCRef("FRONT_RIGHT");
#endif
#if defined GL_BACK_LEFT
	case GL_BACK_LEFT: return StrCRef("BACK_LEFT");
#endif
#if defined GL_BACK_RIGHT
	case GL_BACK_RIGHT: return StrCRef("BACK_RIGHT");
#endif
#if defined GL_FRONT
	case GL_FRONT: return StrCRef("FRONT");
#endif
#if defined GL_BACK
	case GL_BACK: return StrCRef("BACK");
#endif
#if defined GL_LEFT
	case GL_LEFT: return StrCRef("LEFT");
#endif
#if defined GL_RIGHT
	case GL_RIGHT: return StrCRef("RIGHT");
#endif
#if defined GL_FRONT_AND_BACK
	case GL_FRONT_AND_BACK: return StrCRef("FRONT_AND_BACK");
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


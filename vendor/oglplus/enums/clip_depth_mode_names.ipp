//  File implement/oglplus/enums/clip_depth_mode_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/clip_depth_mode.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	ClipDepthMode*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_CLIPDEPTHMODE)
#define OGLPLUS_IMPL_EVN_CLIPDEPTHMODE
{
switch(value)
{
#if defined GL_NEGATIVE_ONE_TO_ONE
	case GL_NEGATIVE_ONE_TO_ONE: return StrCRef("NEGATIVE_ONE_TO_ONE");
#endif
#if defined GL_ZERO_TO_ONE
	case GL_ZERO_TO_ONE: return StrCRef("ZERO_TO_ONE");
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


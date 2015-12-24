//  File implement/oglplus/enums/transform_feedback_mode_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/transform_feedback_mode.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	TransformFeedbackMode*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_TRANSFORMFEEDBACKMODE)
#define OGLPLUS_IMPL_EVN_TRANSFORMFEEDBACKMODE
{
switch(value)
{
#if defined GL_INTERLEAVED_ATTRIBS
	case GL_INTERLEAVED_ATTRIBS: return StrCRef("INTERLEAVED_ATTRIBS");
#endif
#if defined GL_SEPARATE_ATTRIBS
	case GL_SEPARATE_ATTRIBS: return StrCRef("SEPARATE_ATTRIBS");
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


//  File implement/oglplus/enums/transform_feedback_primitive_type_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/transform_feedback_primitive_type.txt'
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
	TransformFeedbackPrimitiveType
> ValueRange_(TransformFeedbackPrimitiveType*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_TRANSFORMFEEDBACKPRIMITIVETYPE)
#define OGLPLUS_IMPL_EVR_TRANSFORMFEEDBACKPRIMITIVETYPE
{
static const GLenum _values[] = {
#if defined GL_TRIANGLES
GL_TRIANGLES,
#endif
#if defined GL_LINES
GL_LINES,
#endif
#if defined GL_POINTS
GL_POINTS,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	TransformFeedbackPrimitiveType
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


//  File implement/oglplus/enums/query_target_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/query_target.txt'
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
	QueryTarget
> ValueRange_(QueryTarget*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_QUERYTARGET)
#define OGLPLUS_IMPL_EVR_QUERYTARGET
{
static const GLenum _values[] = {
#if defined GL_TIME_ELAPSED
GL_TIME_ELAPSED,
#endif
#if defined GL_TIMESTAMP
GL_TIMESTAMP,
#endif
#if defined GL_SAMPLES_PASSED
GL_SAMPLES_PASSED,
#endif
#if defined GL_ANY_SAMPLES_PASSED
GL_ANY_SAMPLES_PASSED,
#endif
#if defined GL_PRIMITIVES_GENERATED
GL_PRIMITIVES_GENERATED,
#endif
#if defined GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN
GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN,
#endif
#if defined GL_VERTICES_SUBMITTED_ARB
GL_VERTICES_SUBMITTED_ARB,
#endif
#if defined GL_PRIMITIVES_SUBMITTED_ARB
GL_PRIMITIVES_SUBMITTED_ARB,
#endif
#if defined GL_VERTEX_SHADER_INVOCATIONS_ARB
GL_VERTEX_SHADER_INVOCATIONS_ARB,
#endif
#if defined GL_TESS_CONTROL_SHADER_PATCHES_ARB
GL_TESS_CONTROL_SHADER_PATCHES_ARB,
#endif
#if defined GL_TESS_EVALUATION_SHADER_INVOCATIONS_ARB
GL_TESS_EVALUATION_SHADER_INVOCATIONS_ARB,
#endif
#if defined GL_GEOMETRY_SHADER_INVOCATIONS
GL_GEOMETRY_SHADER_INVOCATIONS,
#endif
#if defined GL_GEOMETRY_SHADER_PRIMITIVES_EMITTED_ARB
GL_GEOMETRY_SHADER_PRIMITIVES_EMITTED_ARB,
#endif
#if defined GL_FRAGMENT_SHADER_INVOCATIONS_ARB
GL_FRAGMENT_SHADER_INVOCATIONS_ARB,
#endif
#if defined GL_COMPUTE_SHADER_INVOCATIONS_ARB
GL_COMPUTE_SHADER_INVOCATIONS_ARB,
#endif
#if defined GL_CLIPPING_INPUT_PRIMITIVES_ARB
GL_CLIPPING_INPUT_PRIMITIVES_ARB,
#endif
#if defined GL_CLIPPING_OUTPUT_PRIMITIVES_ARB
GL_CLIPPING_OUTPUT_PRIMITIVES_ARB,
#endif
#if defined GL_TRANSFORM_FEEDBACK_OVERFLOW_ARB
GL_TRANSFORM_FEEDBACK_OVERFLOW_ARB,
#endif
#if defined GL_TRANSFORM_FEEDBACK_STREAM_OVERFLOW_ARB
GL_TRANSFORM_FEEDBACK_STREAM_OVERFLOW_ARB,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	QueryTarget
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


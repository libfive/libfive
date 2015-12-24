//  File implement/oglplus/enums/capability_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/capability.txt'
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
	Capability
> ValueRange_(Capability*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_CAPABILITY)
#define OGLPLUS_IMPL_EVR_CAPABILITY
{
static const GLenum _values[] = {
#if defined GL_PRIMITIVE_RESTART
GL_PRIMITIVE_RESTART,
#endif
#if defined GL_DEPTH_TEST
GL_DEPTH_TEST,
#endif
#if defined GL_STENCIL_TEST
GL_STENCIL_TEST,
#endif
#if defined GL_SCISSOR_TEST
GL_SCISSOR_TEST,
#endif
#if defined GL_CULL_FACE
GL_CULL_FACE,
#endif
#if defined GL_RASTERIZER_DISCARD
GL_RASTERIZER_DISCARD,
#endif
#if defined GL_POLYGON_OFFSET_POINT
GL_POLYGON_OFFSET_POINT,
#endif
#if defined GL_POLYGON_OFFSET_LINE
GL_POLYGON_OFFSET_LINE,
#endif
#if defined GL_POLYGON_OFFSET_FILL
GL_POLYGON_OFFSET_FILL,
#endif
#if defined GL_BLEND
GL_BLEND,
#endif
#if defined GL_COLOR_LOGIC_OP
GL_COLOR_LOGIC_OP,
#endif
#if defined GL_DITHER
GL_DITHER,
#endif
#if defined GL_MULTISAMPLE
GL_MULTISAMPLE,
#endif
#if defined GL_SAMPLE_SHADING
GL_SAMPLE_SHADING,
#endif
#if defined GL_LINE_SMOOTH
GL_LINE_SMOOTH,
#endif
#if defined GL_POLYGON_SMOOTH
GL_POLYGON_SMOOTH,
#endif
#if defined GL_PROGRAM_POINT_SIZE
GL_PROGRAM_POINT_SIZE,
#endif
#if defined GL_TEXTURE_CUBE_MAP_SEAMLESS
GL_TEXTURE_CUBE_MAP_SEAMLESS,
#endif
#if defined GL_SAMPLE_ALPHA_TO_COVERAGE
GL_SAMPLE_ALPHA_TO_COVERAGE,
#endif
#if defined GL_SAMPLE_ALPHA_TO_ONE
GL_SAMPLE_ALPHA_TO_ONE,
#endif
#if defined GL_SAMPLE_COVERAGE
GL_SAMPLE_COVERAGE,
#endif
#if defined GL_SAMPLE_MASK
GL_SAMPLE_MASK,
#endif
#if defined GL_FRAMEBUFFER_SRGB
GL_FRAMEBUFFER_SRGB,
#endif
#if defined GL_DEBUG_OUTPUT_SYNCHRONOUS
GL_DEBUG_OUTPUT_SYNCHRONOUS,
#endif
#if defined GL_STREAM_RASTERIZATION_AMD
GL_STREAM_RASTERIZATION_AMD,
#endif
#if defined GL_BLEND_ADVANCED_COHERENT_KHR
GL_BLEND_ADVANCED_COHERENT_KHR,
#endif
#if defined GL_FRAGMENT_COVERAGE_TO_COLOR_NV
GL_FRAGMENT_COVERAGE_TO_COLOR_NV,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	Capability
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


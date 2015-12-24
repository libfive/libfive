//  File implement/oglplus/enums/capability_names.ipp
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
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	Capability*,
	GLenum value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_CAPABILITY)
#define OGLPLUS_IMPL_EVN_CAPABILITY
{
switch(value)
{
#if defined GL_PRIMITIVE_RESTART
	case GL_PRIMITIVE_RESTART: return StrCRef("PRIMITIVE_RESTART");
#endif
#if defined GL_DEPTH_TEST
	case GL_DEPTH_TEST: return StrCRef("DEPTH_TEST");
#endif
#if defined GL_STENCIL_TEST
	case GL_STENCIL_TEST: return StrCRef("STENCIL_TEST");
#endif
#if defined GL_SCISSOR_TEST
	case GL_SCISSOR_TEST: return StrCRef("SCISSOR_TEST");
#endif
#if defined GL_CULL_FACE
	case GL_CULL_FACE: return StrCRef("CULL_FACE");
#endif
#if defined GL_RASTERIZER_DISCARD
	case GL_RASTERIZER_DISCARD: return StrCRef("RASTERIZER_DISCARD");
#endif
#if defined GL_POLYGON_OFFSET_POINT
	case GL_POLYGON_OFFSET_POINT: return StrCRef("POLYGON_OFFSET_POINT");
#endif
#if defined GL_POLYGON_OFFSET_LINE
	case GL_POLYGON_OFFSET_LINE: return StrCRef("POLYGON_OFFSET_LINE");
#endif
#if defined GL_POLYGON_OFFSET_FILL
	case GL_POLYGON_OFFSET_FILL: return StrCRef("POLYGON_OFFSET_FILL");
#endif
#if defined GL_BLEND
	case GL_BLEND: return StrCRef("BLEND");
#endif
#if defined GL_COLOR_LOGIC_OP
	case GL_COLOR_LOGIC_OP: return StrCRef("COLOR_LOGIC_OP");
#endif
#if defined GL_DITHER
	case GL_DITHER: return StrCRef("DITHER");
#endif
#if defined GL_MULTISAMPLE
	case GL_MULTISAMPLE: return StrCRef("MULTISAMPLE");
#endif
#if defined GL_SAMPLE_SHADING
	case GL_SAMPLE_SHADING: return StrCRef("SAMPLE_SHADING");
#endif
#if defined GL_LINE_SMOOTH
	case GL_LINE_SMOOTH: return StrCRef("LINE_SMOOTH");
#endif
#if defined GL_POLYGON_SMOOTH
	case GL_POLYGON_SMOOTH: return StrCRef("POLYGON_SMOOTH");
#endif
#if defined GL_PROGRAM_POINT_SIZE
	case GL_PROGRAM_POINT_SIZE: return StrCRef("PROGRAM_POINT_SIZE");
#endif
#if defined GL_TEXTURE_CUBE_MAP_SEAMLESS
	case GL_TEXTURE_CUBE_MAP_SEAMLESS: return StrCRef("TEXTURE_CUBE_MAP_SEAMLESS");
#endif
#if defined GL_SAMPLE_ALPHA_TO_COVERAGE
	case GL_SAMPLE_ALPHA_TO_COVERAGE: return StrCRef("SAMPLE_ALPHA_TO_COVERAGE");
#endif
#if defined GL_SAMPLE_ALPHA_TO_ONE
	case GL_SAMPLE_ALPHA_TO_ONE: return StrCRef("SAMPLE_ALPHA_TO_ONE");
#endif
#if defined GL_SAMPLE_COVERAGE
	case GL_SAMPLE_COVERAGE: return StrCRef("SAMPLE_COVERAGE");
#endif
#if defined GL_SAMPLE_MASK
	case GL_SAMPLE_MASK: return StrCRef("SAMPLE_MASK");
#endif
#if defined GL_FRAMEBUFFER_SRGB
	case GL_FRAMEBUFFER_SRGB: return StrCRef("FRAMEBUFFER_SRGB");
#endif
#if defined GL_DEBUG_OUTPUT_SYNCHRONOUS
	case GL_DEBUG_OUTPUT_SYNCHRONOUS: return StrCRef("DEBUG_OUTPUT_SYNCHRONOUS");
#endif
#if defined GL_STREAM_RASTERIZATION_AMD
	case GL_STREAM_RASTERIZATION_AMD: return StrCRef("STREAM_RASTERIZATION_AMD");
#endif
#if defined GL_BLEND_ADVANCED_COHERENT_KHR
	case GL_BLEND_ADVANCED_COHERENT_KHR: return StrCRef("BLEND_ADVANCED_COHERENT_KHR");
#endif
#if defined GL_FRAGMENT_COVERAGE_TO_COLOR_NV
	case GL_FRAGMENT_COVERAGE_TO_COLOR_NV: return StrCRef("FRAGMENT_COVERAGE_TO_COLOR_NV");
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


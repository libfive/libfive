//  File implement/oglplus/enums/program_pipeline_stage_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/program_pipeline_stage.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	ProgramPipelineStage*,
	GLbitfield value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PROGRAMPIPELINESTAGE)
#define OGLPLUS_IMPL_EVN_PROGRAMPIPELINESTAGE
{
switch(value)
{
#if defined GL_VERTEX_SHADER_BIT
	case GL_VERTEX_SHADER_BIT: return StrCRef("VERTEX_SHADER_BIT");
#endif
#if defined GL_TESS_CONTROL_SHADER_BIT
	case GL_TESS_CONTROL_SHADER_BIT: return StrCRef("TESS_CONTROL_SHADER_BIT");
#endif
#if defined GL_TESS_EVALUATION_SHADER_BIT
	case GL_TESS_EVALUATION_SHADER_BIT: return StrCRef("TESS_EVALUATION_SHADER_BIT");
#endif
#if defined GL_GEOMETRY_SHADER_BIT
	case GL_GEOMETRY_SHADER_BIT: return StrCRef("GEOMETRY_SHADER_BIT");
#endif
#if defined GL_FRAGMENT_SHADER_BIT
	case GL_FRAGMENT_SHADER_BIT: return StrCRef("FRAGMENT_SHADER_BIT");
#endif
#if defined GL_COMPUTE_SHADER_BIT
	case GL_COMPUTE_SHADER_BIT: return StrCRef("COMPUTE_SHADER_BIT");
#endif
#if defined GL_ALL_SHADER_BITS
	case GL_ALL_SHADER_BITS: return StrCRef("ALL_SHADER_BITS");
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


//  File implement/oglplus/enums/program_pipeline_stage_class.ipp
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
template <typename Base, template<ProgramPipelineStage> class Transform>
class EnumToClass<Base, ProgramPipelineStage, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_VERTEX_SHADER_BIT
# if defined VertexShader
#  pragma push_macro("VertexShader")
#  undef VertexShader
	Transform<ProgramPipelineStage::VertexShader> VertexShader;
#  pragma pop_macro("VertexShader")
# else
	Transform<ProgramPipelineStage::VertexShader> VertexShader;
# endif
#endif
#if defined GL_TESS_CONTROL_SHADER_BIT
# if defined TessControlShader
#  pragma push_macro("TessControlShader")
#  undef TessControlShader
	Transform<ProgramPipelineStage::TessControlShader> TessControlShader;
#  pragma pop_macro("TessControlShader")
# else
	Transform<ProgramPipelineStage::TessControlShader> TessControlShader;
# endif
#endif
#if defined GL_TESS_EVALUATION_SHADER_BIT
# if defined TessEvaluationShader
#  pragma push_macro("TessEvaluationShader")
#  undef TessEvaluationShader
	Transform<ProgramPipelineStage::TessEvaluationShader> TessEvaluationShader;
#  pragma pop_macro("TessEvaluationShader")
# else
	Transform<ProgramPipelineStage::TessEvaluationShader> TessEvaluationShader;
# endif
#endif
#if defined GL_GEOMETRY_SHADER_BIT
# if defined GeometryShader
#  pragma push_macro("GeometryShader")
#  undef GeometryShader
	Transform<ProgramPipelineStage::GeometryShader> GeometryShader;
#  pragma pop_macro("GeometryShader")
# else
	Transform<ProgramPipelineStage::GeometryShader> GeometryShader;
# endif
#endif
#if defined GL_FRAGMENT_SHADER_BIT
# if defined FragmentShader
#  pragma push_macro("FragmentShader")
#  undef FragmentShader
	Transform<ProgramPipelineStage::FragmentShader> FragmentShader;
#  pragma pop_macro("FragmentShader")
# else
	Transform<ProgramPipelineStage::FragmentShader> FragmentShader;
# endif
#endif
#if defined GL_COMPUTE_SHADER_BIT
# if defined ComputeShader
#  pragma push_macro("ComputeShader")
#  undef ComputeShader
	Transform<ProgramPipelineStage::ComputeShader> ComputeShader;
#  pragma pop_macro("ComputeShader")
# else
	Transform<ProgramPipelineStage::ComputeShader> ComputeShader;
# endif
#endif
#if defined GL_ALL_SHADER_BITS
# if defined AllShaders
#  pragma push_macro("AllShaders")
#  undef AllShaders
	Transform<ProgramPipelineStage::AllShaders> AllShaders;
#  pragma pop_macro("AllShaders")
# else
	Transform<ProgramPipelineStage::AllShaders> AllShaders;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_VERTEX_SHADER_BIT
# if defined VertexShader
#  pragma push_macro("VertexShader")
#  undef VertexShader
	 , VertexShader(_base())
#  pragma pop_macro("VertexShader")
# else
	 , VertexShader(_base())
# endif
#endif
#if defined GL_TESS_CONTROL_SHADER_BIT
# if defined TessControlShader
#  pragma push_macro("TessControlShader")
#  undef TessControlShader
	 , TessControlShader(_base())
#  pragma pop_macro("TessControlShader")
# else
	 , TessControlShader(_base())
# endif
#endif
#if defined GL_TESS_EVALUATION_SHADER_BIT
# if defined TessEvaluationShader
#  pragma push_macro("TessEvaluationShader")
#  undef TessEvaluationShader
	 , TessEvaluationShader(_base())
#  pragma pop_macro("TessEvaluationShader")
# else
	 , TessEvaluationShader(_base())
# endif
#endif
#if defined GL_GEOMETRY_SHADER_BIT
# if defined GeometryShader
#  pragma push_macro("GeometryShader")
#  undef GeometryShader
	 , GeometryShader(_base())
#  pragma pop_macro("GeometryShader")
# else
	 , GeometryShader(_base())
# endif
#endif
#if defined GL_FRAGMENT_SHADER_BIT
# if defined FragmentShader
#  pragma push_macro("FragmentShader")
#  undef FragmentShader
	 , FragmentShader(_base())
#  pragma pop_macro("FragmentShader")
# else
	 , FragmentShader(_base())
# endif
#endif
#if defined GL_COMPUTE_SHADER_BIT
# if defined ComputeShader
#  pragma push_macro("ComputeShader")
#  undef ComputeShader
	 , ComputeShader(_base())
#  pragma pop_macro("ComputeShader")
# else
	 , ComputeShader(_base())
# endif
#endif
#if defined GL_ALL_SHADER_BITS
# if defined AllShaders
#  pragma push_macro("AllShaders")
#  undef AllShaders
	 , AllShaders(_base())
#  pragma pop_macro("AllShaders")
# else
	 , AllShaders(_base())
# endif
#endif
	{ }
};

} // namespace enums


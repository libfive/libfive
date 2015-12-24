//  File implement/oglplus/enums/query_target_class.ipp
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
template <typename Base, template<QueryTarget> class Transform>
class EnumToClass<Base, QueryTarget, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_TIME_ELAPSED
# if defined TimeElapsed
#  pragma push_macro("TimeElapsed")
#  undef TimeElapsed
	Transform<QueryTarget::TimeElapsed> TimeElapsed;
#  pragma pop_macro("TimeElapsed")
# else
	Transform<QueryTarget::TimeElapsed> TimeElapsed;
# endif
#endif
#if defined GL_TIMESTAMP
# if defined Timestamp
#  pragma push_macro("Timestamp")
#  undef Timestamp
	Transform<QueryTarget::Timestamp> Timestamp;
#  pragma pop_macro("Timestamp")
# else
	Transform<QueryTarget::Timestamp> Timestamp;
# endif
#endif
#if defined GL_SAMPLES_PASSED
# if defined SamplesPassed
#  pragma push_macro("SamplesPassed")
#  undef SamplesPassed
	Transform<QueryTarget::SamplesPassed> SamplesPassed;
#  pragma pop_macro("SamplesPassed")
# else
	Transform<QueryTarget::SamplesPassed> SamplesPassed;
# endif
#endif
#if defined GL_ANY_SAMPLES_PASSED
# if defined AnySamplesPassed
#  pragma push_macro("AnySamplesPassed")
#  undef AnySamplesPassed
	Transform<QueryTarget::AnySamplesPassed> AnySamplesPassed;
#  pragma pop_macro("AnySamplesPassed")
# else
	Transform<QueryTarget::AnySamplesPassed> AnySamplesPassed;
# endif
#endif
#if defined GL_PRIMITIVES_GENERATED
# if defined PrimitivesGenerated
#  pragma push_macro("PrimitivesGenerated")
#  undef PrimitivesGenerated
	Transform<QueryTarget::PrimitivesGenerated> PrimitivesGenerated;
#  pragma pop_macro("PrimitivesGenerated")
# else
	Transform<QueryTarget::PrimitivesGenerated> PrimitivesGenerated;
# endif
#endif
#if defined GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN
# if defined TransformFeedbackPrimitivesWritten
#  pragma push_macro("TransformFeedbackPrimitivesWritten")
#  undef TransformFeedbackPrimitivesWritten
	Transform<QueryTarget::TransformFeedbackPrimitivesWritten> TransformFeedbackPrimitivesWritten;
#  pragma pop_macro("TransformFeedbackPrimitivesWritten")
# else
	Transform<QueryTarget::TransformFeedbackPrimitivesWritten> TransformFeedbackPrimitivesWritten;
# endif
#endif
#if defined GL_VERTICES_SUBMITTED_ARB
# if defined VerticesSubmitted
#  pragma push_macro("VerticesSubmitted")
#  undef VerticesSubmitted
	Transform<QueryTarget::VerticesSubmitted> VerticesSubmitted;
#  pragma pop_macro("VerticesSubmitted")
# else
	Transform<QueryTarget::VerticesSubmitted> VerticesSubmitted;
# endif
#endif
#if defined GL_PRIMITIVES_SUBMITTED_ARB
# if defined PrimitivesSubmitted
#  pragma push_macro("PrimitivesSubmitted")
#  undef PrimitivesSubmitted
	Transform<QueryTarget::PrimitivesSubmitted> PrimitivesSubmitted;
#  pragma pop_macro("PrimitivesSubmitted")
# else
	Transform<QueryTarget::PrimitivesSubmitted> PrimitivesSubmitted;
# endif
#endif
#if defined GL_VERTEX_SHADER_INVOCATIONS_ARB
# if defined VertexShaderInvocations
#  pragma push_macro("VertexShaderInvocations")
#  undef VertexShaderInvocations
	Transform<QueryTarget::VertexShaderInvocations> VertexShaderInvocations;
#  pragma pop_macro("VertexShaderInvocations")
# else
	Transform<QueryTarget::VertexShaderInvocations> VertexShaderInvocations;
# endif
#endif
#if defined GL_TESS_CONTROL_SHADER_PATCHES_ARB
# if defined TessControlShaderPatches
#  pragma push_macro("TessControlShaderPatches")
#  undef TessControlShaderPatches
	Transform<QueryTarget::TessControlShaderPatches> TessControlShaderPatches;
#  pragma pop_macro("TessControlShaderPatches")
# else
	Transform<QueryTarget::TessControlShaderPatches> TessControlShaderPatches;
# endif
#endif
#if defined GL_TESS_EVALUATION_SHADER_INVOCATIONS_ARB
# if defined TessEvaluationShaderInvocations
#  pragma push_macro("TessEvaluationShaderInvocations")
#  undef TessEvaluationShaderInvocations
	Transform<QueryTarget::TessEvaluationShaderInvocations> TessEvaluationShaderInvocations;
#  pragma pop_macro("TessEvaluationShaderInvocations")
# else
	Transform<QueryTarget::TessEvaluationShaderInvocations> TessEvaluationShaderInvocations;
# endif
#endif
#if defined GL_GEOMETRY_SHADER_INVOCATIONS
# if defined GeometryShaderInvocations
#  pragma push_macro("GeometryShaderInvocations")
#  undef GeometryShaderInvocations
	Transform<QueryTarget::GeometryShaderInvocations> GeometryShaderInvocations;
#  pragma pop_macro("GeometryShaderInvocations")
# else
	Transform<QueryTarget::GeometryShaderInvocations> GeometryShaderInvocations;
# endif
#endif
#if defined GL_GEOMETRY_SHADER_PRIMITIVES_EMITTED_ARB
# if defined GeometryShaderPrimitivesEmitted
#  pragma push_macro("GeometryShaderPrimitivesEmitted")
#  undef GeometryShaderPrimitivesEmitted
	Transform<QueryTarget::GeometryShaderPrimitivesEmitted> GeometryShaderPrimitivesEmitted;
#  pragma pop_macro("GeometryShaderPrimitivesEmitted")
# else
	Transform<QueryTarget::GeometryShaderPrimitivesEmitted> GeometryShaderPrimitivesEmitted;
# endif
#endif
#if defined GL_FRAGMENT_SHADER_INVOCATIONS_ARB
# if defined FragmentShaderInvocations
#  pragma push_macro("FragmentShaderInvocations")
#  undef FragmentShaderInvocations
	Transform<QueryTarget::FragmentShaderInvocations> FragmentShaderInvocations;
#  pragma pop_macro("FragmentShaderInvocations")
# else
	Transform<QueryTarget::FragmentShaderInvocations> FragmentShaderInvocations;
# endif
#endif
#if defined GL_COMPUTE_SHADER_INVOCATIONS_ARB
# if defined ComputeShaderInvocations
#  pragma push_macro("ComputeShaderInvocations")
#  undef ComputeShaderInvocations
	Transform<QueryTarget::ComputeShaderInvocations> ComputeShaderInvocations;
#  pragma pop_macro("ComputeShaderInvocations")
# else
	Transform<QueryTarget::ComputeShaderInvocations> ComputeShaderInvocations;
# endif
#endif
#if defined GL_CLIPPING_INPUT_PRIMITIVES_ARB
# if defined ClippingInputPrimitives
#  pragma push_macro("ClippingInputPrimitives")
#  undef ClippingInputPrimitives
	Transform<QueryTarget::ClippingInputPrimitives> ClippingInputPrimitives;
#  pragma pop_macro("ClippingInputPrimitives")
# else
	Transform<QueryTarget::ClippingInputPrimitives> ClippingInputPrimitives;
# endif
#endif
#if defined GL_CLIPPING_OUTPUT_PRIMITIVES_ARB
# if defined ClippingOutputPrimitives
#  pragma push_macro("ClippingOutputPrimitives")
#  undef ClippingOutputPrimitives
	Transform<QueryTarget::ClippingOutputPrimitives> ClippingOutputPrimitives;
#  pragma pop_macro("ClippingOutputPrimitives")
# else
	Transform<QueryTarget::ClippingOutputPrimitives> ClippingOutputPrimitives;
# endif
#endif
#if defined GL_TRANSFORM_FEEDBACK_OVERFLOW_ARB
# if defined TransformFeedbackOverflow
#  pragma push_macro("TransformFeedbackOverflow")
#  undef TransformFeedbackOverflow
	Transform<QueryTarget::TransformFeedbackOverflow> TransformFeedbackOverflow;
#  pragma pop_macro("TransformFeedbackOverflow")
# else
	Transform<QueryTarget::TransformFeedbackOverflow> TransformFeedbackOverflow;
# endif
#endif
#if defined GL_TRANSFORM_FEEDBACK_STREAM_OVERFLOW_ARB
# if defined TransformFeedbackStreamOverflow
#  pragma push_macro("TransformFeedbackStreamOverflow")
#  undef TransformFeedbackStreamOverflow
	Transform<QueryTarget::TransformFeedbackStreamOverflow> TransformFeedbackStreamOverflow;
#  pragma pop_macro("TransformFeedbackStreamOverflow")
# else
	Transform<QueryTarget::TransformFeedbackStreamOverflow> TransformFeedbackStreamOverflow;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_TIME_ELAPSED
# if defined TimeElapsed
#  pragma push_macro("TimeElapsed")
#  undef TimeElapsed
	 , TimeElapsed(_base())
#  pragma pop_macro("TimeElapsed")
# else
	 , TimeElapsed(_base())
# endif
#endif
#if defined GL_TIMESTAMP
# if defined Timestamp
#  pragma push_macro("Timestamp")
#  undef Timestamp
	 , Timestamp(_base())
#  pragma pop_macro("Timestamp")
# else
	 , Timestamp(_base())
# endif
#endif
#if defined GL_SAMPLES_PASSED
# if defined SamplesPassed
#  pragma push_macro("SamplesPassed")
#  undef SamplesPassed
	 , SamplesPassed(_base())
#  pragma pop_macro("SamplesPassed")
# else
	 , SamplesPassed(_base())
# endif
#endif
#if defined GL_ANY_SAMPLES_PASSED
# if defined AnySamplesPassed
#  pragma push_macro("AnySamplesPassed")
#  undef AnySamplesPassed
	 , AnySamplesPassed(_base())
#  pragma pop_macro("AnySamplesPassed")
# else
	 , AnySamplesPassed(_base())
# endif
#endif
#if defined GL_PRIMITIVES_GENERATED
# if defined PrimitivesGenerated
#  pragma push_macro("PrimitivesGenerated")
#  undef PrimitivesGenerated
	 , PrimitivesGenerated(_base())
#  pragma pop_macro("PrimitivesGenerated")
# else
	 , PrimitivesGenerated(_base())
# endif
#endif
#if defined GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN
# if defined TransformFeedbackPrimitivesWritten
#  pragma push_macro("TransformFeedbackPrimitivesWritten")
#  undef TransformFeedbackPrimitivesWritten
	 , TransformFeedbackPrimitivesWritten(_base())
#  pragma pop_macro("TransformFeedbackPrimitivesWritten")
# else
	 , TransformFeedbackPrimitivesWritten(_base())
# endif
#endif
#if defined GL_VERTICES_SUBMITTED_ARB
# if defined VerticesSubmitted
#  pragma push_macro("VerticesSubmitted")
#  undef VerticesSubmitted
	 , VerticesSubmitted(_base())
#  pragma pop_macro("VerticesSubmitted")
# else
	 , VerticesSubmitted(_base())
# endif
#endif
#if defined GL_PRIMITIVES_SUBMITTED_ARB
# if defined PrimitivesSubmitted
#  pragma push_macro("PrimitivesSubmitted")
#  undef PrimitivesSubmitted
	 , PrimitivesSubmitted(_base())
#  pragma pop_macro("PrimitivesSubmitted")
# else
	 , PrimitivesSubmitted(_base())
# endif
#endif
#if defined GL_VERTEX_SHADER_INVOCATIONS_ARB
# if defined VertexShaderInvocations
#  pragma push_macro("VertexShaderInvocations")
#  undef VertexShaderInvocations
	 , VertexShaderInvocations(_base())
#  pragma pop_macro("VertexShaderInvocations")
# else
	 , VertexShaderInvocations(_base())
# endif
#endif
#if defined GL_TESS_CONTROL_SHADER_PATCHES_ARB
# if defined TessControlShaderPatches
#  pragma push_macro("TessControlShaderPatches")
#  undef TessControlShaderPatches
	 , TessControlShaderPatches(_base())
#  pragma pop_macro("TessControlShaderPatches")
# else
	 , TessControlShaderPatches(_base())
# endif
#endif
#if defined GL_TESS_EVALUATION_SHADER_INVOCATIONS_ARB
# if defined TessEvaluationShaderInvocations
#  pragma push_macro("TessEvaluationShaderInvocations")
#  undef TessEvaluationShaderInvocations
	 , TessEvaluationShaderInvocations(_base())
#  pragma pop_macro("TessEvaluationShaderInvocations")
# else
	 , TessEvaluationShaderInvocations(_base())
# endif
#endif
#if defined GL_GEOMETRY_SHADER_INVOCATIONS
# if defined GeometryShaderInvocations
#  pragma push_macro("GeometryShaderInvocations")
#  undef GeometryShaderInvocations
	 , GeometryShaderInvocations(_base())
#  pragma pop_macro("GeometryShaderInvocations")
# else
	 , GeometryShaderInvocations(_base())
# endif
#endif
#if defined GL_GEOMETRY_SHADER_PRIMITIVES_EMITTED_ARB
# if defined GeometryShaderPrimitivesEmitted
#  pragma push_macro("GeometryShaderPrimitivesEmitted")
#  undef GeometryShaderPrimitivesEmitted
	 , GeometryShaderPrimitivesEmitted(_base())
#  pragma pop_macro("GeometryShaderPrimitivesEmitted")
# else
	 , GeometryShaderPrimitivesEmitted(_base())
# endif
#endif
#if defined GL_FRAGMENT_SHADER_INVOCATIONS_ARB
# if defined FragmentShaderInvocations
#  pragma push_macro("FragmentShaderInvocations")
#  undef FragmentShaderInvocations
	 , FragmentShaderInvocations(_base())
#  pragma pop_macro("FragmentShaderInvocations")
# else
	 , FragmentShaderInvocations(_base())
# endif
#endif
#if defined GL_COMPUTE_SHADER_INVOCATIONS_ARB
# if defined ComputeShaderInvocations
#  pragma push_macro("ComputeShaderInvocations")
#  undef ComputeShaderInvocations
	 , ComputeShaderInvocations(_base())
#  pragma pop_macro("ComputeShaderInvocations")
# else
	 , ComputeShaderInvocations(_base())
# endif
#endif
#if defined GL_CLIPPING_INPUT_PRIMITIVES_ARB
# if defined ClippingInputPrimitives
#  pragma push_macro("ClippingInputPrimitives")
#  undef ClippingInputPrimitives
	 , ClippingInputPrimitives(_base())
#  pragma pop_macro("ClippingInputPrimitives")
# else
	 , ClippingInputPrimitives(_base())
# endif
#endif
#if defined GL_CLIPPING_OUTPUT_PRIMITIVES_ARB
# if defined ClippingOutputPrimitives
#  pragma push_macro("ClippingOutputPrimitives")
#  undef ClippingOutputPrimitives
	 , ClippingOutputPrimitives(_base())
#  pragma pop_macro("ClippingOutputPrimitives")
# else
	 , ClippingOutputPrimitives(_base())
# endif
#endif
#if defined GL_TRANSFORM_FEEDBACK_OVERFLOW_ARB
# if defined TransformFeedbackOverflow
#  pragma push_macro("TransformFeedbackOverflow")
#  undef TransformFeedbackOverflow
	 , TransformFeedbackOverflow(_base())
#  pragma pop_macro("TransformFeedbackOverflow")
# else
	 , TransformFeedbackOverflow(_base())
# endif
#endif
#if defined GL_TRANSFORM_FEEDBACK_STREAM_OVERFLOW_ARB
# if defined TransformFeedbackStreamOverflow
#  pragma push_macro("TransformFeedbackStreamOverflow")
#  undef TransformFeedbackStreamOverflow
	 , TransformFeedbackStreamOverflow(_base())
#  pragma pop_macro("TransformFeedbackStreamOverflow")
# else
	 , TransformFeedbackStreamOverflow(_base())
# endif
#endif
	{ }
};

} // namespace enums


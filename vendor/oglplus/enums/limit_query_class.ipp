//  File implement/oglplus/enums/limit_query_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/limit_query.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<LimitQuery> class Transform>
class EnumToClass<Base, LimitQuery, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_MAX_3D_TEXTURE_SIZE
# if defined Max3DTextureSize
#  pragma push_macro("Max3DTextureSize")
#  undef Max3DTextureSize
	Transform<LimitQuery::Max3DTextureSize> Max3DTextureSize;
#  pragma pop_macro("Max3DTextureSize")
# else
	Transform<LimitQuery::Max3DTextureSize> Max3DTextureSize;
# endif
#endif
#if defined GL_MAX_ARRAY_TEXTURE_LAYERS
# if defined MaxArrayTextureLayers
#  pragma push_macro("MaxArrayTextureLayers")
#  undef MaxArrayTextureLayers
	Transform<LimitQuery::MaxArrayTextureLayers> MaxArrayTextureLayers;
#  pragma pop_macro("MaxArrayTextureLayers")
# else
	Transform<LimitQuery::MaxArrayTextureLayers> MaxArrayTextureLayers;
# endif
#endif
#if defined GL_MAX_ATOMIC_COUNTER_BUFFER_BINDINGS
# if defined MaxAtomicCounterBufferBindings
#  pragma push_macro("MaxAtomicCounterBufferBindings")
#  undef MaxAtomicCounterBufferBindings
	Transform<LimitQuery::MaxAtomicCounterBufferBindings> MaxAtomicCounterBufferBindings;
#  pragma pop_macro("MaxAtomicCounterBufferBindings")
# else
	Transform<LimitQuery::MaxAtomicCounterBufferBindings> MaxAtomicCounterBufferBindings;
# endif
#endif
#if defined GL_MAX_ATOMIC_COUNTER_BUFFER_SIZE
# if defined MaxAtomicCounterBufferSize
#  pragma push_macro("MaxAtomicCounterBufferSize")
#  undef MaxAtomicCounterBufferSize
	Transform<LimitQuery::MaxAtomicCounterBufferSize> MaxAtomicCounterBufferSize;
#  pragma pop_macro("MaxAtomicCounterBufferSize")
# else
	Transform<LimitQuery::MaxAtomicCounterBufferSize> MaxAtomicCounterBufferSize;
# endif
#endif
#if defined GL_MAX_CLIP_DISTANCES
# if defined MaxClipDistances
#  pragma push_macro("MaxClipDistances")
#  undef MaxClipDistances
	Transform<LimitQuery::MaxClipDistances> MaxClipDistances;
#  pragma pop_macro("MaxClipDistances")
# else
	Transform<LimitQuery::MaxClipDistances> MaxClipDistances;
# endif
#endif
#if defined GL_MAX_CULL_DISTANCES
# if defined MaxCullDistances
#  pragma push_macro("MaxCullDistances")
#  undef MaxCullDistances
	Transform<LimitQuery::MaxCullDistances> MaxCullDistances;
#  pragma pop_macro("MaxCullDistances")
# else
	Transform<LimitQuery::MaxCullDistances> MaxCullDistances;
# endif
#endif
#if defined GL_MAX_COMBINED_CLIP_AND_CULL_DISTANCES
# if defined MaxCombinedClipAndCullDistances
#  pragma push_macro("MaxCombinedClipAndCullDistances")
#  undef MaxCombinedClipAndCullDistances
	Transform<LimitQuery::MaxCombinedClipAndCullDistances> MaxCombinedClipAndCullDistances;
#  pragma pop_macro("MaxCombinedClipAndCullDistances")
# else
	Transform<LimitQuery::MaxCombinedClipAndCullDistances> MaxCombinedClipAndCullDistances;
# endif
#endif
#if defined GL_MAX_COLOR_ATTACHMENTS
# if defined MaxColorAttachments
#  pragma push_macro("MaxColorAttachments")
#  undef MaxColorAttachments
	Transform<LimitQuery::MaxColorAttachments> MaxColorAttachments;
#  pragma pop_macro("MaxColorAttachments")
# else
	Transform<LimitQuery::MaxColorAttachments> MaxColorAttachments;
# endif
#endif
#if defined GL_MAX_COLOR_TEXTURE_SAMPLES
# if defined MaxColorTextureSamples
#  pragma push_macro("MaxColorTextureSamples")
#  undef MaxColorTextureSamples
	Transform<LimitQuery::MaxColorTextureSamples> MaxColorTextureSamples;
#  pragma pop_macro("MaxColorTextureSamples")
# else
	Transform<LimitQuery::MaxColorTextureSamples> MaxColorTextureSamples;
# endif
#endif
#if defined GL_MAX_COMBINED_ATOMIC_COUNTER_BUFFERS
# if defined MaxCombinedAtomicCounterBuffers
#  pragma push_macro("MaxCombinedAtomicCounterBuffers")
#  undef MaxCombinedAtomicCounterBuffers
	Transform<LimitQuery::MaxCombinedAtomicCounterBuffers> MaxCombinedAtomicCounterBuffers;
#  pragma pop_macro("MaxCombinedAtomicCounterBuffers")
# else
	Transform<LimitQuery::MaxCombinedAtomicCounterBuffers> MaxCombinedAtomicCounterBuffers;
# endif
#endif
#if defined GL_MAX_COMBINED_ATOMIC_COUNTERS
# if defined MaxCombinedAtomicCounters
#  pragma push_macro("MaxCombinedAtomicCounters")
#  undef MaxCombinedAtomicCounters
	Transform<LimitQuery::MaxCombinedAtomicCounters> MaxCombinedAtomicCounters;
#  pragma pop_macro("MaxCombinedAtomicCounters")
# else
	Transform<LimitQuery::MaxCombinedAtomicCounters> MaxCombinedAtomicCounters;
# endif
#endif
#if defined GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS
# if defined MaxCombinedFragmentUniformComponents
#  pragma push_macro("MaxCombinedFragmentUniformComponents")
#  undef MaxCombinedFragmentUniformComponents
	Transform<LimitQuery::MaxCombinedFragmentUniformComponents> MaxCombinedFragmentUniformComponents;
#  pragma pop_macro("MaxCombinedFragmentUniformComponents")
# else
	Transform<LimitQuery::MaxCombinedFragmentUniformComponents> MaxCombinedFragmentUniformComponents;
# endif
#endif
#if defined GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS
# if defined MaxCombinedGeometryUniformComponents
#  pragma push_macro("MaxCombinedGeometryUniformComponents")
#  undef MaxCombinedGeometryUniformComponents
	Transform<LimitQuery::MaxCombinedGeometryUniformComponents> MaxCombinedGeometryUniformComponents;
#  pragma pop_macro("MaxCombinedGeometryUniformComponents")
# else
	Transform<LimitQuery::MaxCombinedGeometryUniformComponents> MaxCombinedGeometryUniformComponents;
# endif
#endif
#if defined GL_MAX_COMBINED_IMAGE_UNIFORMS
# if defined MaxCombinedImageUniforms
#  pragma push_macro("MaxCombinedImageUniforms")
#  undef MaxCombinedImageUniforms
	Transform<LimitQuery::MaxCombinedImageUniforms> MaxCombinedImageUniforms;
#  pragma pop_macro("MaxCombinedImageUniforms")
# else
	Transform<LimitQuery::MaxCombinedImageUniforms> MaxCombinedImageUniforms;
# endif
#endif
#if defined GL_MAX_COMBINED_IMAGE_UNITS_AND_FRAGMENT_OUTPUTS
# if defined MaxCombinedImageUnitsAndFragmentOutputs
#  pragma push_macro("MaxCombinedImageUnitsAndFragmentOutputs")
#  undef MaxCombinedImageUnitsAndFragmentOutputs
	Transform<LimitQuery::MaxCombinedImageUnitsAndFragmentOutputs> MaxCombinedImageUnitsAndFragmentOutputs;
#  pragma pop_macro("MaxCombinedImageUnitsAndFragmentOutputs")
# else
	Transform<LimitQuery::MaxCombinedImageUnitsAndFragmentOutputs> MaxCombinedImageUnitsAndFragmentOutputs;
# endif
#endif
#if defined GL_MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS
# if defined MaxCombinedTessControlUniformComponents
#  pragma push_macro("MaxCombinedTessControlUniformComponents")
#  undef MaxCombinedTessControlUniformComponents
	Transform<LimitQuery::MaxCombinedTessControlUniformComponents> MaxCombinedTessControlUniformComponents;
#  pragma pop_macro("MaxCombinedTessControlUniformComponents")
# else
	Transform<LimitQuery::MaxCombinedTessControlUniformComponents> MaxCombinedTessControlUniformComponents;
# endif
#endif
#if defined GL_MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS
# if defined MaxCombinedTessEvaluationUniformComponents
#  pragma push_macro("MaxCombinedTessEvaluationUniformComponents")
#  undef MaxCombinedTessEvaluationUniformComponents
	Transform<LimitQuery::MaxCombinedTessEvaluationUniformComponents> MaxCombinedTessEvaluationUniformComponents;
#  pragma pop_macro("MaxCombinedTessEvaluationUniformComponents")
# else
	Transform<LimitQuery::MaxCombinedTessEvaluationUniformComponents> MaxCombinedTessEvaluationUniformComponents;
# endif
#endif
#if defined GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
# if defined MaxCombinedTextureImageUnits
#  pragma push_macro("MaxCombinedTextureImageUnits")
#  undef MaxCombinedTextureImageUnits
	Transform<LimitQuery::MaxCombinedTextureImageUnits> MaxCombinedTextureImageUnits;
#  pragma pop_macro("MaxCombinedTextureImageUnits")
# else
	Transform<LimitQuery::MaxCombinedTextureImageUnits> MaxCombinedTextureImageUnits;
# endif
#endif
#if defined GL_MAX_COMBINED_UNIFORM_BLOCKS
# if defined MaxCombinedUniformBlocks
#  pragma push_macro("MaxCombinedUniformBlocks")
#  undef MaxCombinedUniformBlocks
	Transform<LimitQuery::MaxCombinedUniformBlocks> MaxCombinedUniformBlocks;
#  pragma pop_macro("MaxCombinedUniformBlocks")
# else
	Transform<LimitQuery::MaxCombinedUniformBlocks> MaxCombinedUniformBlocks;
# endif
#endif
#if defined GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS
# if defined MaxCombinedVertexUniformComponents
#  pragma push_macro("MaxCombinedVertexUniformComponents")
#  undef MaxCombinedVertexUniformComponents
	Transform<LimitQuery::MaxCombinedVertexUniformComponents> MaxCombinedVertexUniformComponents;
#  pragma pop_macro("MaxCombinedVertexUniformComponents")
# else
	Transform<LimitQuery::MaxCombinedVertexUniformComponents> MaxCombinedVertexUniformComponents;
# endif
#endif
#if defined GL_MAX_CUBE_MAP_TEXTURE_SIZE
# if defined MaxCubeMapTextureSize
#  pragma push_macro("MaxCubeMapTextureSize")
#  undef MaxCubeMapTextureSize
	Transform<LimitQuery::MaxCubeMapTextureSize> MaxCubeMapTextureSize;
#  pragma pop_macro("MaxCubeMapTextureSize")
# else
	Transform<LimitQuery::MaxCubeMapTextureSize> MaxCubeMapTextureSize;
# endif
#endif
#if defined GL_MAX_DEPTH_TEXTURE_SAMPLES
# if defined MaxDepthTextureSamples
#  pragma push_macro("MaxDepthTextureSamples")
#  undef MaxDepthTextureSamples
	Transform<LimitQuery::MaxDepthTextureSamples> MaxDepthTextureSamples;
#  pragma pop_macro("MaxDepthTextureSamples")
# else
	Transform<LimitQuery::MaxDepthTextureSamples> MaxDepthTextureSamples;
# endif
#endif
#if defined GL_MAX_DRAW_BUFFERS
# if defined MaxDrawBuffers
#  pragma push_macro("MaxDrawBuffers")
#  undef MaxDrawBuffers
	Transform<LimitQuery::MaxDrawBuffers> MaxDrawBuffers;
#  pragma pop_macro("MaxDrawBuffers")
# else
	Transform<LimitQuery::MaxDrawBuffers> MaxDrawBuffers;
# endif
#endif
#if defined GL_MAX_DUAL_SOURCE_DRAW_BUFFERS
# if defined MaxDualSourceDrawBuffers
#  pragma push_macro("MaxDualSourceDrawBuffers")
#  undef MaxDualSourceDrawBuffers
	Transform<LimitQuery::MaxDualSourceDrawBuffers> MaxDualSourceDrawBuffers;
#  pragma pop_macro("MaxDualSourceDrawBuffers")
# else
	Transform<LimitQuery::MaxDualSourceDrawBuffers> MaxDualSourceDrawBuffers;
# endif
#endif
#if defined GL_MAX_ELEMENTS_INDICES
# if defined MaxElementsIndices
#  pragma push_macro("MaxElementsIndices")
#  undef MaxElementsIndices
	Transform<LimitQuery::MaxElementsIndices> MaxElementsIndices;
#  pragma pop_macro("MaxElementsIndices")
# else
	Transform<LimitQuery::MaxElementsIndices> MaxElementsIndices;
# endif
#endif
#if defined GL_MAX_ELEMENTS_VERTICES
# if defined MaxElementsVertices
#  pragma push_macro("MaxElementsVertices")
#  undef MaxElementsVertices
	Transform<LimitQuery::MaxElementsVertices> MaxElementsVertices;
#  pragma pop_macro("MaxElementsVertices")
# else
	Transform<LimitQuery::MaxElementsVertices> MaxElementsVertices;
# endif
#endif
#if defined GL_MAX_FRAGMENT_ATOMIC_COUNTER_BUFFERS
# if defined MaxFragmentAtomicCounterBuffers
#  pragma push_macro("MaxFragmentAtomicCounterBuffers")
#  undef MaxFragmentAtomicCounterBuffers
	Transform<LimitQuery::MaxFragmentAtomicCounterBuffers> MaxFragmentAtomicCounterBuffers;
#  pragma pop_macro("MaxFragmentAtomicCounterBuffers")
# else
	Transform<LimitQuery::MaxFragmentAtomicCounterBuffers> MaxFragmentAtomicCounterBuffers;
# endif
#endif
#if defined GL_MAX_FRAGMENT_ATOMIC_COUNTERS
# if defined MaxFragmentAtomicCounters
#  pragma push_macro("MaxFragmentAtomicCounters")
#  undef MaxFragmentAtomicCounters
	Transform<LimitQuery::MaxFragmentAtomicCounters> MaxFragmentAtomicCounters;
#  pragma pop_macro("MaxFragmentAtomicCounters")
# else
	Transform<LimitQuery::MaxFragmentAtomicCounters> MaxFragmentAtomicCounters;
# endif
#endif
#if defined GL_MAX_FRAGMENT_IMAGE_UNIFORMS
# if defined MaxFragmentImageUniforms
#  pragma push_macro("MaxFragmentImageUniforms")
#  undef MaxFragmentImageUniforms
	Transform<LimitQuery::MaxFragmentImageUniforms> MaxFragmentImageUniforms;
#  pragma pop_macro("MaxFragmentImageUniforms")
# else
	Transform<LimitQuery::MaxFragmentImageUniforms> MaxFragmentImageUniforms;
# endif
#endif
#if defined GL_MAX_FRAGMENT_INPUT_COMPONENTS
# if defined MaxFragmentInputComponents
#  pragma push_macro("MaxFragmentInputComponents")
#  undef MaxFragmentInputComponents
	Transform<LimitQuery::MaxFragmentInputComponents> MaxFragmentInputComponents;
#  pragma pop_macro("MaxFragmentInputComponents")
# else
	Transform<LimitQuery::MaxFragmentInputComponents> MaxFragmentInputComponents;
# endif
#endif
#if defined GL_MAX_FRAGMENT_INTERPOLATION_OFFSET
# if defined MaxFragmentInterpolationOffset
#  pragma push_macro("MaxFragmentInterpolationOffset")
#  undef MaxFragmentInterpolationOffset
	Transform<LimitQuery::MaxFragmentInterpolationOffset> MaxFragmentInterpolationOffset;
#  pragma pop_macro("MaxFragmentInterpolationOffset")
# else
	Transform<LimitQuery::MaxFragmentInterpolationOffset> MaxFragmentInterpolationOffset;
# endif
#endif
#if defined GL_MAX_FRAGMENT_UNIFORM_BLOCKS
# if defined MaxFragmentUniformBlocks
#  pragma push_macro("MaxFragmentUniformBlocks")
#  undef MaxFragmentUniformBlocks
	Transform<LimitQuery::MaxFragmentUniformBlocks> MaxFragmentUniformBlocks;
#  pragma pop_macro("MaxFragmentUniformBlocks")
# else
	Transform<LimitQuery::MaxFragmentUniformBlocks> MaxFragmentUniformBlocks;
# endif
#endif
#if defined GL_MAX_FRAGMENT_UNIFORM_COMPONENTS
# if defined MaxFragmentUniformComponents
#  pragma push_macro("MaxFragmentUniformComponents")
#  undef MaxFragmentUniformComponents
	Transform<LimitQuery::MaxFragmentUniformComponents> MaxFragmentUniformComponents;
#  pragma pop_macro("MaxFragmentUniformComponents")
# else
	Transform<LimitQuery::MaxFragmentUniformComponents> MaxFragmentUniformComponents;
# endif
#endif
#if defined GL_MAX_FRAGMENT_UNIFORM_VECTORS
# if defined MaxFragmentUniformVectors
#  pragma push_macro("MaxFragmentUniformVectors")
#  undef MaxFragmentUniformVectors
	Transform<LimitQuery::MaxFragmentUniformVectors> MaxFragmentUniformVectors;
#  pragma pop_macro("MaxFragmentUniformVectors")
# else
	Transform<LimitQuery::MaxFragmentUniformVectors> MaxFragmentUniformVectors;
# endif
#endif
#if defined GL_MAX_GEOMETRY_ATOMIC_COUNTER_BUFFERS
# if defined MaxGeometryAtomicCounterBuffers
#  pragma push_macro("MaxGeometryAtomicCounterBuffers")
#  undef MaxGeometryAtomicCounterBuffers
	Transform<LimitQuery::MaxGeometryAtomicCounterBuffers> MaxGeometryAtomicCounterBuffers;
#  pragma pop_macro("MaxGeometryAtomicCounterBuffers")
# else
	Transform<LimitQuery::MaxGeometryAtomicCounterBuffers> MaxGeometryAtomicCounterBuffers;
# endif
#endif
#if defined GL_MAX_GEOMETRY_ATOMIC_COUNTERS
# if defined MaxGeometryAtomicCounters
#  pragma push_macro("MaxGeometryAtomicCounters")
#  undef MaxGeometryAtomicCounters
	Transform<LimitQuery::MaxGeometryAtomicCounters> MaxGeometryAtomicCounters;
#  pragma pop_macro("MaxGeometryAtomicCounters")
# else
	Transform<LimitQuery::MaxGeometryAtomicCounters> MaxGeometryAtomicCounters;
# endif
#endif
#if defined GL_MAX_GEOMETRY_IMAGE_UNIFORMS
# if defined MaxGeometryImageUniforms
#  pragma push_macro("MaxGeometryImageUniforms")
#  undef MaxGeometryImageUniforms
	Transform<LimitQuery::MaxGeometryImageUniforms> MaxGeometryImageUniforms;
#  pragma pop_macro("MaxGeometryImageUniforms")
# else
	Transform<LimitQuery::MaxGeometryImageUniforms> MaxGeometryImageUniforms;
# endif
#endif
#if defined GL_MAX_GEOMETRY_INPUT_COMPONENTS
# if defined MaxGeometryInputComponents
#  pragma push_macro("MaxGeometryInputComponents")
#  undef MaxGeometryInputComponents
	Transform<LimitQuery::MaxGeometryInputComponents> MaxGeometryInputComponents;
#  pragma pop_macro("MaxGeometryInputComponents")
# else
	Transform<LimitQuery::MaxGeometryInputComponents> MaxGeometryInputComponents;
# endif
#endif
#if defined GL_MAX_GEOMETRY_OUTPUT_COMPONENTS
# if defined MaxGeometryOutputComponents
#  pragma push_macro("MaxGeometryOutputComponents")
#  undef MaxGeometryOutputComponents
	Transform<LimitQuery::MaxGeometryOutputComponents> MaxGeometryOutputComponents;
#  pragma pop_macro("MaxGeometryOutputComponents")
# else
	Transform<LimitQuery::MaxGeometryOutputComponents> MaxGeometryOutputComponents;
# endif
#endif
#if defined GL_MAX_GEOMETRY_OUTPUT_VERTICES
# if defined MaxGeometryOutputVertices
#  pragma push_macro("MaxGeometryOutputVertices")
#  undef MaxGeometryOutputVertices
	Transform<LimitQuery::MaxGeometryOutputVertices> MaxGeometryOutputVertices;
#  pragma pop_macro("MaxGeometryOutputVertices")
# else
	Transform<LimitQuery::MaxGeometryOutputVertices> MaxGeometryOutputVertices;
# endif
#endif
#if defined GL_MAX_GEOMETRY_SHADER_INVOCATIONS
# if defined MaxGeometryShaderInvocations
#  pragma push_macro("MaxGeometryShaderInvocations")
#  undef MaxGeometryShaderInvocations
	Transform<LimitQuery::MaxGeometryShaderInvocations> MaxGeometryShaderInvocations;
#  pragma pop_macro("MaxGeometryShaderInvocations")
# else
	Transform<LimitQuery::MaxGeometryShaderInvocations> MaxGeometryShaderInvocations;
# endif
#endif
#if defined GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS
# if defined MaxGeometryTextureImageUnits
#  pragma push_macro("MaxGeometryTextureImageUnits")
#  undef MaxGeometryTextureImageUnits
	Transform<LimitQuery::MaxGeometryTextureImageUnits> MaxGeometryTextureImageUnits;
#  pragma pop_macro("MaxGeometryTextureImageUnits")
# else
	Transform<LimitQuery::MaxGeometryTextureImageUnits> MaxGeometryTextureImageUnits;
# endif
#endif
#if defined GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS
# if defined MaxGeometryTotalOutputComponents
#  pragma push_macro("MaxGeometryTotalOutputComponents")
#  undef MaxGeometryTotalOutputComponents
	Transform<LimitQuery::MaxGeometryTotalOutputComponents> MaxGeometryTotalOutputComponents;
#  pragma pop_macro("MaxGeometryTotalOutputComponents")
# else
	Transform<LimitQuery::MaxGeometryTotalOutputComponents> MaxGeometryTotalOutputComponents;
# endif
#endif
#if defined GL_MAX_GEOMETRY_UNIFORM_BLOCKS
# if defined MaxGeometryUniformBlocks
#  pragma push_macro("MaxGeometryUniformBlocks")
#  undef MaxGeometryUniformBlocks
	Transform<LimitQuery::MaxGeometryUniformBlocks> MaxGeometryUniformBlocks;
#  pragma pop_macro("MaxGeometryUniformBlocks")
# else
	Transform<LimitQuery::MaxGeometryUniformBlocks> MaxGeometryUniformBlocks;
# endif
#endif
#if defined GL_MAX_GEOMETRY_UNIFORM_COMPONENTS
# if defined MaxGeometryUniformComponents
#  pragma push_macro("MaxGeometryUniformComponents")
#  undef MaxGeometryUniformComponents
	Transform<LimitQuery::MaxGeometryUniformComponents> MaxGeometryUniformComponents;
#  pragma pop_macro("MaxGeometryUniformComponents")
# else
	Transform<LimitQuery::MaxGeometryUniformComponents> MaxGeometryUniformComponents;
# endif
#endif
#if defined GL_MAX_IMAGE_SAMPLES
# if defined MaxImageSamples
#  pragma push_macro("MaxImageSamples")
#  undef MaxImageSamples
	Transform<LimitQuery::MaxImageSamples> MaxImageSamples;
#  pragma pop_macro("MaxImageSamples")
# else
	Transform<LimitQuery::MaxImageSamples> MaxImageSamples;
# endif
#endif
#if defined GL_MAX_IMAGE_UNITS
# if defined MaxImageUnits
#  pragma push_macro("MaxImageUnits")
#  undef MaxImageUnits
	Transform<LimitQuery::MaxImageUnits> MaxImageUnits;
#  pragma pop_macro("MaxImageUnits")
# else
	Transform<LimitQuery::MaxImageUnits> MaxImageUnits;
# endif
#endif
#if defined GL_MAX_INTEGER_SAMPLES
# if defined MaxIntegerSamples
#  pragma push_macro("MaxIntegerSamples")
#  undef MaxIntegerSamples
	Transform<LimitQuery::MaxIntegerSamples> MaxIntegerSamples;
#  pragma pop_macro("MaxIntegerSamples")
# else
	Transform<LimitQuery::MaxIntegerSamples> MaxIntegerSamples;
# endif
#endif
#if defined GL_MAX_PATCH_VERTICES
# if defined MaxPatchVertices
#  pragma push_macro("MaxPatchVertices")
#  undef MaxPatchVertices
	Transform<LimitQuery::MaxPatchVertices> MaxPatchVertices;
#  pragma pop_macro("MaxPatchVertices")
# else
	Transform<LimitQuery::MaxPatchVertices> MaxPatchVertices;
# endif
#endif
#if defined GL_MAX_PROGRAM_TEXEL_OFFSET
# if defined MaxProgramTexelOffset
#  pragma push_macro("MaxProgramTexelOffset")
#  undef MaxProgramTexelOffset
	Transform<LimitQuery::MaxProgramTexelOffset> MaxProgramTexelOffset;
#  pragma pop_macro("MaxProgramTexelOffset")
# else
	Transform<LimitQuery::MaxProgramTexelOffset> MaxProgramTexelOffset;
# endif
#endif
#if defined GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET
# if defined MaxProgramTextureGatherOffset
#  pragma push_macro("MaxProgramTextureGatherOffset")
#  undef MaxProgramTextureGatherOffset
	Transform<LimitQuery::MaxProgramTextureGatherOffset> MaxProgramTextureGatherOffset;
#  pragma pop_macro("MaxProgramTextureGatherOffset")
# else
	Transform<LimitQuery::MaxProgramTextureGatherOffset> MaxProgramTextureGatherOffset;
# endif
#endif
#if defined GL_MAX_RECTANGLE_TEXTURE_SIZE
# if defined MaxRectangleTextureSize
#  pragma push_macro("MaxRectangleTextureSize")
#  undef MaxRectangleTextureSize
	Transform<LimitQuery::MaxRectangleTextureSize> MaxRectangleTextureSize;
#  pragma pop_macro("MaxRectangleTextureSize")
# else
	Transform<LimitQuery::MaxRectangleTextureSize> MaxRectangleTextureSize;
# endif
#endif
#if defined GL_MAX_RENDERBUFFER_SIZE
# if defined MaxRenderbufferSize
#  pragma push_macro("MaxRenderbufferSize")
#  undef MaxRenderbufferSize
	Transform<LimitQuery::MaxRenderbufferSize> MaxRenderbufferSize;
#  pragma pop_macro("MaxRenderbufferSize")
# else
	Transform<LimitQuery::MaxRenderbufferSize> MaxRenderbufferSize;
# endif
#endif
#if defined GL_MAX_SAMPLE_MASK_WORDS
# if defined MaxSampleMaskWords
#  pragma push_macro("MaxSampleMaskWords")
#  undef MaxSampleMaskWords
	Transform<LimitQuery::MaxSampleMaskWords> MaxSampleMaskWords;
#  pragma pop_macro("MaxSampleMaskWords")
# else
	Transform<LimitQuery::MaxSampleMaskWords> MaxSampleMaskWords;
# endif
#endif
#if defined GL_MAX_SAMPLES
# if defined MaxSamples
#  pragma push_macro("MaxSamples")
#  undef MaxSamples
	Transform<LimitQuery::MaxSamples> MaxSamples;
#  pragma pop_macro("MaxSamples")
# else
	Transform<LimitQuery::MaxSamples> MaxSamples;
# endif
#endif
#if defined GL_MAX_SERVER_WAIT_TIMEOUT
# if defined MaxServerWaitTimeout
#  pragma push_macro("MaxServerWaitTimeout")
#  undef MaxServerWaitTimeout
	Transform<LimitQuery::MaxServerWaitTimeout> MaxServerWaitTimeout;
#  pragma pop_macro("MaxServerWaitTimeout")
# else
	Transform<LimitQuery::MaxServerWaitTimeout> MaxServerWaitTimeout;
# endif
#endif
#if defined GL_MAX_SUBROUTINES
# if defined MaxSubroutines
#  pragma push_macro("MaxSubroutines")
#  undef MaxSubroutines
	Transform<LimitQuery::MaxSubroutines> MaxSubroutines;
#  pragma pop_macro("MaxSubroutines")
# else
	Transform<LimitQuery::MaxSubroutines> MaxSubroutines;
# endif
#endif
#if defined GL_MAX_SUBROUTINE_UNIFORM_LOCATIONS
# if defined MaxSubroutineUniformLocations
#  pragma push_macro("MaxSubroutineUniformLocations")
#  undef MaxSubroutineUniformLocations
	Transform<LimitQuery::MaxSubroutineUniformLocations> MaxSubroutineUniformLocations;
#  pragma pop_macro("MaxSubroutineUniformLocations")
# else
	Transform<LimitQuery::MaxSubroutineUniformLocations> MaxSubroutineUniformLocations;
# endif
#endif
#if defined GL_MAX_TESS_CONTROL_ATOMIC_COUNTER_BUFFERS
# if defined MaxTessControlAtomicCounterBuffers
#  pragma push_macro("MaxTessControlAtomicCounterBuffers")
#  undef MaxTessControlAtomicCounterBuffers
	Transform<LimitQuery::MaxTessControlAtomicCounterBuffers> MaxTessControlAtomicCounterBuffers;
#  pragma pop_macro("MaxTessControlAtomicCounterBuffers")
# else
	Transform<LimitQuery::MaxTessControlAtomicCounterBuffers> MaxTessControlAtomicCounterBuffers;
# endif
#endif
#if defined GL_MAX_TESS_CONTROL_ATOMIC_COUNTERS
# if defined MaxTessControlAtomicCounters
#  pragma push_macro("MaxTessControlAtomicCounters")
#  undef MaxTessControlAtomicCounters
	Transform<LimitQuery::MaxTessControlAtomicCounters> MaxTessControlAtomicCounters;
#  pragma pop_macro("MaxTessControlAtomicCounters")
# else
	Transform<LimitQuery::MaxTessControlAtomicCounters> MaxTessControlAtomicCounters;
# endif
#endif
#if defined GL_MAX_TESS_CONTROL_IMAGE_UNIFORMS
# if defined MaxTessControlImageUniforms
#  pragma push_macro("MaxTessControlImageUniforms")
#  undef MaxTessControlImageUniforms
	Transform<LimitQuery::MaxTessControlImageUniforms> MaxTessControlImageUniforms;
#  pragma pop_macro("MaxTessControlImageUniforms")
# else
	Transform<LimitQuery::MaxTessControlImageUniforms> MaxTessControlImageUniforms;
# endif
#endif
#if defined GL_MAX_TESS_CONTROL_INPUT_COMPONENTS
# if defined MaxTessControlInputComponents
#  pragma push_macro("MaxTessControlInputComponents")
#  undef MaxTessControlInputComponents
	Transform<LimitQuery::MaxTessControlInputComponents> MaxTessControlInputComponents;
#  pragma pop_macro("MaxTessControlInputComponents")
# else
	Transform<LimitQuery::MaxTessControlInputComponents> MaxTessControlInputComponents;
# endif
#endif
#if defined GL_MAX_TESS_CONTROL_OUTPUT_COMPONENTS
# if defined MaxTessControlOutputComponents
#  pragma push_macro("MaxTessControlOutputComponents")
#  undef MaxTessControlOutputComponents
	Transform<LimitQuery::MaxTessControlOutputComponents> MaxTessControlOutputComponents;
#  pragma pop_macro("MaxTessControlOutputComponents")
# else
	Transform<LimitQuery::MaxTessControlOutputComponents> MaxTessControlOutputComponents;
# endif
#endif
#if defined GL_MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS
# if defined MaxTessControlTextureImageUnits
#  pragma push_macro("MaxTessControlTextureImageUnits")
#  undef MaxTessControlTextureImageUnits
	Transform<LimitQuery::MaxTessControlTextureImageUnits> MaxTessControlTextureImageUnits;
#  pragma pop_macro("MaxTessControlTextureImageUnits")
# else
	Transform<LimitQuery::MaxTessControlTextureImageUnits> MaxTessControlTextureImageUnits;
# endif
#endif
#if defined GL_MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS
# if defined MaxTessControlTotalOutputComponents
#  pragma push_macro("MaxTessControlTotalOutputComponents")
#  undef MaxTessControlTotalOutputComponents
	Transform<LimitQuery::MaxTessControlTotalOutputComponents> MaxTessControlTotalOutputComponents;
#  pragma pop_macro("MaxTessControlTotalOutputComponents")
# else
	Transform<LimitQuery::MaxTessControlTotalOutputComponents> MaxTessControlTotalOutputComponents;
# endif
#endif
#if defined GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS
# if defined MaxTessControlUniformBlocks
#  pragma push_macro("MaxTessControlUniformBlocks")
#  undef MaxTessControlUniformBlocks
	Transform<LimitQuery::MaxTessControlUniformBlocks> MaxTessControlUniformBlocks;
#  pragma pop_macro("MaxTessControlUniformBlocks")
# else
	Transform<LimitQuery::MaxTessControlUniformBlocks> MaxTessControlUniformBlocks;
# endif
#endif
#if defined GL_MAX_TESS_CONTROL_UNIFORM_COMPONENTS
# if defined MaxTessControlUniformComponents
#  pragma push_macro("MaxTessControlUniformComponents")
#  undef MaxTessControlUniformComponents
	Transform<LimitQuery::MaxTessControlUniformComponents> MaxTessControlUniformComponents;
#  pragma pop_macro("MaxTessControlUniformComponents")
# else
	Transform<LimitQuery::MaxTessControlUniformComponents> MaxTessControlUniformComponents;
# endif
#endif
#if defined GL_MAX_TESS_EVALUATION_ATOMIC_COUNTER_BUFFERS
# if defined MaxTessEvaluationAtomicCounterBuffers
#  pragma push_macro("MaxTessEvaluationAtomicCounterBuffers")
#  undef MaxTessEvaluationAtomicCounterBuffers
	Transform<LimitQuery::MaxTessEvaluationAtomicCounterBuffers> MaxTessEvaluationAtomicCounterBuffers;
#  pragma pop_macro("MaxTessEvaluationAtomicCounterBuffers")
# else
	Transform<LimitQuery::MaxTessEvaluationAtomicCounterBuffers> MaxTessEvaluationAtomicCounterBuffers;
# endif
#endif
#if defined GL_MAX_TESS_EVALUATION_ATOMIC_COUNTERS
# if defined MaxTessEvaluationAtomicCounters
#  pragma push_macro("MaxTessEvaluationAtomicCounters")
#  undef MaxTessEvaluationAtomicCounters
	Transform<LimitQuery::MaxTessEvaluationAtomicCounters> MaxTessEvaluationAtomicCounters;
#  pragma pop_macro("MaxTessEvaluationAtomicCounters")
# else
	Transform<LimitQuery::MaxTessEvaluationAtomicCounters> MaxTessEvaluationAtomicCounters;
# endif
#endif
#if defined GL_MAX_TESS_EVALUATION_IMAGE_UNIFORMS
# if defined MaxTessEvaluationImageUniforms
#  pragma push_macro("MaxTessEvaluationImageUniforms")
#  undef MaxTessEvaluationImageUniforms
	Transform<LimitQuery::MaxTessEvaluationImageUniforms> MaxTessEvaluationImageUniforms;
#  pragma pop_macro("MaxTessEvaluationImageUniforms")
# else
	Transform<LimitQuery::MaxTessEvaluationImageUniforms> MaxTessEvaluationImageUniforms;
# endif
#endif
#if defined GL_MAX_TESS_EVALUATION_INPUT_COMPONENTS
# if defined MaxTessEvaluationInputComponents
#  pragma push_macro("MaxTessEvaluationInputComponents")
#  undef MaxTessEvaluationInputComponents
	Transform<LimitQuery::MaxTessEvaluationInputComponents> MaxTessEvaluationInputComponents;
#  pragma pop_macro("MaxTessEvaluationInputComponents")
# else
	Transform<LimitQuery::MaxTessEvaluationInputComponents> MaxTessEvaluationInputComponents;
# endif
#endif
#if defined GL_MAX_TESS_EVALUATION_OUTPUT_COMPONENTS
# if defined MaxTessEvaluationOutputComponents
#  pragma push_macro("MaxTessEvaluationOutputComponents")
#  undef MaxTessEvaluationOutputComponents
	Transform<LimitQuery::MaxTessEvaluationOutputComponents> MaxTessEvaluationOutputComponents;
#  pragma pop_macro("MaxTessEvaluationOutputComponents")
# else
	Transform<LimitQuery::MaxTessEvaluationOutputComponents> MaxTessEvaluationOutputComponents;
# endif
#endif
#if defined GL_MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS
# if defined MaxTessEvaluationTextureImageUnits
#  pragma push_macro("MaxTessEvaluationTextureImageUnits")
#  undef MaxTessEvaluationTextureImageUnits
	Transform<LimitQuery::MaxTessEvaluationTextureImageUnits> MaxTessEvaluationTextureImageUnits;
#  pragma pop_macro("MaxTessEvaluationTextureImageUnits")
# else
	Transform<LimitQuery::MaxTessEvaluationTextureImageUnits> MaxTessEvaluationTextureImageUnits;
# endif
#endif
#if defined GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS
# if defined MaxTessEvaluationUniformBlocks
#  pragma push_macro("MaxTessEvaluationUniformBlocks")
#  undef MaxTessEvaluationUniformBlocks
	Transform<LimitQuery::MaxTessEvaluationUniformBlocks> MaxTessEvaluationUniformBlocks;
#  pragma pop_macro("MaxTessEvaluationUniformBlocks")
# else
	Transform<LimitQuery::MaxTessEvaluationUniformBlocks> MaxTessEvaluationUniformBlocks;
# endif
#endif
#if defined GL_MAX_TESS_EVALUATION_UNIFORM_COMPONENTS
# if defined MaxTessEvaluationUniformComponents
#  pragma push_macro("MaxTessEvaluationUniformComponents")
#  undef MaxTessEvaluationUniformComponents
	Transform<LimitQuery::MaxTessEvaluationUniformComponents> MaxTessEvaluationUniformComponents;
#  pragma pop_macro("MaxTessEvaluationUniformComponents")
# else
	Transform<LimitQuery::MaxTessEvaluationUniformComponents> MaxTessEvaluationUniformComponents;
# endif
#endif
#if defined GL_MAX_TESS_GEN_LEVEL
# if defined MaxTessGenLevel
#  pragma push_macro("MaxTessGenLevel")
#  undef MaxTessGenLevel
	Transform<LimitQuery::MaxTessGenLevel> MaxTessGenLevel;
#  pragma pop_macro("MaxTessGenLevel")
# else
	Transform<LimitQuery::MaxTessGenLevel> MaxTessGenLevel;
# endif
#endif
#if defined GL_MAX_TESS_PATCH_COMPONENTS
# if defined MaxTessPatchComponents
#  pragma push_macro("MaxTessPatchComponents")
#  undef MaxTessPatchComponents
	Transform<LimitQuery::MaxTessPatchComponents> MaxTessPatchComponents;
#  pragma pop_macro("MaxTessPatchComponents")
# else
	Transform<LimitQuery::MaxTessPatchComponents> MaxTessPatchComponents;
# endif
#endif
#if defined GL_MAX_TEXTURE_BUFFER_SIZE
# if defined MaxTextureBufferSize
#  pragma push_macro("MaxTextureBufferSize")
#  undef MaxTextureBufferSize
	Transform<LimitQuery::MaxTextureBufferSize> MaxTextureBufferSize;
#  pragma pop_macro("MaxTextureBufferSize")
# else
	Transform<LimitQuery::MaxTextureBufferSize> MaxTextureBufferSize;
# endif
#endif
#if defined GL_MAX_TEXTURE_IMAGE_UNITS
# if defined MaxTextureImageUnits
#  pragma push_macro("MaxTextureImageUnits")
#  undef MaxTextureImageUnits
	Transform<LimitQuery::MaxTextureImageUnits> MaxTextureImageUnits;
#  pragma pop_macro("MaxTextureImageUnits")
# else
	Transform<LimitQuery::MaxTextureImageUnits> MaxTextureImageUnits;
# endif
#endif
#if defined GL_MAX_TEXTURE_LOD_BIAS
# if defined MaxTextureLodBias
#  pragma push_macro("MaxTextureLodBias")
#  undef MaxTextureLodBias
	Transform<LimitQuery::MaxTextureLodBias> MaxTextureLodBias;
#  pragma pop_macro("MaxTextureLodBias")
# else
	Transform<LimitQuery::MaxTextureLodBias> MaxTextureLodBias;
# endif
#endif
#if defined GL_MAX_TEXTURE_SIZE
# if defined MaxTextureSize
#  pragma push_macro("MaxTextureSize")
#  undef MaxTextureSize
	Transform<LimitQuery::MaxTextureSize> MaxTextureSize;
#  pragma pop_macro("MaxTextureSize")
# else
	Transform<LimitQuery::MaxTextureSize> MaxTextureSize;
# endif
#endif
#if defined GL_MAX_TRANSFORM_FEEDBACK_BUFFERS
# if defined MaxTransformFeedbackBuffers
#  pragma push_macro("MaxTransformFeedbackBuffers")
#  undef MaxTransformFeedbackBuffers
	Transform<LimitQuery::MaxTransformFeedbackBuffers> MaxTransformFeedbackBuffers;
#  pragma pop_macro("MaxTransformFeedbackBuffers")
# else
	Transform<LimitQuery::MaxTransformFeedbackBuffers> MaxTransformFeedbackBuffers;
# endif
#endif
#if defined GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS
# if defined MaxTransformFeedbackInterleavedComponents
#  pragma push_macro("MaxTransformFeedbackInterleavedComponents")
#  undef MaxTransformFeedbackInterleavedComponents
	Transform<LimitQuery::MaxTransformFeedbackInterleavedComponents> MaxTransformFeedbackInterleavedComponents;
#  pragma pop_macro("MaxTransformFeedbackInterleavedComponents")
# else
	Transform<LimitQuery::MaxTransformFeedbackInterleavedComponents> MaxTransformFeedbackInterleavedComponents;
# endif
#endif
#if defined GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS
# if defined MaxTransformFeedbackSeparateAttribs
#  pragma push_macro("MaxTransformFeedbackSeparateAttribs")
#  undef MaxTransformFeedbackSeparateAttribs
	Transform<LimitQuery::MaxTransformFeedbackSeparateAttribs> MaxTransformFeedbackSeparateAttribs;
#  pragma pop_macro("MaxTransformFeedbackSeparateAttribs")
# else
	Transform<LimitQuery::MaxTransformFeedbackSeparateAttribs> MaxTransformFeedbackSeparateAttribs;
# endif
#endif
#if defined GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS
# if defined MaxTransformFeedbackSeparateComponents
#  pragma push_macro("MaxTransformFeedbackSeparateComponents")
#  undef MaxTransformFeedbackSeparateComponents
	Transform<LimitQuery::MaxTransformFeedbackSeparateComponents> MaxTransformFeedbackSeparateComponents;
#  pragma pop_macro("MaxTransformFeedbackSeparateComponents")
# else
	Transform<LimitQuery::MaxTransformFeedbackSeparateComponents> MaxTransformFeedbackSeparateComponents;
# endif
#endif
#if defined GL_MAX_UNIFORM_BLOCK_SIZE
# if defined MaxUniformBlockSize
#  pragma push_macro("MaxUniformBlockSize")
#  undef MaxUniformBlockSize
	Transform<LimitQuery::MaxUniformBlockSize> MaxUniformBlockSize;
#  pragma pop_macro("MaxUniformBlockSize")
# else
	Transform<LimitQuery::MaxUniformBlockSize> MaxUniformBlockSize;
# endif
#endif
#if defined GL_MAX_UNIFORM_BUFFER_BINDINGS
# if defined MaxUniformBufferBindings
#  pragma push_macro("MaxUniformBufferBindings")
#  undef MaxUniformBufferBindings
	Transform<LimitQuery::MaxUniformBufferBindings> MaxUniformBufferBindings;
#  pragma pop_macro("MaxUniformBufferBindings")
# else
	Transform<LimitQuery::MaxUniformBufferBindings> MaxUniformBufferBindings;
# endif
#endif
#if defined GL_MAX_VARYING_COMPONENTS
# if defined MaxVaryingComponents
#  pragma push_macro("MaxVaryingComponents")
#  undef MaxVaryingComponents
	Transform<LimitQuery::MaxVaryingComponents> MaxVaryingComponents;
#  pragma pop_macro("MaxVaryingComponents")
# else
	Transform<LimitQuery::MaxVaryingComponents> MaxVaryingComponents;
# endif
#endif
#if defined GL_MAX_VARYING_VECTORS
# if defined MaxVaryingVectors
#  pragma push_macro("MaxVaryingVectors")
#  undef MaxVaryingVectors
	Transform<LimitQuery::MaxVaryingVectors> MaxVaryingVectors;
#  pragma pop_macro("MaxVaryingVectors")
# else
	Transform<LimitQuery::MaxVaryingVectors> MaxVaryingVectors;
# endif
#endif
#if defined GL_MAX_VERTEX_ATOMIC_COUNTER_BUFFERS
# if defined MaxVertexAtomicCounterBuffers
#  pragma push_macro("MaxVertexAtomicCounterBuffers")
#  undef MaxVertexAtomicCounterBuffers
	Transform<LimitQuery::MaxVertexAtomicCounterBuffers> MaxVertexAtomicCounterBuffers;
#  pragma pop_macro("MaxVertexAtomicCounterBuffers")
# else
	Transform<LimitQuery::MaxVertexAtomicCounterBuffers> MaxVertexAtomicCounterBuffers;
# endif
#endif
#if defined GL_MAX_VERTEX_ATOMIC_COUNTERS
# if defined MaxVertexAtomicCounters
#  pragma push_macro("MaxVertexAtomicCounters")
#  undef MaxVertexAtomicCounters
	Transform<LimitQuery::MaxVertexAtomicCounters> MaxVertexAtomicCounters;
#  pragma pop_macro("MaxVertexAtomicCounters")
# else
	Transform<LimitQuery::MaxVertexAtomicCounters> MaxVertexAtomicCounters;
# endif
#endif
#if defined GL_MAX_VERTEX_ATTRIBS
# if defined MaxVertexAttribs
#  pragma push_macro("MaxVertexAttribs")
#  undef MaxVertexAttribs
	Transform<LimitQuery::MaxVertexAttribs> MaxVertexAttribs;
#  pragma pop_macro("MaxVertexAttribs")
# else
	Transform<LimitQuery::MaxVertexAttribs> MaxVertexAttribs;
# endif
#endif
#if defined GL_MAX_VERTEX_IMAGE_UNIFORMS
# if defined MaxVertexImageUniforms
#  pragma push_macro("MaxVertexImageUniforms")
#  undef MaxVertexImageUniforms
	Transform<LimitQuery::MaxVertexImageUniforms> MaxVertexImageUniforms;
#  pragma pop_macro("MaxVertexImageUniforms")
# else
	Transform<LimitQuery::MaxVertexImageUniforms> MaxVertexImageUniforms;
# endif
#endif
#if defined GL_MAX_VERTEX_OUTPUT_COMPONENTS
# if defined MaxVertexOutputComponents
#  pragma push_macro("MaxVertexOutputComponents")
#  undef MaxVertexOutputComponents
	Transform<LimitQuery::MaxVertexOutputComponents> MaxVertexOutputComponents;
#  pragma pop_macro("MaxVertexOutputComponents")
# else
	Transform<LimitQuery::MaxVertexOutputComponents> MaxVertexOutputComponents;
# endif
#endif
#if defined GL_MAX_VERTEX_STREAMS
# if defined MaxVertexStreams
#  pragma push_macro("MaxVertexStreams")
#  undef MaxVertexStreams
	Transform<LimitQuery::MaxVertexStreams> MaxVertexStreams;
#  pragma pop_macro("MaxVertexStreams")
# else
	Transform<LimitQuery::MaxVertexStreams> MaxVertexStreams;
# endif
#endif
#if defined GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS
# if defined MaxVertexTextureImageUnits
#  pragma push_macro("MaxVertexTextureImageUnits")
#  undef MaxVertexTextureImageUnits
	Transform<LimitQuery::MaxVertexTextureImageUnits> MaxVertexTextureImageUnits;
#  pragma pop_macro("MaxVertexTextureImageUnits")
# else
	Transform<LimitQuery::MaxVertexTextureImageUnits> MaxVertexTextureImageUnits;
# endif
#endif
#if defined GL_MAX_VERTEX_UNIFORM_BLOCKS
# if defined MaxVertexUniformBlocks
#  pragma push_macro("MaxVertexUniformBlocks")
#  undef MaxVertexUniformBlocks
	Transform<LimitQuery::MaxVertexUniformBlocks> MaxVertexUniformBlocks;
#  pragma pop_macro("MaxVertexUniformBlocks")
# else
	Transform<LimitQuery::MaxVertexUniformBlocks> MaxVertexUniformBlocks;
# endif
#endif
#if defined GL_MAX_VERTEX_UNIFORM_COMPONENTS
# if defined MaxVertexUniformComponents
#  pragma push_macro("MaxVertexUniformComponents")
#  undef MaxVertexUniformComponents
	Transform<LimitQuery::MaxVertexUniformComponents> MaxVertexUniformComponents;
#  pragma pop_macro("MaxVertexUniformComponents")
# else
	Transform<LimitQuery::MaxVertexUniformComponents> MaxVertexUniformComponents;
# endif
#endif
#if defined GL_MAX_VERTEX_UNIFORM_VECTORS
# if defined MaxVertexUniformVectors
#  pragma push_macro("MaxVertexUniformVectors")
#  undef MaxVertexUniformVectors
	Transform<LimitQuery::MaxVertexUniformVectors> MaxVertexUniformVectors;
#  pragma pop_macro("MaxVertexUniformVectors")
# else
	Transform<LimitQuery::MaxVertexUniformVectors> MaxVertexUniformVectors;
# endif
#endif
#if defined GL_MAX_VIEWPORT_DIMS
# if defined MaxViewportDims
#  pragma push_macro("MaxViewportDims")
#  undef MaxViewportDims
	Transform<LimitQuery::MaxViewportDims> MaxViewportDims;
#  pragma pop_macro("MaxViewportDims")
# else
	Transform<LimitQuery::MaxViewportDims> MaxViewportDims;
# endif
#endif
#if defined GL_MAX_VIEWPORTS
# if defined MaxViewports
#  pragma push_macro("MaxViewports")
#  undef MaxViewports
	Transform<LimitQuery::MaxViewports> MaxViewports;
#  pragma pop_macro("MaxViewports")
# else
	Transform<LimitQuery::MaxViewports> MaxViewports;
# endif
#endif
#if defined GL_MAX_COMPUTE_WORK_GROUP_COUNT
# if defined MaxComputeWorkGroupCount
#  pragma push_macro("MaxComputeWorkGroupCount")
#  undef MaxComputeWorkGroupCount
	Transform<LimitQuery::MaxComputeWorkGroupCount> MaxComputeWorkGroupCount;
#  pragma pop_macro("MaxComputeWorkGroupCount")
# else
	Transform<LimitQuery::MaxComputeWorkGroupCount> MaxComputeWorkGroupCount;
# endif
#endif
#if defined GL_MAX_COMPUTE_WORK_GROUP_SIZE
# if defined MaxComputeWorkGroupSize
#  pragma push_macro("MaxComputeWorkGroupSize")
#  undef MaxComputeWorkGroupSize
	Transform<LimitQuery::MaxComputeWorkGroupSize> MaxComputeWorkGroupSize;
#  pragma pop_macro("MaxComputeWorkGroupSize")
# else
	Transform<LimitQuery::MaxComputeWorkGroupSize> MaxComputeWorkGroupSize;
# endif
#endif
#if defined GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS
# if defined MaxComputeWorkGroupInvocations
#  pragma push_macro("MaxComputeWorkGroupInvocations")
#  undef MaxComputeWorkGroupInvocations
	Transform<LimitQuery::MaxComputeWorkGroupInvocations> MaxComputeWorkGroupInvocations;
#  pragma pop_macro("MaxComputeWorkGroupInvocations")
# else
	Transform<LimitQuery::MaxComputeWorkGroupInvocations> MaxComputeWorkGroupInvocations;
# endif
#endif
#if defined GL_MAX_COMPUTE_SHARED_MEMORY_SIZE
# if defined MaxComputeSharedMemorySize
#  pragma push_macro("MaxComputeSharedMemorySize")
#  undef MaxComputeSharedMemorySize
	Transform<LimitQuery::MaxComputeSharedMemorySize> MaxComputeSharedMemorySize;
#  pragma pop_macro("MaxComputeSharedMemorySize")
# else
	Transform<LimitQuery::MaxComputeSharedMemorySize> MaxComputeSharedMemorySize;
# endif
#endif
#if defined GL_MIN_FRAGMENT_INTERPOLATION_OFFSET
# if defined MinFragmentInterpolationOffset
#  pragma push_macro("MinFragmentInterpolationOffset")
#  undef MinFragmentInterpolationOffset
	Transform<LimitQuery::MinFragmentInterpolationOffset> MinFragmentInterpolationOffset;
#  pragma pop_macro("MinFragmentInterpolationOffset")
# else
	Transform<LimitQuery::MinFragmentInterpolationOffset> MinFragmentInterpolationOffset;
# endif
#endif
#if defined GL_MIN_MAP_BUFFER_ALIGNMENT
# if defined MinMapBufferAlignment
#  pragma push_macro("MinMapBufferAlignment")
#  undef MinMapBufferAlignment
	Transform<LimitQuery::MinMapBufferAlignment> MinMapBufferAlignment;
#  pragma pop_macro("MinMapBufferAlignment")
# else
	Transform<LimitQuery::MinMapBufferAlignment> MinMapBufferAlignment;
# endif
#endif
#if defined GL_MIN_PROGRAM_TEXEL_OFFSET
# if defined MinProgramTexelOffset
#  pragma push_macro("MinProgramTexelOffset")
#  undef MinProgramTexelOffset
	Transform<LimitQuery::MinProgramTexelOffset> MinProgramTexelOffset;
#  pragma pop_macro("MinProgramTexelOffset")
# else
	Transform<LimitQuery::MinProgramTexelOffset> MinProgramTexelOffset;
# endif
#endif
#if defined GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET
# if defined MinProgramTextureGatherOffset
#  pragma push_macro("MinProgramTextureGatherOffset")
#  undef MinProgramTextureGatherOffset
	Transform<LimitQuery::MinProgramTextureGatherOffset> MinProgramTextureGatherOffset;
#  pragma pop_macro("MinProgramTextureGatherOffset")
# else
	Transform<LimitQuery::MinProgramTextureGatherOffset> MinProgramTextureGatherOffset;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_MAX_3D_TEXTURE_SIZE
# if defined Max3DTextureSize
#  pragma push_macro("Max3DTextureSize")
#  undef Max3DTextureSize
	 , Max3DTextureSize(_base())
#  pragma pop_macro("Max3DTextureSize")
# else
	 , Max3DTextureSize(_base())
# endif
#endif
#if defined GL_MAX_ARRAY_TEXTURE_LAYERS
# if defined MaxArrayTextureLayers
#  pragma push_macro("MaxArrayTextureLayers")
#  undef MaxArrayTextureLayers
	 , MaxArrayTextureLayers(_base())
#  pragma pop_macro("MaxArrayTextureLayers")
# else
	 , MaxArrayTextureLayers(_base())
# endif
#endif
#if defined GL_MAX_ATOMIC_COUNTER_BUFFER_BINDINGS
# if defined MaxAtomicCounterBufferBindings
#  pragma push_macro("MaxAtomicCounterBufferBindings")
#  undef MaxAtomicCounterBufferBindings
	 , MaxAtomicCounterBufferBindings(_base())
#  pragma pop_macro("MaxAtomicCounterBufferBindings")
# else
	 , MaxAtomicCounterBufferBindings(_base())
# endif
#endif
#if defined GL_MAX_ATOMIC_COUNTER_BUFFER_SIZE
# if defined MaxAtomicCounterBufferSize
#  pragma push_macro("MaxAtomicCounterBufferSize")
#  undef MaxAtomicCounterBufferSize
	 , MaxAtomicCounterBufferSize(_base())
#  pragma pop_macro("MaxAtomicCounterBufferSize")
# else
	 , MaxAtomicCounterBufferSize(_base())
# endif
#endif
#if defined GL_MAX_CLIP_DISTANCES
# if defined MaxClipDistances
#  pragma push_macro("MaxClipDistances")
#  undef MaxClipDistances
	 , MaxClipDistances(_base())
#  pragma pop_macro("MaxClipDistances")
# else
	 , MaxClipDistances(_base())
# endif
#endif
#if defined GL_MAX_CULL_DISTANCES
# if defined MaxCullDistances
#  pragma push_macro("MaxCullDistances")
#  undef MaxCullDistances
	 , MaxCullDistances(_base())
#  pragma pop_macro("MaxCullDistances")
# else
	 , MaxCullDistances(_base())
# endif
#endif
#if defined GL_MAX_COMBINED_CLIP_AND_CULL_DISTANCES
# if defined MaxCombinedClipAndCullDistances
#  pragma push_macro("MaxCombinedClipAndCullDistances")
#  undef MaxCombinedClipAndCullDistances
	 , MaxCombinedClipAndCullDistances(_base())
#  pragma pop_macro("MaxCombinedClipAndCullDistances")
# else
	 , MaxCombinedClipAndCullDistances(_base())
# endif
#endif
#if defined GL_MAX_COLOR_ATTACHMENTS
# if defined MaxColorAttachments
#  pragma push_macro("MaxColorAttachments")
#  undef MaxColorAttachments
	 , MaxColorAttachments(_base())
#  pragma pop_macro("MaxColorAttachments")
# else
	 , MaxColorAttachments(_base())
# endif
#endif
#if defined GL_MAX_COLOR_TEXTURE_SAMPLES
# if defined MaxColorTextureSamples
#  pragma push_macro("MaxColorTextureSamples")
#  undef MaxColorTextureSamples
	 , MaxColorTextureSamples(_base())
#  pragma pop_macro("MaxColorTextureSamples")
# else
	 , MaxColorTextureSamples(_base())
# endif
#endif
#if defined GL_MAX_COMBINED_ATOMIC_COUNTER_BUFFERS
# if defined MaxCombinedAtomicCounterBuffers
#  pragma push_macro("MaxCombinedAtomicCounterBuffers")
#  undef MaxCombinedAtomicCounterBuffers
	 , MaxCombinedAtomicCounterBuffers(_base())
#  pragma pop_macro("MaxCombinedAtomicCounterBuffers")
# else
	 , MaxCombinedAtomicCounterBuffers(_base())
# endif
#endif
#if defined GL_MAX_COMBINED_ATOMIC_COUNTERS
# if defined MaxCombinedAtomicCounters
#  pragma push_macro("MaxCombinedAtomicCounters")
#  undef MaxCombinedAtomicCounters
	 , MaxCombinedAtomicCounters(_base())
#  pragma pop_macro("MaxCombinedAtomicCounters")
# else
	 , MaxCombinedAtomicCounters(_base())
# endif
#endif
#if defined GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS
# if defined MaxCombinedFragmentUniformComponents
#  pragma push_macro("MaxCombinedFragmentUniformComponents")
#  undef MaxCombinedFragmentUniformComponents
	 , MaxCombinedFragmentUniformComponents(_base())
#  pragma pop_macro("MaxCombinedFragmentUniformComponents")
# else
	 , MaxCombinedFragmentUniformComponents(_base())
# endif
#endif
#if defined GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS
# if defined MaxCombinedGeometryUniformComponents
#  pragma push_macro("MaxCombinedGeometryUniformComponents")
#  undef MaxCombinedGeometryUniformComponents
	 , MaxCombinedGeometryUniformComponents(_base())
#  pragma pop_macro("MaxCombinedGeometryUniformComponents")
# else
	 , MaxCombinedGeometryUniformComponents(_base())
# endif
#endif
#if defined GL_MAX_COMBINED_IMAGE_UNIFORMS
# if defined MaxCombinedImageUniforms
#  pragma push_macro("MaxCombinedImageUniforms")
#  undef MaxCombinedImageUniforms
	 , MaxCombinedImageUniforms(_base())
#  pragma pop_macro("MaxCombinedImageUniforms")
# else
	 , MaxCombinedImageUniforms(_base())
# endif
#endif
#if defined GL_MAX_COMBINED_IMAGE_UNITS_AND_FRAGMENT_OUTPUTS
# if defined MaxCombinedImageUnitsAndFragmentOutputs
#  pragma push_macro("MaxCombinedImageUnitsAndFragmentOutputs")
#  undef MaxCombinedImageUnitsAndFragmentOutputs
	 , MaxCombinedImageUnitsAndFragmentOutputs(_base())
#  pragma pop_macro("MaxCombinedImageUnitsAndFragmentOutputs")
# else
	 , MaxCombinedImageUnitsAndFragmentOutputs(_base())
# endif
#endif
#if defined GL_MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS
# if defined MaxCombinedTessControlUniformComponents
#  pragma push_macro("MaxCombinedTessControlUniformComponents")
#  undef MaxCombinedTessControlUniformComponents
	 , MaxCombinedTessControlUniformComponents(_base())
#  pragma pop_macro("MaxCombinedTessControlUniformComponents")
# else
	 , MaxCombinedTessControlUniformComponents(_base())
# endif
#endif
#if defined GL_MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS
# if defined MaxCombinedTessEvaluationUniformComponents
#  pragma push_macro("MaxCombinedTessEvaluationUniformComponents")
#  undef MaxCombinedTessEvaluationUniformComponents
	 , MaxCombinedTessEvaluationUniformComponents(_base())
#  pragma pop_macro("MaxCombinedTessEvaluationUniformComponents")
# else
	 , MaxCombinedTessEvaluationUniformComponents(_base())
# endif
#endif
#if defined GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
# if defined MaxCombinedTextureImageUnits
#  pragma push_macro("MaxCombinedTextureImageUnits")
#  undef MaxCombinedTextureImageUnits
	 , MaxCombinedTextureImageUnits(_base())
#  pragma pop_macro("MaxCombinedTextureImageUnits")
# else
	 , MaxCombinedTextureImageUnits(_base())
# endif
#endif
#if defined GL_MAX_COMBINED_UNIFORM_BLOCKS
# if defined MaxCombinedUniformBlocks
#  pragma push_macro("MaxCombinedUniformBlocks")
#  undef MaxCombinedUniformBlocks
	 , MaxCombinedUniformBlocks(_base())
#  pragma pop_macro("MaxCombinedUniformBlocks")
# else
	 , MaxCombinedUniformBlocks(_base())
# endif
#endif
#if defined GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS
# if defined MaxCombinedVertexUniformComponents
#  pragma push_macro("MaxCombinedVertexUniformComponents")
#  undef MaxCombinedVertexUniformComponents
	 , MaxCombinedVertexUniformComponents(_base())
#  pragma pop_macro("MaxCombinedVertexUniformComponents")
# else
	 , MaxCombinedVertexUniformComponents(_base())
# endif
#endif
#if defined GL_MAX_CUBE_MAP_TEXTURE_SIZE
# if defined MaxCubeMapTextureSize
#  pragma push_macro("MaxCubeMapTextureSize")
#  undef MaxCubeMapTextureSize
	 , MaxCubeMapTextureSize(_base())
#  pragma pop_macro("MaxCubeMapTextureSize")
# else
	 , MaxCubeMapTextureSize(_base())
# endif
#endif
#if defined GL_MAX_DEPTH_TEXTURE_SAMPLES
# if defined MaxDepthTextureSamples
#  pragma push_macro("MaxDepthTextureSamples")
#  undef MaxDepthTextureSamples
	 , MaxDepthTextureSamples(_base())
#  pragma pop_macro("MaxDepthTextureSamples")
# else
	 , MaxDepthTextureSamples(_base())
# endif
#endif
#if defined GL_MAX_DRAW_BUFFERS
# if defined MaxDrawBuffers
#  pragma push_macro("MaxDrawBuffers")
#  undef MaxDrawBuffers
	 , MaxDrawBuffers(_base())
#  pragma pop_macro("MaxDrawBuffers")
# else
	 , MaxDrawBuffers(_base())
# endif
#endif
#if defined GL_MAX_DUAL_SOURCE_DRAW_BUFFERS
# if defined MaxDualSourceDrawBuffers
#  pragma push_macro("MaxDualSourceDrawBuffers")
#  undef MaxDualSourceDrawBuffers
	 , MaxDualSourceDrawBuffers(_base())
#  pragma pop_macro("MaxDualSourceDrawBuffers")
# else
	 , MaxDualSourceDrawBuffers(_base())
# endif
#endif
#if defined GL_MAX_ELEMENTS_INDICES
# if defined MaxElementsIndices
#  pragma push_macro("MaxElementsIndices")
#  undef MaxElementsIndices
	 , MaxElementsIndices(_base())
#  pragma pop_macro("MaxElementsIndices")
# else
	 , MaxElementsIndices(_base())
# endif
#endif
#if defined GL_MAX_ELEMENTS_VERTICES
# if defined MaxElementsVertices
#  pragma push_macro("MaxElementsVertices")
#  undef MaxElementsVertices
	 , MaxElementsVertices(_base())
#  pragma pop_macro("MaxElementsVertices")
# else
	 , MaxElementsVertices(_base())
# endif
#endif
#if defined GL_MAX_FRAGMENT_ATOMIC_COUNTER_BUFFERS
# if defined MaxFragmentAtomicCounterBuffers
#  pragma push_macro("MaxFragmentAtomicCounterBuffers")
#  undef MaxFragmentAtomicCounterBuffers
	 , MaxFragmentAtomicCounterBuffers(_base())
#  pragma pop_macro("MaxFragmentAtomicCounterBuffers")
# else
	 , MaxFragmentAtomicCounterBuffers(_base())
# endif
#endif
#if defined GL_MAX_FRAGMENT_ATOMIC_COUNTERS
# if defined MaxFragmentAtomicCounters
#  pragma push_macro("MaxFragmentAtomicCounters")
#  undef MaxFragmentAtomicCounters
	 , MaxFragmentAtomicCounters(_base())
#  pragma pop_macro("MaxFragmentAtomicCounters")
# else
	 , MaxFragmentAtomicCounters(_base())
# endif
#endif
#if defined GL_MAX_FRAGMENT_IMAGE_UNIFORMS
# if defined MaxFragmentImageUniforms
#  pragma push_macro("MaxFragmentImageUniforms")
#  undef MaxFragmentImageUniforms
	 , MaxFragmentImageUniforms(_base())
#  pragma pop_macro("MaxFragmentImageUniforms")
# else
	 , MaxFragmentImageUniforms(_base())
# endif
#endif
#if defined GL_MAX_FRAGMENT_INPUT_COMPONENTS
# if defined MaxFragmentInputComponents
#  pragma push_macro("MaxFragmentInputComponents")
#  undef MaxFragmentInputComponents
	 , MaxFragmentInputComponents(_base())
#  pragma pop_macro("MaxFragmentInputComponents")
# else
	 , MaxFragmentInputComponents(_base())
# endif
#endif
#if defined GL_MAX_FRAGMENT_INTERPOLATION_OFFSET
# if defined MaxFragmentInterpolationOffset
#  pragma push_macro("MaxFragmentInterpolationOffset")
#  undef MaxFragmentInterpolationOffset
	 , MaxFragmentInterpolationOffset(_base())
#  pragma pop_macro("MaxFragmentInterpolationOffset")
# else
	 , MaxFragmentInterpolationOffset(_base())
# endif
#endif
#if defined GL_MAX_FRAGMENT_UNIFORM_BLOCKS
# if defined MaxFragmentUniformBlocks
#  pragma push_macro("MaxFragmentUniformBlocks")
#  undef MaxFragmentUniformBlocks
	 , MaxFragmentUniformBlocks(_base())
#  pragma pop_macro("MaxFragmentUniformBlocks")
# else
	 , MaxFragmentUniformBlocks(_base())
# endif
#endif
#if defined GL_MAX_FRAGMENT_UNIFORM_COMPONENTS
# if defined MaxFragmentUniformComponents
#  pragma push_macro("MaxFragmentUniformComponents")
#  undef MaxFragmentUniformComponents
	 , MaxFragmentUniformComponents(_base())
#  pragma pop_macro("MaxFragmentUniformComponents")
# else
	 , MaxFragmentUniformComponents(_base())
# endif
#endif
#if defined GL_MAX_FRAGMENT_UNIFORM_VECTORS
# if defined MaxFragmentUniformVectors
#  pragma push_macro("MaxFragmentUniformVectors")
#  undef MaxFragmentUniformVectors
	 , MaxFragmentUniformVectors(_base())
#  pragma pop_macro("MaxFragmentUniformVectors")
# else
	 , MaxFragmentUniformVectors(_base())
# endif
#endif
#if defined GL_MAX_GEOMETRY_ATOMIC_COUNTER_BUFFERS
# if defined MaxGeometryAtomicCounterBuffers
#  pragma push_macro("MaxGeometryAtomicCounterBuffers")
#  undef MaxGeometryAtomicCounterBuffers
	 , MaxGeometryAtomicCounterBuffers(_base())
#  pragma pop_macro("MaxGeometryAtomicCounterBuffers")
# else
	 , MaxGeometryAtomicCounterBuffers(_base())
# endif
#endif
#if defined GL_MAX_GEOMETRY_ATOMIC_COUNTERS
# if defined MaxGeometryAtomicCounters
#  pragma push_macro("MaxGeometryAtomicCounters")
#  undef MaxGeometryAtomicCounters
	 , MaxGeometryAtomicCounters(_base())
#  pragma pop_macro("MaxGeometryAtomicCounters")
# else
	 , MaxGeometryAtomicCounters(_base())
# endif
#endif
#if defined GL_MAX_GEOMETRY_IMAGE_UNIFORMS
# if defined MaxGeometryImageUniforms
#  pragma push_macro("MaxGeometryImageUniforms")
#  undef MaxGeometryImageUniforms
	 , MaxGeometryImageUniforms(_base())
#  pragma pop_macro("MaxGeometryImageUniforms")
# else
	 , MaxGeometryImageUniforms(_base())
# endif
#endif
#if defined GL_MAX_GEOMETRY_INPUT_COMPONENTS
# if defined MaxGeometryInputComponents
#  pragma push_macro("MaxGeometryInputComponents")
#  undef MaxGeometryInputComponents
	 , MaxGeometryInputComponents(_base())
#  pragma pop_macro("MaxGeometryInputComponents")
# else
	 , MaxGeometryInputComponents(_base())
# endif
#endif
#if defined GL_MAX_GEOMETRY_OUTPUT_COMPONENTS
# if defined MaxGeometryOutputComponents
#  pragma push_macro("MaxGeometryOutputComponents")
#  undef MaxGeometryOutputComponents
	 , MaxGeometryOutputComponents(_base())
#  pragma pop_macro("MaxGeometryOutputComponents")
# else
	 , MaxGeometryOutputComponents(_base())
# endif
#endif
#if defined GL_MAX_GEOMETRY_OUTPUT_VERTICES
# if defined MaxGeometryOutputVertices
#  pragma push_macro("MaxGeometryOutputVertices")
#  undef MaxGeometryOutputVertices
	 , MaxGeometryOutputVertices(_base())
#  pragma pop_macro("MaxGeometryOutputVertices")
# else
	 , MaxGeometryOutputVertices(_base())
# endif
#endif
#if defined GL_MAX_GEOMETRY_SHADER_INVOCATIONS
# if defined MaxGeometryShaderInvocations
#  pragma push_macro("MaxGeometryShaderInvocations")
#  undef MaxGeometryShaderInvocations
	 , MaxGeometryShaderInvocations(_base())
#  pragma pop_macro("MaxGeometryShaderInvocations")
# else
	 , MaxGeometryShaderInvocations(_base())
# endif
#endif
#if defined GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS
# if defined MaxGeometryTextureImageUnits
#  pragma push_macro("MaxGeometryTextureImageUnits")
#  undef MaxGeometryTextureImageUnits
	 , MaxGeometryTextureImageUnits(_base())
#  pragma pop_macro("MaxGeometryTextureImageUnits")
# else
	 , MaxGeometryTextureImageUnits(_base())
# endif
#endif
#if defined GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS
# if defined MaxGeometryTotalOutputComponents
#  pragma push_macro("MaxGeometryTotalOutputComponents")
#  undef MaxGeometryTotalOutputComponents
	 , MaxGeometryTotalOutputComponents(_base())
#  pragma pop_macro("MaxGeometryTotalOutputComponents")
# else
	 , MaxGeometryTotalOutputComponents(_base())
# endif
#endif
#if defined GL_MAX_GEOMETRY_UNIFORM_BLOCKS
# if defined MaxGeometryUniformBlocks
#  pragma push_macro("MaxGeometryUniformBlocks")
#  undef MaxGeometryUniformBlocks
	 , MaxGeometryUniformBlocks(_base())
#  pragma pop_macro("MaxGeometryUniformBlocks")
# else
	 , MaxGeometryUniformBlocks(_base())
# endif
#endif
#if defined GL_MAX_GEOMETRY_UNIFORM_COMPONENTS
# if defined MaxGeometryUniformComponents
#  pragma push_macro("MaxGeometryUniformComponents")
#  undef MaxGeometryUniformComponents
	 , MaxGeometryUniformComponents(_base())
#  pragma pop_macro("MaxGeometryUniformComponents")
# else
	 , MaxGeometryUniformComponents(_base())
# endif
#endif
#if defined GL_MAX_IMAGE_SAMPLES
# if defined MaxImageSamples
#  pragma push_macro("MaxImageSamples")
#  undef MaxImageSamples
	 , MaxImageSamples(_base())
#  pragma pop_macro("MaxImageSamples")
# else
	 , MaxImageSamples(_base())
# endif
#endif
#if defined GL_MAX_IMAGE_UNITS
# if defined MaxImageUnits
#  pragma push_macro("MaxImageUnits")
#  undef MaxImageUnits
	 , MaxImageUnits(_base())
#  pragma pop_macro("MaxImageUnits")
# else
	 , MaxImageUnits(_base())
# endif
#endif
#if defined GL_MAX_INTEGER_SAMPLES
# if defined MaxIntegerSamples
#  pragma push_macro("MaxIntegerSamples")
#  undef MaxIntegerSamples
	 , MaxIntegerSamples(_base())
#  pragma pop_macro("MaxIntegerSamples")
# else
	 , MaxIntegerSamples(_base())
# endif
#endif
#if defined GL_MAX_PATCH_VERTICES
# if defined MaxPatchVertices
#  pragma push_macro("MaxPatchVertices")
#  undef MaxPatchVertices
	 , MaxPatchVertices(_base())
#  pragma pop_macro("MaxPatchVertices")
# else
	 , MaxPatchVertices(_base())
# endif
#endif
#if defined GL_MAX_PROGRAM_TEXEL_OFFSET
# if defined MaxProgramTexelOffset
#  pragma push_macro("MaxProgramTexelOffset")
#  undef MaxProgramTexelOffset
	 , MaxProgramTexelOffset(_base())
#  pragma pop_macro("MaxProgramTexelOffset")
# else
	 , MaxProgramTexelOffset(_base())
# endif
#endif
#if defined GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET
# if defined MaxProgramTextureGatherOffset
#  pragma push_macro("MaxProgramTextureGatherOffset")
#  undef MaxProgramTextureGatherOffset
	 , MaxProgramTextureGatherOffset(_base())
#  pragma pop_macro("MaxProgramTextureGatherOffset")
# else
	 , MaxProgramTextureGatherOffset(_base())
# endif
#endif
#if defined GL_MAX_RECTANGLE_TEXTURE_SIZE
# if defined MaxRectangleTextureSize
#  pragma push_macro("MaxRectangleTextureSize")
#  undef MaxRectangleTextureSize
	 , MaxRectangleTextureSize(_base())
#  pragma pop_macro("MaxRectangleTextureSize")
# else
	 , MaxRectangleTextureSize(_base())
# endif
#endif
#if defined GL_MAX_RENDERBUFFER_SIZE
# if defined MaxRenderbufferSize
#  pragma push_macro("MaxRenderbufferSize")
#  undef MaxRenderbufferSize
	 , MaxRenderbufferSize(_base())
#  pragma pop_macro("MaxRenderbufferSize")
# else
	 , MaxRenderbufferSize(_base())
# endif
#endif
#if defined GL_MAX_SAMPLE_MASK_WORDS
# if defined MaxSampleMaskWords
#  pragma push_macro("MaxSampleMaskWords")
#  undef MaxSampleMaskWords
	 , MaxSampleMaskWords(_base())
#  pragma pop_macro("MaxSampleMaskWords")
# else
	 , MaxSampleMaskWords(_base())
# endif
#endif
#if defined GL_MAX_SAMPLES
# if defined MaxSamples
#  pragma push_macro("MaxSamples")
#  undef MaxSamples
	 , MaxSamples(_base())
#  pragma pop_macro("MaxSamples")
# else
	 , MaxSamples(_base())
# endif
#endif
#if defined GL_MAX_SERVER_WAIT_TIMEOUT
# if defined MaxServerWaitTimeout
#  pragma push_macro("MaxServerWaitTimeout")
#  undef MaxServerWaitTimeout
	 , MaxServerWaitTimeout(_base())
#  pragma pop_macro("MaxServerWaitTimeout")
# else
	 , MaxServerWaitTimeout(_base())
# endif
#endif
#if defined GL_MAX_SUBROUTINES
# if defined MaxSubroutines
#  pragma push_macro("MaxSubroutines")
#  undef MaxSubroutines
	 , MaxSubroutines(_base())
#  pragma pop_macro("MaxSubroutines")
# else
	 , MaxSubroutines(_base())
# endif
#endif
#if defined GL_MAX_SUBROUTINE_UNIFORM_LOCATIONS
# if defined MaxSubroutineUniformLocations
#  pragma push_macro("MaxSubroutineUniformLocations")
#  undef MaxSubroutineUniformLocations
	 , MaxSubroutineUniformLocations(_base())
#  pragma pop_macro("MaxSubroutineUniformLocations")
# else
	 , MaxSubroutineUniformLocations(_base())
# endif
#endif
#if defined GL_MAX_TESS_CONTROL_ATOMIC_COUNTER_BUFFERS
# if defined MaxTessControlAtomicCounterBuffers
#  pragma push_macro("MaxTessControlAtomicCounterBuffers")
#  undef MaxTessControlAtomicCounterBuffers
	 , MaxTessControlAtomicCounterBuffers(_base())
#  pragma pop_macro("MaxTessControlAtomicCounterBuffers")
# else
	 , MaxTessControlAtomicCounterBuffers(_base())
# endif
#endif
#if defined GL_MAX_TESS_CONTROL_ATOMIC_COUNTERS
# if defined MaxTessControlAtomicCounters
#  pragma push_macro("MaxTessControlAtomicCounters")
#  undef MaxTessControlAtomicCounters
	 , MaxTessControlAtomicCounters(_base())
#  pragma pop_macro("MaxTessControlAtomicCounters")
# else
	 , MaxTessControlAtomicCounters(_base())
# endif
#endif
#if defined GL_MAX_TESS_CONTROL_IMAGE_UNIFORMS
# if defined MaxTessControlImageUniforms
#  pragma push_macro("MaxTessControlImageUniforms")
#  undef MaxTessControlImageUniforms
	 , MaxTessControlImageUniforms(_base())
#  pragma pop_macro("MaxTessControlImageUniforms")
# else
	 , MaxTessControlImageUniforms(_base())
# endif
#endif
#if defined GL_MAX_TESS_CONTROL_INPUT_COMPONENTS
# if defined MaxTessControlInputComponents
#  pragma push_macro("MaxTessControlInputComponents")
#  undef MaxTessControlInputComponents
	 , MaxTessControlInputComponents(_base())
#  pragma pop_macro("MaxTessControlInputComponents")
# else
	 , MaxTessControlInputComponents(_base())
# endif
#endif
#if defined GL_MAX_TESS_CONTROL_OUTPUT_COMPONENTS
# if defined MaxTessControlOutputComponents
#  pragma push_macro("MaxTessControlOutputComponents")
#  undef MaxTessControlOutputComponents
	 , MaxTessControlOutputComponents(_base())
#  pragma pop_macro("MaxTessControlOutputComponents")
# else
	 , MaxTessControlOutputComponents(_base())
# endif
#endif
#if defined GL_MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS
# if defined MaxTessControlTextureImageUnits
#  pragma push_macro("MaxTessControlTextureImageUnits")
#  undef MaxTessControlTextureImageUnits
	 , MaxTessControlTextureImageUnits(_base())
#  pragma pop_macro("MaxTessControlTextureImageUnits")
# else
	 , MaxTessControlTextureImageUnits(_base())
# endif
#endif
#if defined GL_MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS
# if defined MaxTessControlTotalOutputComponents
#  pragma push_macro("MaxTessControlTotalOutputComponents")
#  undef MaxTessControlTotalOutputComponents
	 , MaxTessControlTotalOutputComponents(_base())
#  pragma pop_macro("MaxTessControlTotalOutputComponents")
# else
	 , MaxTessControlTotalOutputComponents(_base())
# endif
#endif
#if defined GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS
# if defined MaxTessControlUniformBlocks
#  pragma push_macro("MaxTessControlUniformBlocks")
#  undef MaxTessControlUniformBlocks
	 , MaxTessControlUniformBlocks(_base())
#  pragma pop_macro("MaxTessControlUniformBlocks")
# else
	 , MaxTessControlUniformBlocks(_base())
# endif
#endif
#if defined GL_MAX_TESS_CONTROL_UNIFORM_COMPONENTS
# if defined MaxTessControlUniformComponents
#  pragma push_macro("MaxTessControlUniformComponents")
#  undef MaxTessControlUniformComponents
	 , MaxTessControlUniformComponents(_base())
#  pragma pop_macro("MaxTessControlUniformComponents")
# else
	 , MaxTessControlUniformComponents(_base())
# endif
#endif
#if defined GL_MAX_TESS_EVALUATION_ATOMIC_COUNTER_BUFFERS
# if defined MaxTessEvaluationAtomicCounterBuffers
#  pragma push_macro("MaxTessEvaluationAtomicCounterBuffers")
#  undef MaxTessEvaluationAtomicCounterBuffers
	 , MaxTessEvaluationAtomicCounterBuffers(_base())
#  pragma pop_macro("MaxTessEvaluationAtomicCounterBuffers")
# else
	 , MaxTessEvaluationAtomicCounterBuffers(_base())
# endif
#endif
#if defined GL_MAX_TESS_EVALUATION_ATOMIC_COUNTERS
# if defined MaxTessEvaluationAtomicCounters
#  pragma push_macro("MaxTessEvaluationAtomicCounters")
#  undef MaxTessEvaluationAtomicCounters
	 , MaxTessEvaluationAtomicCounters(_base())
#  pragma pop_macro("MaxTessEvaluationAtomicCounters")
# else
	 , MaxTessEvaluationAtomicCounters(_base())
# endif
#endif
#if defined GL_MAX_TESS_EVALUATION_IMAGE_UNIFORMS
# if defined MaxTessEvaluationImageUniforms
#  pragma push_macro("MaxTessEvaluationImageUniforms")
#  undef MaxTessEvaluationImageUniforms
	 , MaxTessEvaluationImageUniforms(_base())
#  pragma pop_macro("MaxTessEvaluationImageUniforms")
# else
	 , MaxTessEvaluationImageUniforms(_base())
# endif
#endif
#if defined GL_MAX_TESS_EVALUATION_INPUT_COMPONENTS
# if defined MaxTessEvaluationInputComponents
#  pragma push_macro("MaxTessEvaluationInputComponents")
#  undef MaxTessEvaluationInputComponents
	 , MaxTessEvaluationInputComponents(_base())
#  pragma pop_macro("MaxTessEvaluationInputComponents")
# else
	 , MaxTessEvaluationInputComponents(_base())
# endif
#endif
#if defined GL_MAX_TESS_EVALUATION_OUTPUT_COMPONENTS
# if defined MaxTessEvaluationOutputComponents
#  pragma push_macro("MaxTessEvaluationOutputComponents")
#  undef MaxTessEvaluationOutputComponents
	 , MaxTessEvaluationOutputComponents(_base())
#  pragma pop_macro("MaxTessEvaluationOutputComponents")
# else
	 , MaxTessEvaluationOutputComponents(_base())
# endif
#endif
#if defined GL_MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS
# if defined MaxTessEvaluationTextureImageUnits
#  pragma push_macro("MaxTessEvaluationTextureImageUnits")
#  undef MaxTessEvaluationTextureImageUnits
	 , MaxTessEvaluationTextureImageUnits(_base())
#  pragma pop_macro("MaxTessEvaluationTextureImageUnits")
# else
	 , MaxTessEvaluationTextureImageUnits(_base())
# endif
#endif
#if defined GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS
# if defined MaxTessEvaluationUniformBlocks
#  pragma push_macro("MaxTessEvaluationUniformBlocks")
#  undef MaxTessEvaluationUniformBlocks
	 , MaxTessEvaluationUniformBlocks(_base())
#  pragma pop_macro("MaxTessEvaluationUniformBlocks")
# else
	 , MaxTessEvaluationUniformBlocks(_base())
# endif
#endif
#if defined GL_MAX_TESS_EVALUATION_UNIFORM_COMPONENTS
# if defined MaxTessEvaluationUniformComponents
#  pragma push_macro("MaxTessEvaluationUniformComponents")
#  undef MaxTessEvaluationUniformComponents
	 , MaxTessEvaluationUniformComponents(_base())
#  pragma pop_macro("MaxTessEvaluationUniformComponents")
# else
	 , MaxTessEvaluationUniformComponents(_base())
# endif
#endif
#if defined GL_MAX_TESS_GEN_LEVEL
# if defined MaxTessGenLevel
#  pragma push_macro("MaxTessGenLevel")
#  undef MaxTessGenLevel
	 , MaxTessGenLevel(_base())
#  pragma pop_macro("MaxTessGenLevel")
# else
	 , MaxTessGenLevel(_base())
# endif
#endif
#if defined GL_MAX_TESS_PATCH_COMPONENTS
# if defined MaxTessPatchComponents
#  pragma push_macro("MaxTessPatchComponents")
#  undef MaxTessPatchComponents
	 , MaxTessPatchComponents(_base())
#  pragma pop_macro("MaxTessPatchComponents")
# else
	 , MaxTessPatchComponents(_base())
# endif
#endif
#if defined GL_MAX_TEXTURE_BUFFER_SIZE
# if defined MaxTextureBufferSize
#  pragma push_macro("MaxTextureBufferSize")
#  undef MaxTextureBufferSize
	 , MaxTextureBufferSize(_base())
#  pragma pop_macro("MaxTextureBufferSize")
# else
	 , MaxTextureBufferSize(_base())
# endif
#endif
#if defined GL_MAX_TEXTURE_IMAGE_UNITS
# if defined MaxTextureImageUnits
#  pragma push_macro("MaxTextureImageUnits")
#  undef MaxTextureImageUnits
	 , MaxTextureImageUnits(_base())
#  pragma pop_macro("MaxTextureImageUnits")
# else
	 , MaxTextureImageUnits(_base())
# endif
#endif
#if defined GL_MAX_TEXTURE_LOD_BIAS
# if defined MaxTextureLodBias
#  pragma push_macro("MaxTextureLodBias")
#  undef MaxTextureLodBias
	 , MaxTextureLodBias(_base())
#  pragma pop_macro("MaxTextureLodBias")
# else
	 , MaxTextureLodBias(_base())
# endif
#endif
#if defined GL_MAX_TEXTURE_SIZE
# if defined MaxTextureSize
#  pragma push_macro("MaxTextureSize")
#  undef MaxTextureSize
	 , MaxTextureSize(_base())
#  pragma pop_macro("MaxTextureSize")
# else
	 , MaxTextureSize(_base())
# endif
#endif
#if defined GL_MAX_TRANSFORM_FEEDBACK_BUFFERS
# if defined MaxTransformFeedbackBuffers
#  pragma push_macro("MaxTransformFeedbackBuffers")
#  undef MaxTransformFeedbackBuffers
	 , MaxTransformFeedbackBuffers(_base())
#  pragma pop_macro("MaxTransformFeedbackBuffers")
# else
	 , MaxTransformFeedbackBuffers(_base())
# endif
#endif
#if defined GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS
# if defined MaxTransformFeedbackInterleavedComponents
#  pragma push_macro("MaxTransformFeedbackInterleavedComponents")
#  undef MaxTransformFeedbackInterleavedComponents
	 , MaxTransformFeedbackInterleavedComponents(_base())
#  pragma pop_macro("MaxTransformFeedbackInterleavedComponents")
# else
	 , MaxTransformFeedbackInterleavedComponents(_base())
# endif
#endif
#if defined GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS
# if defined MaxTransformFeedbackSeparateAttribs
#  pragma push_macro("MaxTransformFeedbackSeparateAttribs")
#  undef MaxTransformFeedbackSeparateAttribs
	 , MaxTransformFeedbackSeparateAttribs(_base())
#  pragma pop_macro("MaxTransformFeedbackSeparateAttribs")
# else
	 , MaxTransformFeedbackSeparateAttribs(_base())
# endif
#endif
#if defined GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS
# if defined MaxTransformFeedbackSeparateComponents
#  pragma push_macro("MaxTransformFeedbackSeparateComponents")
#  undef MaxTransformFeedbackSeparateComponents
	 , MaxTransformFeedbackSeparateComponents(_base())
#  pragma pop_macro("MaxTransformFeedbackSeparateComponents")
# else
	 , MaxTransformFeedbackSeparateComponents(_base())
# endif
#endif
#if defined GL_MAX_UNIFORM_BLOCK_SIZE
# if defined MaxUniformBlockSize
#  pragma push_macro("MaxUniformBlockSize")
#  undef MaxUniformBlockSize
	 , MaxUniformBlockSize(_base())
#  pragma pop_macro("MaxUniformBlockSize")
# else
	 , MaxUniformBlockSize(_base())
# endif
#endif
#if defined GL_MAX_UNIFORM_BUFFER_BINDINGS
# if defined MaxUniformBufferBindings
#  pragma push_macro("MaxUniformBufferBindings")
#  undef MaxUniformBufferBindings
	 , MaxUniformBufferBindings(_base())
#  pragma pop_macro("MaxUniformBufferBindings")
# else
	 , MaxUniformBufferBindings(_base())
# endif
#endif
#if defined GL_MAX_VARYING_COMPONENTS
# if defined MaxVaryingComponents
#  pragma push_macro("MaxVaryingComponents")
#  undef MaxVaryingComponents
	 , MaxVaryingComponents(_base())
#  pragma pop_macro("MaxVaryingComponents")
# else
	 , MaxVaryingComponents(_base())
# endif
#endif
#if defined GL_MAX_VARYING_VECTORS
# if defined MaxVaryingVectors
#  pragma push_macro("MaxVaryingVectors")
#  undef MaxVaryingVectors
	 , MaxVaryingVectors(_base())
#  pragma pop_macro("MaxVaryingVectors")
# else
	 , MaxVaryingVectors(_base())
# endif
#endif
#if defined GL_MAX_VERTEX_ATOMIC_COUNTER_BUFFERS
# if defined MaxVertexAtomicCounterBuffers
#  pragma push_macro("MaxVertexAtomicCounterBuffers")
#  undef MaxVertexAtomicCounterBuffers
	 , MaxVertexAtomicCounterBuffers(_base())
#  pragma pop_macro("MaxVertexAtomicCounterBuffers")
# else
	 , MaxVertexAtomicCounterBuffers(_base())
# endif
#endif
#if defined GL_MAX_VERTEX_ATOMIC_COUNTERS
# if defined MaxVertexAtomicCounters
#  pragma push_macro("MaxVertexAtomicCounters")
#  undef MaxVertexAtomicCounters
	 , MaxVertexAtomicCounters(_base())
#  pragma pop_macro("MaxVertexAtomicCounters")
# else
	 , MaxVertexAtomicCounters(_base())
# endif
#endif
#if defined GL_MAX_VERTEX_ATTRIBS
# if defined MaxVertexAttribs
#  pragma push_macro("MaxVertexAttribs")
#  undef MaxVertexAttribs
	 , MaxVertexAttribs(_base())
#  pragma pop_macro("MaxVertexAttribs")
# else
	 , MaxVertexAttribs(_base())
# endif
#endif
#if defined GL_MAX_VERTEX_IMAGE_UNIFORMS
# if defined MaxVertexImageUniforms
#  pragma push_macro("MaxVertexImageUniforms")
#  undef MaxVertexImageUniforms
	 , MaxVertexImageUniforms(_base())
#  pragma pop_macro("MaxVertexImageUniforms")
# else
	 , MaxVertexImageUniforms(_base())
# endif
#endif
#if defined GL_MAX_VERTEX_OUTPUT_COMPONENTS
# if defined MaxVertexOutputComponents
#  pragma push_macro("MaxVertexOutputComponents")
#  undef MaxVertexOutputComponents
	 , MaxVertexOutputComponents(_base())
#  pragma pop_macro("MaxVertexOutputComponents")
# else
	 , MaxVertexOutputComponents(_base())
# endif
#endif
#if defined GL_MAX_VERTEX_STREAMS
# if defined MaxVertexStreams
#  pragma push_macro("MaxVertexStreams")
#  undef MaxVertexStreams
	 , MaxVertexStreams(_base())
#  pragma pop_macro("MaxVertexStreams")
# else
	 , MaxVertexStreams(_base())
# endif
#endif
#if defined GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS
# if defined MaxVertexTextureImageUnits
#  pragma push_macro("MaxVertexTextureImageUnits")
#  undef MaxVertexTextureImageUnits
	 , MaxVertexTextureImageUnits(_base())
#  pragma pop_macro("MaxVertexTextureImageUnits")
# else
	 , MaxVertexTextureImageUnits(_base())
# endif
#endif
#if defined GL_MAX_VERTEX_UNIFORM_BLOCKS
# if defined MaxVertexUniformBlocks
#  pragma push_macro("MaxVertexUniformBlocks")
#  undef MaxVertexUniformBlocks
	 , MaxVertexUniformBlocks(_base())
#  pragma pop_macro("MaxVertexUniformBlocks")
# else
	 , MaxVertexUniformBlocks(_base())
# endif
#endif
#if defined GL_MAX_VERTEX_UNIFORM_COMPONENTS
# if defined MaxVertexUniformComponents
#  pragma push_macro("MaxVertexUniformComponents")
#  undef MaxVertexUniformComponents
	 , MaxVertexUniformComponents(_base())
#  pragma pop_macro("MaxVertexUniformComponents")
# else
	 , MaxVertexUniformComponents(_base())
# endif
#endif
#if defined GL_MAX_VERTEX_UNIFORM_VECTORS
# if defined MaxVertexUniformVectors
#  pragma push_macro("MaxVertexUniformVectors")
#  undef MaxVertexUniformVectors
	 , MaxVertexUniformVectors(_base())
#  pragma pop_macro("MaxVertexUniformVectors")
# else
	 , MaxVertexUniformVectors(_base())
# endif
#endif
#if defined GL_MAX_VIEWPORT_DIMS
# if defined MaxViewportDims
#  pragma push_macro("MaxViewportDims")
#  undef MaxViewportDims
	 , MaxViewportDims(_base())
#  pragma pop_macro("MaxViewportDims")
# else
	 , MaxViewportDims(_base())
# endif
#endif
#if defined GL_MAX_VIEWPORTS
# if defined MaxViewports
#  pragma push_macro("MaxViewports")
#  undef MaxViewports
	 , MaxViewports(_base())
#  pragma pop_macro("MaxViewports")
# else
	 , MaxViewports(_base())
# endif
#endif
#if defined GL_MAX_COMPUTE_WORK_GROUP_COUNT
# if defined MaxComputeWorkGroupCount
#  pragma push_macro("MaxComputeWorkGroupCount")
#  undef MaxComputeWorkGroupCount
	 , MaxComputeWorkGroupCount(_base())
#  pragma pop_macro("MaxComputeWorkGroupCount")
# else
	 , MaxComputeWorkGroupCount(_base())
# endif
#endif
#if defined GL_MAX_COMPUTE_WORK_GROUP_SIZE
# if defined MaxComputeWorkGroupSize
#  pragma push_macro("MaxComputeWorkGroupSize")
#  undef MaxComputeWorkGroupSize
	 , MaxComputeWorkGroupSize(_base())
#  pragma pop_macro("MaxComputeWorkGroupSize")
# else
	 , MaxComputeWorkGroupSize(_base())
# endif
#endif
#if defined GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS
# if defined MaxComputeWorkGroupInvocations
#  pragma push_macro("MaxComputeWorkGroupInvocations")
#  undef MaxComputeWorkGroupInvocations
	 , MaxComputeWorkGroupInvocations(_base())
#  pragma pop_macro("MaxComputeWorkGroupInvocations")
# else
	 , MaxComputeWorkGroupInvocations(_base())
# endif
#endif
#if defined GL_MAX_COMPUTE_SHARED_MEMORY_SIZE
# if defined MaxComputeSharedMemorySize
#  pragma push_macro("MaxComputeSharedMemorySize")
#  undef MaxComputeSharedMemorySize
	 , MaxComputeSharedMemorySize(_base())
#  pragma pop_macro("MaxComputeSharedMemorySize")
# else
	 , MaxComputeSharedMemorySize(_base())
# endif
#endif
#if defined GL_MIN_FRAGMENT_INTERPOLATION_OFFSET
# if defined MinFragmentInterpolationOffset
#  pragma push_macro("MinFragmentInterpolationOffset")
#  undef MinFragmentInterpolationOffset
	 , MinFragmentInterpolationOffset(_base())
#  pragma pop_macro("MinFragmentInterpolationOffset")
# else
	 , MinFragmentInterpolationOffset(_base())
# endif
#endif
#if defined GL_MIN_MAP_BUFFER_ALIGNMENT
# if defined MinMapBufferAlignment
#  pragma push_macro("MinMapBufferAlignment")
#  undef MinMapBufferAlignment
	 , MinMapBufferAlignment(_base())
#  pragma pop_macro("MinMapBufferAlignment")
# else
	 , MinMapBufferAlignment(_base())
# endif
#endif
#if defined GL_MIN_PROGRAM_TEXEL_OFFSET
# if defined MinProgramTexelOffset
#  pragma push_macro("MinProgramTexelOffset")
#  undef MinProgramTexelOffset
	 , MinProgramTexelOffset(_base())
#  pragma pop_macro("MinProgramTexelOffset")
# else
	 , MinProgramTexelOffset(_base())
# endif
#endif
#if defined GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET
# if defined MinProgramTextureGatherOffset
#  pragma push_macro("MinProgramTextureGatherOffset")
#  undef MinProgramTextureGatherOffset
	 , MinProgramTextureGatherOffset(_base())
#  pragma pop_macro("MinProgramTextureGatherOffset")
# else
	 , MinProgramTextureGatherOffset(_base())
# endif
#endif
	{ }
};

} // namespace enums


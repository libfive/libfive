//  File implement/oglplus/enums/program_resource_property_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/program_resource_property.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<ProgramResourceProperty> class Transform>
class EnumToClass<Base, ProgramResourceProperty, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_ARRAY_SIZE
# if defined ArraySize
#  pragma push_macro("ArraySize")
#  undef ArraySize
	Transform<ProgramResourceProperty::ArraySize> ArraySize;
#  pragma pop_macro("ArraySize")
# else
	Transform<ProgramResourceProperty::ArraySize> ArraySize;
# endif
#endif
#if defined GL_OFFSET
# if defined Offset
#  pragma push_macro("Offset")
#  undef Offset
	Transform<ProgramResourceProperty::Offset> Offset;
#  pragma pop_macro("Offset")
# else
	Transform<ProgramResourceProperty::Offset> Offset;
# endif
#endif
#if defined GL_BLOCK_INDEX
# if defined BlockIndex
#  pragma push_macro("BlockIndex")
#  undef BlockIndex
	Transform<ProgramResourceProperty::BlockIndex> BlockIndex;
#  pragma pop_macro("BlockIndex")
# else
	Transform<ProgramResourceProperty::BlockIndex> BlockIndex;
# endif
#endif
#if defined GL_ARRAY_STRIDE
# if defined ArrayStride
#  pragma push_macro("ArrayStride")
#  undef ArrayStride
	Transform<ProgramResourceProperty::ArrayStride> ArrayStride;
#  pragma pop_macro("ArrayStride")
# else
	Transform<ProgramResourceProperty::ArrayStride> ArrayStride;
# endif
#endif
#if defined GL_MATRIX_STRIDE
# if defined MatrixStride
#  pragma push_macro("MatrixStride")
#  undef MatrixStride
	Transform<ProgramResourceProperty::MatrixStride> MatrixStride;
#  pragma pop_macro("MatrixStride")
# else
	Transform<ProgramResourceProperty::MatrixStride> MatrixStride;
# endif
#endif
#if defined GL_IS_ROW_MAJOR
# if defined IsRowMajor
#  pragma push_macro("IsRowMajor")
#  undef IsRowMajor
	Transform<ProgramResourceProperty::IsRowMajor> IsRowMajor;
#  pragma pop_macro("IsRowMajor")
# else
	Transform<ProgramResourceProperty::IsRowMajor> IsRowMajor;
# endif
#endif
#if defined GL_ATOMIC_COUNTER_BUFFER_INDEX
# if defined AtomicCounterBufferIndex
#  pragma push_macro("AtomicCounterBufferIndex")
#  undef AtomicCounterBufferIndex
	Transform<ProgramResourceProperty::AtomicCounterBufferIndex> AtomicCounterBufferIndex;
#  pragma pop_macro("AtomicCounterBufferIndex")
# else
	Transform<ProgramResourceProperty::AtomicCounterBufferIndex> AtomicCounterBufferIndex;
# endif
#endif
#if defined GL_BUFFER_BINDING
# if defined BufferBinding
#  pragma push_macro("BufferBinding")
#  undef BufferBinding
	Transform<ProgramResourceProperty::BufferBinding> BufferBinding;
#  pragma pop_macro("BufferBinding")
# else
	Transform<ProgramResourceProperty::BufferBinding> BufferBinding;
# endif
#endif
#if defined GL_BUFFER_DATA_SIZE
# if defined BufferDataSize
#  pragma push_macro("BufferDataSize")
#  undef BufferDataSize
	Transform<ProgramResourceProperty::BufferDataSize> BufferDataSize;
#  pragma pop_macro("BufferDataSize")
# else
	Transform<ProgramResourceProperty::BufferDataSize> BufferDataSize;
# endif
#endif
#if defined GL_NUM_ACTIVE_VARIABLES
# if defined NumActiveVariables
#  pragma push_macro("NumActiveVariables")
#  undef NumActiveVariables
	Transform<ProgramResourceProperty::NumActiveVariables> NumActiveVariables;
#  pragma pop_macro("NumActiveVariables")
# else
	Transform<ProgramResourceProperty::NumActiveVariables> NumActiveVariables;
# endif
#endif
#if defined GL_ACTIVE_VARIABLES
# if defined ActiveVariables
#  pragma push_macro("ActiveVariables")
#  undef ActiveVariables
	Transform<ProgramResourceProperty::ActiveVariables> ActiveVariables;
#  pragma pop_macro("ActiveVariables")
# else
	Transform<ProgramResourceProperty::ActiveVariables> ActiveVariables;
# endif
#endif
#if defined GL_REFERENCED_BY_VERTEX_SHADER
# if defined ReferencedByVertexShader
#  pragma push_macro("ReferencedByVertexShader")
#  undef ReferencedByVertexShader
	Transform<ProgramResourceProperty::ReferencedByVertexShader> ReferencedByVertexShader;
#  pragma pop_macro("ReferencedByVertexShader")
# else
	Transform<ProgramResourceProperty::ReferencedByVertexShader> ReferencedByVertexShader;
# endif
#endif
#if defined GL_REFERENCED_BY_TESS_CONTROL_SHADER
# if defined ReferencedByTessControlShader
#  pragma push_macro("ReferencedByTessControlShader")
#  undef ReferencedByTessControlShader
	Transform<ProgramResourceProperty::ReferencedByTessControlShader> ReferencedByTessControlShader;
#  pragma pop_macro("ReferencedByTessControlShader")
# else
	Transform<ProgramResourceProperty::ReferencedByTessControlShader> ReferencedByTessControlShader;
# endif
#endif
#if defined GL_REFERENCED_BY_TESS_EVALUATION_SHADER
# if defined ReferencedByTessEvaluationShader
#  pragma push_macro("ReferencedByTessEvaluationShader")
#  undef ReferencedByTessEvaluationShader
	Transform<ProgramResourceProperty::ReferencedByTessEvaluationShader> ReferencedByTessEvaluationShader;
#  pragma pop_macro("ReferencedByTessEvaluationShader")
# else
	Transform<ProgramResourceProperty::ReferencedByTessEvaluationShader> ReferencedByTessEvaluationShader;
# endif
#endif
#if defined GL_REFERENCED_BY_GEOMETRY_SHADER
# if defined ReferencedByGeometryShader
#  pragma push_macro("ReferencedByGeometryShader")
#  undef ReferencedByGeometryShader
	Transform<ProgramResourceProperty::ReferencedByGeometryShader> ReferencedByGeometryShader;
#  pragma pop_macro("ReferencedByGeometryShader")
# else
	Transform<ProgramResourceProperty::ReferencedByGeometryShader> ReferencedByGeometryShader;
# endif
#endif
#if defined GL_REFERENCED_BY_FRAGMENT_SHADER
# if defined ReferencedByFragmentShader
#  pragma push_macro("ReferencedByFragmentShader")
#  undef ReferencedByFragmentShader
	Transform<ProgramResourceProperty::ReferencedByFragmentShader> ReferencedByFragmentShader;
#  pragma pop_macro("ReferencedByFragmentShader")
# else
	Transform<ProgramResourceProperty::ReferencedByFragmentShader> ReferencedByFragmentShader;
# endif
#endif
#if defined GL_REFERENCED_BY_COMPUTE_SHADER
# if defined ReferencedByComputeShader
#  pragma push_macro("ReferencedByComputeShader")
#  undef ReferencedByComputeShader
	Transform<ProgramResourceProperty::ReferencedByComputeShader> ReferencedByComputeShader;
#  pragma pop_macro("ReferencedByComputeShader")
# else
	Transform<ProgramResourceProperty::ReferencedByComputeShader> ReferencedByComputeShader;
# endif
#endif
#if defined GL_NUM_COMPATIBLE_SUBROUTINES
# if defined NumCompatibleSubroutines
#  pragma push_macro("NumCompatibleSubroutines")
#  undef NumCompatibleSubroutines
	Transform<ProgramResourceProperty::NumCompatibleSubroutines> NumCompatibleSubroutines;
#  pragma pop_macro("NumCompatibleSubroutines")
# else
	Transform<ProgramResourceProperty::NumCompatibleSubroutines> NumCompatibleSubroutines;
# endif
#endif
#if defined GL_COMPATIBLE_SUBROUTINES
# if defined CompatibleSubroutines
#  pragma push_macro("CompatibleSubroutines")
#  undef CompatibleSubroutines
	Transform<ProgramResourceProperty::CompatibleSubroutines> CompatibleSubroutines;
#  pragma pop_macro("CompatibleSubroutines")
# else
	Transform<ProgramResourceProperty::CompatibleSubroutines> CompatibleSubroutines;
# endif
#endif
#if defined GL_TOP_LEVEL_ARRAY_SIZE
# if defined TopLevelArraySize
#  pragma push_macro("TopLevelArraySize")
#  undef TopLevelArraySize
	Transform<ProgramResourceProperty::TopLevelArraySize> TopLevelArraySize;
#  pragma pop_macro("TopLevelArraySize")
# else
	Transform<ProgramResourceProperty::TopLevelArraySize> TopLevelArraySize;
# endif
#endif
#if defined GL_TOP_LEVEL_ARRAY_STRIDE
# if defined TopLevelArrayStride
#  pragma push_macro("TopLevelArrayStride")
#  undef TopLevelArrayStride
	Transform<ProgramResourceProperty::TopLevelArrayStride> TopLevelArrayStride;
#  pragma pop_macro("TopLevelArrayStride")
# else
	Transform<ProgramResourceProperty::TopLevelArrayStride> TopLevelArrayStride;
# endif
#endif
#if defined GL_LOCATION
# if defined Location
#  pragma push_macro("Location")
#  undef Location
	Transform<ProgramResourceProperty::Location> Location;
#  pragma pop_macro("Location")
# else
	Transform<ProgramResourceProperty::Location> Location;
# endif
#endif
#if defined GL_LOCATION_INDEX
# if defined LocationIndex
#  pragma push_macro("LocationIndex")
#  undef LocationIndex
	Transform<ProgramResourceProperty::LocationIndex> LocationIndex;
#  pragma pop_macro("LocationIndex")
# else
	Transform<ProgramResourceProperty::LocationIndex> LocationIndex;
# endif
#endif
#if defined GL_LOCATION_COMPONENT
# if defined LocationComponent
#  pragma push_macro("LocationComponent")
#  undef LocationComponent
	Transform<ProgramResourceProperty::LocationComponent> LocationComponent;
#  pragma pop_macro("LocationComponent")
# else
	Transform<ProgramResourceProperty::LocationComponent> LocationComponent;
# endif
#endif
#if defined GL_IS_PER_PATCH
# if defined IsPerPatch
#  pragma push_macro("IsPerPatch")
#  undef IsPerPatch
	Transform<ProgramResourceProperty::IsPerPatch> IsPerPatch;
#  pragma pop_macro("IsPerPatch")
# else
	Transform<ProgramResourceProperty::IsPerPatch> IsPerPatch;
# endif
#endif
#if defined GL_TRANSFORM_FEEDBACK_BUFFER_INDEX
# if defined TransformFeedbackBufferIndex
#  pragma push_macro("TransformFeedbackBufferIndex")
#  undef TransformFeedbackBufferIndex
	Transform<ProgramResourceProperty::TransformFeedbackBufferIndex> TransformFeedbackBufferIndex;
#  pragma pop_macro("TransformFeedbackBufferIndex")
# else
	Transform<ProgramResourceProperty::TransformFeedbackBufferIndex> TransformFeedbackBufferIndex;
# endif
#endif
#if defined GL_TRANSFORM_FEEDBACK_BUFFER_STRIDE
# if defined TransformFeedbackBufferStride
#  pragma push_macro("TransformFeedbackBufferStride")
#  undef TransformFeedbackBufferStride
	Transform<ProgramResourceProperty::TransformFeedbackBufferStride> TransformFeedbackBufferStride;
#  pragma pop_macro("TransformFeedbackBufferStride")
# else
	Transform<ProgramResourceProperty::TransformFeedbackBufferStride> TransformFeedbackBufferStride;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_ARRAY_SIZE
# if defined ArraySize
#  pragma push_macro("ArraySize")
#  undef ArraySize
	 , ArraySize(_base())
#  pragma pop_macro("ArraySize")
# else
	 , ArraySize(_base())
# endif
#endif
#if defined GL_OFFSET
# if defined Offset
#  pragma push_macro("Offset")
#  undef Offset
	 , Offset(_base())
#  pragma pop_macro("Offset")
# else
	 , Offset(_base())
# endif
#endif
#if defined GL_BLOCK_INDEX
# if defined BlockIndex
#  pragma push_macro("BlockIndex")
#  undef BlockIndex
	 , BlockIndex(_base())
#  pragma pop_macro("BlockIndex")
# else
	 , BlockIndex(_base())
# endif
#endif
#if defined GL_ARRAY_STRIDE
# if defined ArrayStride
#  pragma push_macro("ArrayStride")
#  undef ArrayStride
	 , ArrayStride(_base())
#  pragma pop_macro("ArrayStride")
# else
	 , ArrayStride(_base())
# endif
#endif
#if defined GL_MATRIX_STRIDE
# if defined MatrixStride
#  pragma push_macro("MatrixStride")
#  undef MatrixStride
	 , MatrixStride(_base())
#  pragma pop_macro("MatrixStride")
# else
	 , MatrixStride(_base())
# endif
#endif
#if defined GL_IS_ROW_MAJOR
# if defined IsRowMajor
#  pragma push_macro("IsRowMajor")
#  undef IsRowMajor
	 , IsRowMajor(_base())
#  pragma pop_macro("IsRowMajor")
# else
	 , IsRowMajor(_base())
# endif
#endif
#if defined GL_ATOMIC_COUNTER_BUFFER_INDEX
# if defined AtomicCounterBufferIndex
#  pragma push_macro("AtomicCounterBufferIndex")
#  undef AtomicCounterBufferIndex
	 , AtomicCounterBufferIndex(_base())
#  pragma pop_macro("AtomicCounterBufferIndex")
# else
	 , AtomicCounterBufferIndex(_base())
# endif
#endif
#if defined GL_BUFFER_BINDING
# if defined BufferBinding
#  pragma push_macro("BufferBinding")
#  undef BufferBinding
	 , BufferBinding(_base())
#  pragma pop_macro("BufferBinding")
# else
	 , BufferBinding(_base())
# endif
#endif
#if defined GL_BUFFER_DATA_SIZE
# if defined BufferDataSize
#  pragma push_macro("BufferDataSize")
#  undef BufferDataSize
	 , BufferDataSize(_base())
#  pragma pop_macro("BufferDataSize")
# else
	 , BufferDataSize(_base())
# endif
#endif
#if defined GL_NUM_ACTIVE_VARIABLES
# if defined NumActiveVariables
#  pragma push_macro("NumActiveVariables")
#  undef NumActiveVariables
	 , NumActiveVariables(_base())
#  pragma pop_macro("NumActiveVariables")
# else
	 , NumActiveVariables(_base())
# endif
#endif
#if defined GL_ACTIVE_VARIABLES
# if defined ActiveVariables
#  pragma push_macro("ActiveVariables")
#  undef ActiveVariables
	 , ActiveVariables(_base())
#  pragma pop_macro("ActiveVariables")
# else
	 , ActiveVariables(_base())
# endif
#endif
#if defined GL_REFERENCED_BY_VERTEX_SHADER
# if defined ReferencedByVertexShader
#  pragma push_macro("ReferencedByVertexShader")
#  undef ReferencedByVertexShader
	 , ReferencedByVertexShader(_base())
#  pragma pop_macro("ReferencedByVertexShader")
# else
	 , ReferencedByVertexShader(_base())
# endif
#endif
#if defined GL_REFERENCED_BY_TESS_CONTROL_SHADER
# if defined ReferencedByTessControlShader
#  pragma push_macro("ReferencedByTessControlShader")
#  undef ReferencedByTessControlShader
	 , ReferencedByTessControlShader(_base())
#  pragma pop_macro("ReferencedByTessControlShader")
# else
	 , ReferencedByTessControlShader(_base())
# endif
#endif
#if defined GL_REFERENCED_BY_TESS_EVALUATION_SHADER
# if defined ReferencedByTessEvaluationShader
#  pragma push_macro("ReferencedByTessEvaluationShader")
#  undef ReferencedByTessEvaluationShader
	 , ReferencedByTessEvaluationShader(_base())
#  pragma pop_macro("ReferencedByTessEvaluationShader")
# else
	 , ReferencedByTessEvaluationShader(_base())
# endif
#endif
#if defined GL_REFERENCED_BY_GEOMETRY_SHADER
# if defined ReferencedByGeometryShader
#  pragma push_macro("ReferencedByGeometryShader")
#  undef ReferencedByGeometryShader
	 , ReferencedByGeometryShader(_base())
#  pragma pop_macro("ReferencedByGeometryShader")
# else
	 , ReferencedByGeometryShader(_base())
# endif
#endif
#if defined GL_REFERENCED_BY_FRAGMENT_SHADER
# if defined ReferencedByFragmentShader
#  pragma push_macro("ReferencedByFragmentShader")
#  undef ReferencedByFragmentShader
	 , ReferencedByFragmentShader(_base())
#  pragma pop_macro("ReferencedByFragmentShader")
# else
	 , ReferencedByFragmentShader(_base())
# endif
#endif
#if defined GL_REFERENCED_BY_COMPUTE_SHADER
# if defined ReferencedByComputeShader
#  pragma push_macro("ReferencedByComputeShader")
#  undef ReferencedByComputeShader
	 , ReferencedByComputeShader(_base())
#  pragma pop_macro("ReferencedByComputeShader")
# else
	 , ReferencedByComputeShader(_base())
# endif
#endif
#if defined GL_NUM_COMPATIBLE_SUBROUTINES
# if defined NumCompatibleSubroutines
#  pragma push_macro("NumCompatibleSubroutines")
#  undef NumCompatibleSubroutines
	 , NumCompatibleSubroutines(_base())
#  pragma pop_macro("NumCompatibleSubroutines")
# else
	 , NumCompatibleSubroutines(_base())
# endif
#endif
#if defined GL_COMPATIBLE_SUBROUTINES
# if defined CompatibleSubroutines
#  pragma push_macro("CompatibleSubroutines")
#  undef CompatibleSubroutines
	 , CompatibleSubroutines(_base())
#  pragma pop_macro("CompatibleSubroutines")
# else
	 , CompatibleSubroutines(_base())
# endif
#endif
#if defined GL_TOP_LEVEL_ARRAY_SIZE
# if defined TopLevelArraySize
#  pragma push_macro("TopLevelArraySize")
#  undef TopLevelArraySize
	 , TopLevelArraySize(_base())
#  pragma pop_macro("TopLevelArraySize")
# else
	 , TopLevelArraySize(_base())
# endif
#endif
#if defined GL_TOP_LEVEL_ARRAY_STRIDE
# if defined TopLevelArrayStride
#  pragma push_macro("TopLevelArrayStride")
#  undef TopLevelArrayStride
	 , TopLevelArrayStride(_base())
#  pragma pop_macro("TopLevelArrayStride")
# else
	 , TopLevelArrayStride(_base())
# endif
#endif
#if defined GL_LOCATION
# if defined Location
#  pragma push_macro("Location")
#  undef Location
	 , Location(_base())
#  pragma pop_macro("Location")
# else
	 , Location(_base())
# endif
#endif
#if defined GL_LOCATION_INDEX
# if defined LocationIndex
#  pragma push_macro("LocationIndex")
#  undef LocationIndex
	 , LocationIndex(_base())
#  pragma pop_macro("LocationIndex")
# else
	 , LocationIndex(_base())
# endif
#endif
#if defined GL_LOCATION_COMPONENT
# if defined LocationComponent
#  pragma push_macro("LocationComponent")
#  undef LocationComponent
	 , LocationComponent(_base())
#  pragma pop_macro("LocationComponent")
# else
	 , LocationComponent(_base())
# endif
#endif
#if defined GL_IS_PER_PATCH
# if defined IsPerPatch
#  pragma push_macro("IsPerPatch")
#  undef IsPerPatch
	 , IsPerPatch(_base())
#  pragma pop_macro("IsPerPatch")
# else
	 , IsPerPatch(_base())
# endif
#endif
#if defined GL_TRANSFORM_FEEDBACK_BUFFER_INDEX
# if defined TransformFeedbackBufferIndex
#  pragma push_macro("TransformFeedbackBufferIndex")
#  undef TransformFeedbackBufferIndex
	 , TransformFeedbackBufferIndex(_base())
#  pragma pop_macro("TransformFeedbackBufferIndex")
# else
	 , TransformFeedbackBufferIndex(_base())
# endif
#endif
#if defined GL_TRANSFORM_FEEDBACK_BUFFER_STRIDE
# if defined TransformFeedbackBufferStride
#  pragma push_macro("TransformFeedbackBufferStride")
#  undef TransformFeedbackBufferStride
	 , TransformFeedbackBufferStride(_base())
#  pragma pop_macro("TransformFeedbackBufferStride")
# else
	 , TransformFeedbackBufferStride(_base())
# endif
#endif
	{ }
};

} // namespace enums


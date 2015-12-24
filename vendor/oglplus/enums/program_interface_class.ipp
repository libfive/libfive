//  File implement/oglplus/enums/program_interface_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/program_interface.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<ProgramInterface> class Transform>
class EnumToClass<Base, ProgramInterface, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_UNIFORM
# if defined Uniform
#  pragma push_macro("Uniform")
#  undef Uniform
	Transform<ProgramInterface::Uniform> Uniform;
#  pragma pop_macro("Uniform")
# else
	Transform<ProgramInterface::Uniform> Uniform;
# endif
#endif
#if defined GL_UNIFORM_BLOCK
# if defined UniformBlock
#  pragma push_macro("UniformBlock")
#  undef UniformBlock
	Transform<ProgramInterface::UniformBlock> UniformBlock;
#  pragma pop_macro("UniformBlock")
# else
	Transform<ProgramInterface::UniformBlock> UniformBlock;
# endif
#endif
#if defined GL_ATOMIC_COUNTER_BUFFER
# if defined AtomicCounterBuffer
#  pragma push_macro("AtomicCounterBuffer")
#  undef AtomicCounterBuffer
	Transform<ProgramInterface::AtomicCounterBuffer> AtomicCounterBuffer;
#  pragma pop_macro("AtomicCounterBuffer")
# else
	Transform<ProgramInterface::AtomicCounterBuffer> AtomicCounterBuffer;
# endif
#endif
#if defined GL_PROGRAM_INPUT
# if defined ProgramInput
#  pragma push_macro("ProgramInput")
#  undef ProgramInput
	Transform<ProgramInterface::ProgramInput> ProgramInput;
#  pragma pop_macro("ProgramInput")
# else
	Transform<ProgramInterface::ProgramInput> ProgramInput;
# endif
#endif
#if defined GL_PROGRAM_OUTPUT
# if defined ProgramOutput
#  pragma push_macro("ProgramOutput")
#  undef ProgramOutput
	Transform<ProgramInterface::ProgramOutput> ProgramOutput;
#  pragma pop_macro("ProgramOutput")
# else
	Transform<ProgramInterface::ProgramOutput> ProgramOutput;
# endif
#endif
#if defined GL_VERTEX_SUBROUTINE
# if defined VertexSubroutine
#  pragma push_macro("VertexSubroutine")
#  undef VertexSubroutine
	Transform<ProgramInterface::VertexSubroutine> VertexSubroutine;
#  pragma pop_macro("VertexSubroutine")
# else
	Transform<ProgramInterface::VertexSubroutine> VertexSubroutine;
# endif
#endif
#if defined GL_TESS_CONTROL_SUBROUTINE
# if defined TessControlSubroutine
#  pragma push_macro("TessControlSubroutine")
#  undef TessControlSubroutine
	Transform<ProgramInterface::TessControlSubroutine> TessControlSubroutine;
#  pragma pop_macro("TessControlSubroutine")
# else
	Transform<ProgramInterface::TessControlSubroutine> TessControlSubroutine;
# endif
#endif
#if defined GL_TESS_EVALUATION_SUBROUTINE
# if defined TessEvaluationSubroutine
#  pragma push_macro("TessEvaluationSubroutine")
#  undef TessEvaluationSubroutine
	Transform<ProgramInterface::TessEvaluationSubroutine> TessEvaluationSubroutine;
#  pragma pop_macro("TessEvaluationSubroutine")
# else
	Transform<ProgramInterface::TessEvaluationSubroutine> TessEvaluationSubroutine;
# endif
#endif
#if defined GL_GEOMETRY_SUBROUTINE
# if defined GeometrySubroutine
#  pragma push_macro("GeometrySubroutine")
#  undef GeometrySubroutine
	Transform<ProgramInterface::GeometrySubroutine> GeometrySubroutine;
#  pragma pop_macro("GeometrySubroutine")
# else
	Transform<ProgramInterface::GeometrySubroutine> GeometrySubroutine;
# endif
#endif
#if defined GL_FRAGMENT_SUBROUTINE
# if defined FragmentSubroutine
#  pragma push_macro("FragmentSubroutine")
#  undef FragmentSubroutine
	Transform<ProgramInterface::FragmentSubroutine> FragmentSubroutine;
#  pragma pop_macro("FragmentSubroutine")
# else
	Transform<ProgramInterface::FragmentSubroutine> FragmentSubroutine;
# endif
#endif
#if defined GL_COMPUTE_SUBROUTINE
# if defined ComputeSubroutine
#  pragma push_macro("ComputeSubroutine")
#  undef ComputeSubroutine
	Transform<ProgramInterface::ComputeSubroutine> ComputeSubroutine;
#  pragma pop_macro("ComputeSubroutine")
# else
	Transform<ProgramInterface::ComputeSubroutine> ComputeSubroutine;
# endif
#endif
#if defined GL_VERTEX_SUBROUTINE_UNIFORM
# if defined VertexSubroutineUniform
#  pragma push_macro("VertexSubroutineUniform")
#  undef VertexSubroutineUniform
	Transform<ProgramInterface::VertexSubroutineUniform> VertexSubroutineUniform;
#  pragma pop_macro("VertexSubroutineUniform")
# else
	Transform<ProgramInterface::VertexSubroutineUniform> VertexSubroutineUniform;
# endif
#endif
#if defined GL_TESS_CONTROL_SUBROUTINE_UNIFORM
# if defined TessControlSubroutineUniform
#  pragma push_macro("TessControlSubroutineUniform")
#  undef TessControlSubroutineUniform
	Transform<ProgramInterface::TessControlSubroutineUniform> TessControlSubroutineUniform;
#  pragma pop_macro("TessControlSubroutineUniform")
# else
	Transform<ProgramInterface::TessControlSubroutineUniform> TessControlSubroutineUniform;
# endif
#endif
#if defined GL_TESS_EVALUATION_SUBROUTINE_UNIFORM
# if defined TessEvaluationSubroutineUniform
#  pragma push_macro("TessEvaluationSubroutineUniform")
#  undef TessEvaluationSubroutineUniform
	Transform<ProgramInterface::TessEvaluationSubroutineUniform> TessEvaluationSubroutineUniform;
#  pragma pop_macro("TessEvaluationSubroutineUniform")
# else
	Transform<ProgramInterface::TessEvaluationSubroutineUniform> TessEvaluationSubroutineUniform;
# endif
#endif
#if defined GL_GEOMETRY_SUBROUTINE_UNIFORM
# if defined GeometrySubroutineUniform
#  pragma push_macro("GeometrySubroutineUniform")
#  undef GeometrySubroutineUniform
	Transform<ProgramInterface::GeometrySubroutineUniform> GeometrySubroutineUniform;
#  pragma pop_macro("GeometrySubroutineUniform")
# else
	Transform<ProgramInterface::GeometrySubroutineUniform> GeometrySubroutineUniform;
# endif
#endif
#if defined GL_FRAGMENT_SUBROUTINE_UNIFORM
# if defined FragmentSubroutineUniform
#  pragma push_macro("FragmentSubroutineUniform")
#  undef FragmentSubroutineUniform
	Transform<ProgramInterface::FragmentSubroutineUniform> FragmentSubroutineUniform;
#  pragma pop_macro("FragmentSubroutineUniform")
# else
	Transform<ProgramInterface::FragmentSubroutineUniform> FragmentSubroutineUniform;
# endif
#endif
#if defined GL_COMPUTE_SUBROUTINE_UNIFORM
# if defined ComputeSubroutineUniform
#  pragma push_macro("ComputeSubroutineUniform")
#  undef ComputeSubroutineUniform
	Transform<ProgramInterface::ComputeSubroutineUniform> ComputeSubroutineUniform;
#  pragma pop_macro("ComputeSubroutineUniform")
# else
	Transform<ProgramInterface::ComputeSubroutineUniform> ComputeSubroutineUniform;
# endif
#endif
#if defined GL_TRANSFORM_FEEDEBACK_VARYING
# if defined TransformFeedebackVarying
#  pragma push_macro("TransformFeedebackVarying")
#  undef TransformFeedebackVarying
	Transform<ProgramInterface::TransformFeedebackVarying> TransformFeedebackVarying;
#  pragma pop_macro("TransformFeedebackVarying")
# else
	Transform<ProgramInterface::TransformFeedebackVarying> TransformFeedebackVarying;
# endif
#endif
#if defined GL_BUFFER_VARIABLE
# if defined BufferVariable
#  pragma push_macro("BufferVariable")
#  undef BufferVariable
	Transform<ProgramInterface::BufferVariable> BufferVariable;
#  pragma pop_macro("BufferVariable")
# else
	Transform<ProgramInterface::BufferVariable> BufferVariable;
# endif
#endif
#if defined GL_SHADER_STORAGE_BLOCK
# if defined ShaderStorageBlock
#  pragma push_macro("ShaderStorageBlock")
#  undef ShaderStorageBlock
	Transform<ProgramInterface::ShaderStorageBlock> ShaderStorageBlock;
#  pragma pop_macro("ShaderStorageBlock")
# else
	Transform<ProgramInterface::ShaderStorageBlock> ShaderStorageBlock;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_UNIFORM
# if defined Uniform
#  pragma push_macro("Uniform")
#  undef Uniform
	 , Uniform(_base())
#  pragma pop_macro("Uniform")
# else
	 , Uniform(_base())
# endif
#endif
#if defined GL_UNIFORM_BLOCK
# if defined UniformBlock
#  pragma push_macro("UniformBlock")
#  undef UniformBlock
	 , UniformBlock(_base())
#  pragma pop_macro("UniformBlock")
# else
	 , UniformBlock(_base())
# endif
#endif
#if defined GL_ATOMIC_COUNTER_BUFFER
# if defined AtomicCounterBuffer
#  pragma push_macro("AtomicCounterBuffer")
#  undef AtomicCounterBuffer
	 , AtomicCounterBuffer(_base())
#  pragma pop_macro("AtomicCounterBuffer")
# else
	 , AtomicCounterBuffer(_base())
# endif
#endif
#if defined GL_PROGRAM_INPUT
# if defined ProgramInput
#  pragma push_macro("ProgramInput")
#  undef ProgramInput
	 , ProgramInput(_base())
#  pragma pop_macro("ProgramInput")
# else
	 , ProgramInput(_base())
# endif
#endif
#if defined GL_PROGRAM_OUTPUT
# if defined ProgramOutput
#  pragma push_macro("ProgramOutput")
#  undef ProgramOutput
	 , ProgramOutput(_base())
#  pragma pop_macro("ProgramOutput")
# else
	 , ProgramOutput(_base())
# endif
#endif
#if defined GL_VERTEX_SUBROUTINE
# if defined VertexSubroutine
#  pragma push_macro("VertexSubroutine")
#  undef VertexSubroutine
	 , VertexSubroutine(_base())
#  pragma pop_macro("VertexSubroutine")
# else
	 , VertexSubroutine(_base())
# endif
#endif
#if defined GL_TESS_CONTROL_SUBROUTINE
# if defined TessControlSubroutine
#  pragma push_macro("TessControlSubroutine")
#  undef TessControlSubroutine
	 , TessControlSubroutine(_base())
#  pragma pop_macro("TessControlSubroutine")
# else
	 , TessControlSubroutine(_base())
# endif
#endif
#if defined GL_TESS_EVALUATION_SUBROUTINE
# if defined TessEvaluationSubroutine
#  pragma push_macro("TessEvaluationSubroutine")
#  undef TessEvaluationSubroutine
	 , TessEvaluationSubroutine(_base())
#  pragma pop_macro("TessEvaluationSubroutine")
# else
	 , TessEvaluationSubroutine(_base())
# endif
#endif
#if defined GL_GEOMETRY_SUBROUTINE
# if defined GeometrySubroutine
#  pragma push_macro("GeometrySubroutine")
#  undef GeometrySubroutine
	 , GeometrySubroutine(_base())
#  pragma pop_macro("GeometrySubroutine")
# else
	 , GeometrySubroutine(_base())
# endif
#endif
#if defined GL_FRAGMENT_SUBROUTINE
# if defined FragmentSubroutine
#  pragma push_macro("FragmentSubroutine")
#  undef FragmentSubroutine
	 , FragmentSubroutine(_base())
#  pragma pop_macro("FragmentSubroutine")
# else
	 , FragmentSubroutine(_base())
# endif
#endif
#if defined GL_COMPUTE_SUBROUTINE
# if defined ComputeSubroutine
#  pragma push_macro("ComputeSubroutine")
#  undef ComputeSubroutine
	 , ComputeSubroutine(_base())
#  pragma pop_macro("ComputeSubroutine")
# else
	 , ComputeSubroutine(_base())
# endif
#endif
#if defined GL_VERTEX_SUBROUTINE_UNIFORM
# if defined VertexSubroutineUniform
#  pragma push_macro("VertexSubroutineUniform")
#  undef VertexSubroutineUniform
	 , VertexSubroutineUniform(_base())
#  pragma pop_macro("VertexSubroutineUniform")
# else
	 , VertexSubroutineUniform(_base())
# endif
#endif
#if defined GL_TESS_CONTROL_SUBROUTINE_UNIFORM
# if defined TessControlSubroutineUniform
#  pragma push_macro("TessControlSubroutineUniform")
#  undef TessControlSubroutineUniform
	 , TessControlSubroutineUniform(_base())
#  pragma pop_macro("TessControlSubroutineUniform")
# else
	 , TessControlSubroutineUniform(_base())
# endif
#endif
#if defined GL_TESS_EVALUATION_SUBROUTINE_UNIFORM
# if defined TessEvaluationSubroutineUniform
#  pragma push_macro("TessEvaluationSubroutineUniform")
#  undef TessEvaluationSubroutineUniform
	 , TessEvaluationSubroutineUniform(_base())
#  pragma pop_macro("TessEvaluationSubroutineUniform")
# else
	 , TessEvaluationSubroutineUniform(_base())
# endif
#endif
#if defined GL_GEOMETRY_SUBROUTINE_UNIFORM
# if defined GeometrySubroutineUniform
#  pragma push_macro("GeometrySubroutineUniform")
#  undef GeometrySubroutineUniform
	 , GeometrySubroutineUniform(_base())
#  pragma pop_macro("GeometrySubroutineUniform")
# else
	 , GeometrySubroutineUniform(_base())
# endif
#endif
#if defined GL_FRAGMENT_SUBROUTINE_UNIFORM
# if defined FragmentSubroutineUniform
#  pragma push_macro("FragmentSubroutineUniform")
#  undef FragmentSubroutineUniform
	 , FragmentSubroutineUniform(_base())
#  pragma pop_macro("FragmentSubroutineUniform")
# else
	 , FragmentSubroutineUniform(_base())
# endif
#endif
#if defined GL_COMPUTE_SUBROUTINE_UNIFORM
# if defined ComputeSubroutineUniform
#  pragma push_macro("ComputeSubroutineUniform")
#  undef ComputeSubroutineUniform
	 , ComputeSubroutineUniform(_base())
#  pragma pop_macro("ComputeSubroutineUniform")
# else
	 , ComputeSubroutineUniform(_base())
# endif
#endif
#if defined GL_TRANSFORM_FEEDEBACK_VARYING
# if defined TransformFeedebackVarying
#  pragma push_macro("TransformFeedebackVarying")
#  undef TransformFeedebackVarying
	 , TransformFeedebackVarying(_base())
#  pragma pop_macro("TransformFeedebackVarying")
# else
	 , TransformFeedebackVarying(_base())
# endif
#endif
#if defined GL_BUFFER_VARIABLE
# if defined BufferVariable
#  pragma push_macro("BufferVariable")
#  undef BufferVariable
	 , BufferVariable(_base())
#  pragma pop_macro("BufferVariable")
# else
	 , BufferVariable(_base())
# endif
#endif
#if defined GL_SHADER_STORAGE_BLOCK
# if defined ShaderStorageBlock
#  pragma push_macro("ShaderStorageBlock")
#  undef ShaderStorageBlock
	 , ShaderStorageBlock(_base())
#  pragma pop_macro("ShaderStorageBlock")
# else
	 , ShaderStorageBlock(_base())
# endif
#endif
	{ }
};

} // namespace enums


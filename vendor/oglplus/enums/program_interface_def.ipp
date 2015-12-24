//  File implement/oglplus/enums/program_interface_def.ipp
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
#ifdef OGLPLUS_LIST_NEEDS_COMMA
# undef OGLPLUS_LIST_NEEDS_COMMA
#endif

#if defined GL_UNIFORM
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined Uniform
#  pragma push_macro("Uniform")
#  undef Uniform
   OGLPLUS_ENUM_CLASS_VALUE(Uniform, GL_UNIFORM)
#  pragma pop_macro("Uniform")
# else
   OGLPLUS_ENUM_CLASS_VALUE(Uniform, GL_UNIFORM)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_UNIFORM_BLOCK
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined UniformBlock
#  pragma push_macro("UniformBlock")
#  undef UniformBlock
   OGLPLUS_ENUM_CLASS_VALUE(UniformBlock, GL_UNIFORM_BLOCK)
#  pragma pop_macro("UniformBlock")
# else
   OGLPLUS_ENUM_CLASS_VALUE(UniformBlock, GL_UNIFORM_BLOCK)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_ATOMIC_COUNTER_BUFFER
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined AtomicCounterBuffer
#  pragma push_macro("AtomicCounterBuffer")
#  undef AtomicCounterBuffer
   OGLPLUS_ENUM_CLASS_VALUE(AtomicCounterBuffer, GL_ATOMIC_COUNTER_BUFFER)
#  pragma pop_macro("AtomicCounterBuffer")
# else
   OGLPLUS_ENUM_CLASS_VALUE(AtomicCounterBuffer, GL_ATOMIC_COUNTER_BUFFER)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_PROGRAM_INPUT
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined ProgramInput
#  pragma push_macro("ProgramInput")
#  undef ProgramInput
   OGLPLUS_ENUM_CLASS_VALUE(ProgramInput, GL_PROGRAM_INPUT)
#  pragma pop_macro("ProgramInput")
# else
   OGLPLUS_ENUM_CLASS_VALUE(ProgramInput, GL_PROGRAM_INPUT)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_PROGRAM_OUTPUT
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined ProgramOutput
#  pragma push_macro("ProgramOutput")
#  undef ProgramOutput
   OGLPLUS_ENUM_CLASS_VALUE(ProgramOutput, GL_PROGRAM_OUTPUT)
#  pragma pop_macro("ProgramOutput")
# else
   OGLPLUS_ENUM_CLASS_VALUE(ProgramOutput, GL_PROGRAM_OUTPUT)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_VERTEX_SUBROUTINE
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined VertexSubroutine
#  pragma push_macro("VertexSubroutine")
#  undef VertexSubroutine
   OGLPLUS_ENUM_CLASS_VALUE(VertexSubroutine, GL_VERTEX_SUBROUTINE)
#  pragma pop_macro("VertexSubroutine")
# else
   OGLPLUS_ENUM_CLASS_VALUE(VertexSubroutine, GL_VERTEX_SUBROUTINE)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_TESS_CONTROL_SUBROUTINE
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined TessControlSubroutine
#  pragma push_macro("TessControlSubroutine")
#  undef TessControlSubroutine
   OGLPLUS_ENUM_CLASS_VALUE(TessControlSubroutine, GL_TESS_CONTROL_SUBROUTINE)
#  pragma pop_macro("TessControlSubroutine")
# else
   OGLPLUS_ENUM_CLASS_VALUE(TessControlSubroutine, GL_TESS_CONTROL_SUBROUTINE)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_TESS_EVALUATION_SUBROUTINE
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined TessEvaluationSubroutine
#  pragma push_macro("TessEvaluationSubroutine")
#  undef TessEvaluationSubroutine
   OGLPLUS_ENUM_CLASS_VALUE(TessEvaluationSubroutine, GL_TESS_EVALUATION_SUBROUTINE)
#  pragma pop_macro("TessEvaluationSubroutine")
# else
   OGLPLUS_ENUM_CLASS_VALUE(TessEvaluationSubroutine, GL_TESS_EVALUATION_SUBROUTINE)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_GEOMETRY_SUBROUTINE
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined GeometrySubroutine
#  pragma push_macro("GeometrySubroutine")
#  undef GeometrySubroutine
   OGLPLUS_ENUM_CLASS_VALUE(GeometrySubroutine, GL_GEOMETRY_SUBROUTINE)
#  pragma pop_macro("GeometrySubroutine")
# else
   OGLPLUS_ENUM_CLASS_VALUE(GeometrySubroutine, GL_GEOMETRY_SUBROUTINE)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_FRAGMENT_SUBROUTINE
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined FragmentSubroutine
#  pragma push_macro("FragmentSubroutine")
#  undef FragmentSubroutine
   OGLPLUS_ENUM_CLASS_VALUE(FragmentSubroutine, GL_FRAGMENT_SUBROUTINE)
#  pragma pop_macro("FragmentSubroutine")
# else
   OGLPLUS_ENUM_CLASS_VALUE(FragmentSubroutine, GL_FRAGMENT_SUBROUTINE)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_COMPUTE_SUBROUTINE
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined ComputeSubroutine
#  pragma push_macro("ComputeSubroutine")
#  undef ComputeSubroutine
   OGLPLUS_ENUM_CLASS_VALUE(ComputeSubroutine, GL_COMPUTE_SUBROUTINE)
#  pragma pop_macro("ComputeSubroutine")
# else
   OGLPLUS_ENUM_CLASS_VALUE(ComputeSubroutine, GL_COMPUTE_SUBROUTINE)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_VERTEX_SUBROUTINE_UNIFORM
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined VertexSubroutineUniform
#  pragma push_macro("VertexSubroutineUniform")
#  undef VertexSubroutineUniform
   OGLPLUS_ENUM_CLASS_VALUE(VertexSubroutineUniform, GL_VERTEX_SUBROUTINE_UNIFORM)
#  pragma pop_macro("VertexSubroutineUniform")
# else
   OGLPLUS_ENUM_CLASS_VALUE(VertexSubroutineUniform, GL_VERTEX_SUBROUTINE_UNIFORM)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_TESS_CONTROL_SUBROUTINE_UNIFORM
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined TessControlSubroutineUniform
#  pragma push_macro("TessControlSubroutineUniform")
#  undef TessControlSubroutineUniform
   OGLPLUS_ENUM_CLASS_VALUE(TessControlSubroutineUniform, GL_TESS_CONTROL_SUBROUTINE_UNIFORM)
#  pragma pop_macro("TessControlSubroutineUniform")
# else
   OGLPLUS_ENUM_CLASS_VALUE(TessControlSubroutineUniform, GL_TESS_CONTROL_SUBROUTINE_UNIFORM)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_TESS_EVALUATION_SUBROUTINE_UNIFORM
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined TessEvaluationSubroutineUniform
#  pragma push_macro("TessEvaluationSubroutineUniform")
#  undef TessEvaluationSubroutineUniform
   OGLPLUS_ENUM_CLASS_VALUE(TessEvaluationSubroutineUniform, GL_TESS_EVALUATION_SUBROUTINE_UNIFORM)
#  pragma pop_macro("TessEvaluationSubroutineUniform")
# else
   OGLPLUS_ENUM_CLASS_VALUE(TessEvaluationSubroutineUniform, GL_TESS_EVALUATION_SUBROUTINE_UNIFORM)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_GEOMETRY_SUBROUTINE_UNIFORM
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined GeometrySubroutineUniform
#  pragma push_macro("GeometrySubroutineUniform")
#  undef GeometrySubroutineUniform
   OGLPLUS_ENUM_CLASS_VALUE(GeometrySubroutineUniform, GL_GEOMETRY_SUBROUTINE_UNIFORM)
#  pragma pop_macro("GeometrySubroutineUniform")
# else
   OGLPLUS_ENUM_CLASS_VALUE(GeometrySubroutineUniform, GL_GEOMETRY_SUBROUTINE_UNIFORM)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_FRAGMENT_SUBROUTINE_UNIFORM
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined FragmentSubroutineUniform
#  pragma push_macro("FragmentSubroutineUniform")
#  undef FragmentSubroutineUniform
   OGLPLUS_ENUM_CLASS_VALUE(FragmentSubroutineUniform, GL_FRAGMENT_SUBROUTINE_UNIFORM)
#  pragma pop_macro("FragmentSubroutineUniform")
# else
   OGLPLUS_ENUM_CLASS_VALUE(FragmentSubroutineUniform, GL_FRAGMENT_SUBROUTINE_UNIFORM)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_COMPUTE_SUBROUTINE_UNIFORM
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined ComputeSubroutineUniform
#  pragma push_macro("ComputeSubroutineUniform")
#  undef ComputeSubroutineUniform
   OGLPLUS_ENUM_CLASS_VALUE(ComputeSubroutineUniform, GL_COMPUTE_SUBROUTINE_UNIFORM)
#  pragma pop_macro("ComputeSubroutineUniform")
# else
   OGLPLUS_ENUM_CLASS_VALUE(ComputeSubroutineUniform, GL_COMPUTE_SUBROUTINE_UNIFORM)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_TRANSFORM_FEEDEBACK_VARYING
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined TransformFeedebackVarying
#  pragma push_macro("TransformFeedebackVarying")
#  undef TransformFeedebackVarying
   OGLPLUS_ENUM_CLASS_VALUE(TransformFeedebackVarying, GL_TRANSFORM_FEEDEBACK_VARYING)
#  pragma pop_macro("TransformFeedebackVarying")
# else
   OGLPLUS_ENUM_CLASS_VALUE(TransformFeedebackVarying, GL_TRANSFORM_FEEDEBACK_VARYING)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_BUFFER_VARIABLE
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined BufferVariable
#  pragma push_macro("BufferVariable")
#  undef BufferVariable
   OGLPLUS_ENUM_CLASS_VALUE(BufferVariable, GL_BUFFER_VARIABLE)
#  pragma pop_macro("BufferVariable")
# else
   OGLPLUS_ENUM_CLASS_VALUE(BufferVariable, GL_BUFFER_VARIABLE)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#if defined GL_SHADER_STORAGE_BLOCK
# ifdef OGLPLUS_LIST_NEEDS_COMMA
   OGLPLUS_ENUM_CLASS_COMMA
# endif
# if defined ShaderStorageBlock
#  pragma push_macro("ShaderStorageBlock")
#  undef ShaderStorageBlock
   OGLPLUS_ENUM_CLASS_VALUE(ShaderStorageBlock, GL_SHADER_STORAGE_BLOCK)
#  pragma pop_macro("ShaderStorageBlock")
# else
   OGLPLUS_ENUM_CLASS_VALUE(ShaderStorageBlock, GL_SHADER_STORAGE_BLOCK)
# endif
# ifndef OGLPLUS_LIST_NEEDS_COMMA
#  define OGLPLUS_LIST_NEEDS_COMMA 1
# endif
#endif
#ifdef OGLPLUS_LIST_NEEDS_COMMA
# undef OGLPLUS_LIST_NEEDS_COMMA
#endif


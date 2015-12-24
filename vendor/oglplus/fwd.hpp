/**
 *  @file oglplus/fwd.hpp
 *  @brief Forward declarations
 *
 *  @author Matus Chochlik
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#pragma once
#ifndef OGLPLUS_FWD_1107121519_HPP
#define OGLPLUS_FWD_1107121519_HPP

#include <oglplus/config/compiler.hpp>
#include <type_traits>
#include <cstddef>

namespace oglplus {

// Angle
template <typename T>
class Angle;

template <typename T>
T Radians(const Angle<T>&);

template <typename T>
T Degrees(const Angle<T>&);

template <typename T>
T Sin(const Angle<T>&);

template <typename T>
T Cos(const Angle<T>&);

template <typename T>
T Tan(const Angle<T>&);

// Vector
template <typename T, std::size_t N>
class Vector;

template <typename T, std::size_t N>
const T* Data(const Vector<T, N>& a);

template <typename T, std::size_t N>
std::size_t Size(const Vector<T, N>&);

template <typename T, std::size_t N>
T At(const Vector<T, N>& a, std::size_t i);

template <typename T, std::size_t N>
T At(const Vector<T, N>& a, std::size_t i, T fallback);

// Quaternion
template <typename T>
class Quaternion;

// Matrix
template <typename T, std::size_t Rows, std::size_t Cols>
class Matrix;

template <typename T, std::size_t R, std::size_t C>
const T* Data(const Matrix<T, R, C>& matrix);

template <typename T, std::size_t R, std::size_t C>
std::size_t Size(const Matrix<T, R, C>&);

template <typename T, std::size_t R, std::size_t C>
std::size_t Rows(const Matrix<T, R, C>&);

template <typename T, std::size_t R, std::size_t C>
std::size_t Cols(const Matrix<T, R, C>&);

template <typename T, std::size_t R, std::size_t C>
T At(const Matrix<T, R, C>& matrix, std::size_t i, std::size_t j);

// Tags
namespace tag {

struct OGLplus;

struct ObjectName;
struct ObjectOps;
struct Object;
struct ProgVar;

struct Generate;
struct Create;

struct Renderbuffer;
struct Framebuffer;
struct Texture;
struct Buffer;
struct Query;
struct ProgramPipeline;
struct Program;
struct TransformFeedback;
struct Sampler;
struct VertexArray;
struct Shader;
struct PerfMonitorAMD;
struct PathNV;

struct VertexAttrib;
struct Uniform;
struct UniformBlock;
struct Subroutine;
struct SubroutineUniform;
struct FragData;

struct ShaderStorageBlock;

struct NativeTypes;
struct MatrixTypes;

struct DirectInit;
struct EagerInit;
struct LazyInit;

struct Typecheck;
struct NoTypecheck;

struct ExplicitSel;
struct ImplicitSel;
struct DirectState;
struct DirectStateEXT;
struct CurrentBound;
} // namespace tag

template <typename X>
struct Classify;

// Object
template <typename ObjTag>
class ObjectName;

template <typename ObjName>
class Sequence;

template <typename ObjTag>
class ObjGenDelOps;

template <typename OpsTag, typename ObjTag>
struct ObjGenTag;

template <typename ObjTag>
class ObjBindingOps;

template <typename ObjTag>
class ObjCommonOps;

template <typename ObjTag>
class BoundObjOps;

template <typename OpsTag, typename ObjTag>
class ObjZeroOps;

template <typename CommonOps>
class ObjectZero;

template <typename ObjectOps>
class Object;

template <typename ObjTag>
struct ObjectSubtype;

template <typename Target>
struct ObjectTargetTag;

template <typename ObjectT>
class Reference;

template <typename ObjectT>
struct Classify<Reference<ObjectT>>
 : Classify<ObjectT>
{ };

template <typename X>
class Optional;

template <typename Object>
class Array;

template <typename Object>
class Group;

class Shader;

template <typename VarTag>
class ProgVarLoc;

typedef ProgVarLoc<tag::VertexAttrib> VertexAttribLoc;
typedef ProgVarLoc<tag::Uniform> UniformLoc;
typedef ProgVarLoc<tag::UniformBlock> UniformBlockLoc;
typedef ProgVarLoc<tag::Subroutine> SubroutineLoc;
typedef ProgVarLoc<tag::SubroutineUniform> SubroutineUniformLoc;
typedef ProgVarLoc<tag::FragData> FragDataLoc;
typedef ProgVarLoc<tag::ShaderStorageBlock> ShaderStorageBlockLoc;

template <typename VarTag>
class ProgVarLocOps;

template <typename VarTag>
class ProgVarTypeOps;

template <typename ChkTag, typename VarTag>
class ProgVarTypecheck;

template <typename OpsTag, typename VarTag, typename TypTag>
class ProgVarSetters;

template <typename OpsTag, typename T>
class ProgVarCallers;

template <typename VarTag>
class ProgVarCommonOps;

template <typename OpsTag, typename VarTag, typename T>
class ProgVarGetSetOps;

template <typename OpsTag, typename VarTag, typename ChkTag, typename T>
class ProgVar;

template <typename OpsTg, typename VarTg, typename ChkTg, typename T>
struct Classify<ProgVar<OpsTg, VarTg, ChkTg, T>>
{
	typedef ProgVar<OpsTg, VarTg, ChkTg, T> Base;
	typedef tag::ProgVar Tag;
	typedef OpsTg OpsTag;
	typedef VarTg VarTag;
	typedef ChkTg ChkTag;
	typedef T Type;
};

template <typename ProgVar>
class Typechecked;

template <typename ProgVar>
class Lazy;

} // namespace oglplus

#endif // include guard

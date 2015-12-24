//  File implement/oglplus/enums/sl_data_type_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/sl_data_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<SLDataType> class Transform>
class EnumToClass<Base, SLDataType, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_FLOAT
# if defined Float
#  pragma push_macro("Float")
#  undef Float
	Transform<SLDataType::Float> Float;
#  pragma pop_macro("Float")
# else
	Transform<SLDataType::Float> Float;
# endif
#endif
#if defined GL_FLOAT_VEC2
# if defined FloatVec2
#  pragma push_macro("FloatVec2")
#  undef FloatVec2
	Transform<SLDataType::FloatVec2> FloatVec2;
#  pragma pop_macro("FloatVec2")
# else
	Transform<SLDataType::FloatVec2> FloatVec2;
# endif
#endif
#if defined GL_FLOAT_VEC3
# if defined FloatVec3
#  pragma push_macro("FloatVec3")
#  undef FloatVec3
	Transform<SLDataType::FloatVec3> FloatVec3;
#  pragma pop_macro("FloatVec3")
# else
	Transform<SLDataType::FloatVec3> FloatVec3;
# endif
#endif
#if defined GL_FLOAT_VEC4
# if defined FloatVec4
#  pragma push_macro("FloatVec4")
#  undef FloatVec4
	Transform<SLDataType::FloatVec4> FloatVec4;
#  pragma pop_macro("FloatVec4")
# else
	Transform<SLDataType::FloatVec4> FloatVec4;
# endif
#endif
#if defined GL_DOUBLE
# if defined Double
#  pragma push_macro("Double")
#  undef Double
	Transform<SLDataType::Double> Double;
#  pragma pop_macro("Double")
# else
	Transform<SLDataType::Double> Double;
# endif
#endif
#if defined GL_DOUBLE_VEC2
# if defined DoubleVec2
#  pragma push_macro("DoubleVec2")
#  undef DoubleVec2
	Transform<SLDataType::DoubleVec2> DoubleVec2;
#  pragma pop_macro("DoubleVec2")
# else
	Transform<SLDataType::DoubleVec2> DoubleVec2;
# endif
#endif
#if defined GL_DOUBLE_VEC3
# if defined DoubleVec3
#  pragma push_macro("DoubleVec3")
#  undef DoubleVec3
	Transform<SLDataType::DoubleVec3> DoubleVec3;
#  pragma pop_macro("DoubleVec3")
# else
	Transform<SLDataType::DoubleVec3> DoubleVec3;
# endif
#endif
#if defined GL_DOUBLE_VEC4
# if defined DoubleVec4
#  pragma push_macro("DoubleVec4")
#  undef DoubleVec4
	Transform<SLDataType::DoubleVec4> DoubleVec4;
#  pragma pop_macro("DoubleVec4")
# else
	Transform<SLDataType::DoubleVec4> DoubleVec4;
# endif
#endif
#if defined GL_INT
# if defined Int
#  pragma push_macro("Int")
#  undef Int
	Transform<SLDataType::Int> Int;
#  pragma pop_macro("Int")
# else
	Transform<SLDataType::Int> Int;
# endif
#endif
#if defined GL_INT_VEC2
# if defined IntVec2
#  pragma push_macro("IntVec2")
#  undef IntVec2
	Transform<SLDataType::IntVec2> IntVec2;
#  pragma pop_macro("IntVec2")
# else
	Transform<SLDataType::IntVec2> IntVec2;
# endif
#endif
#if defined GL_INT_VEC3
# if defined IntVec3
#  pragma push_macro("IntVec3")
#  undef IntVec3
	Transform<SLDataType::IntVec3> IntVec3;
#  pragma pop_macro("IntVec3")
# else
	Transform<SLDataType::IntVec3> IntVec3;
# endif
#endif
#if defined GL_INT_VEC4
# if defined IntVec4
#  pragma push_macro("IntVec4")
#  undef IntVec4
	Transform<SLDataType::IntVec4> IntVec4;
#  pragma pop_macro("IntVec4")
# else
	Transform<SLDataType::IntVec4> IntVec4;
# endif
#endif
#if defined GL_UNSIGNED_INT
# if defined UnsignedInt
#  pragma push_macro("UnsignedInt")
#  undef UnsignedInt
	Transform<SLDataType::UnsignedInt> UnsignedInt;
#  pragma pop_macro("UnsignedInt")
# else
	Transform<SLDataType::UnsignedInt> UnsignedInt;
# endif
#endif
#if defined GL_UNSIGNED_INT_VEC2
# if defined UnsignedIntVec2
#  pragma push_macro("UnsignedIntVec2")
#  undef UnsignedIntVec2
	Transform<SLDataType::UnsignedIntVec2> UnsignedIntVec2;
#  pragma pop_macro("UnsignedIntVec2")
# else
	Transform<SLDataType::UnsignedIntVec2> UnsignedIntVec2;
# endif
#endif
#if defined GL_UNSIGNED_INT_VEC3
# if defined UnsignedIntVec3
#  pragma push_macro("UnsignedIntVec3")
#  undef UnsignedIntVec3
	Transform<SLDataType::UnsignedIntVec3> UnsignedIntVec3;
#  pragma pop_macro("UnsignedIntVec3")
# else
	Transform<SLDataType::UnsignedIntVec3> UnsignedIntVec3;
# endif
#endif
#if defined GL_UNSIGNED_INT_VEC4
# if defined UnsignedIntVec4
#  pragma push_macro("UnsignedIntVec4")
#  undef UnsignedIntVec4
	Transform<SLDataType::UnsignedIntVec4> UnsignedIntVec4;
#  pragma pop_macro("UnsignedIntVec4")
# else
	Transform<SLDataType::UnsignedIntVec4> UnsignedIntVec4;
# endif
#endif
#if defined GL_BOOL
# if defined Bool
#  pragma push_macro("Bool")
#  undef Bool
	Transform<SLDataType::Bool> Bool;
#  pragma pop_macro("Bool")
# else
	Transform<SLDataType::Bool> Bool;
# endif
#endif
#if defined GL_BOOL_VEC2
# if defined BoolVec2
#  pragma push_macro("BoolVec2")
#  undef BoolVec2
	Transform<SLDataType::BoolVec2> BoolVec2;
#  pragma pop_macro("BoolVec2")
# else
	Transform<SLDataType::BoolVec2> BoolVec2;
# endif
#endif
#if defined GL_BOOL_VEC3
# if defined BoolVec3
#  pragma push_macro("BoolVec3")
#  undef BoolVec3
	Transform<SLDataType::BoolVec3> BoolVec3;
#  pragma pop_macro("BoolVec3")
# else
	Transform<SLDataType::BoolVec3> BoolVec3;
# endif
#endif
#if defined GL_BOOL_VEC4
# if defined BoolVec4
#  pragma push_macro("BoolVec4")
#  undef BoolVec4
	Transform<SLDataType::BoolVec4> BoolVec4;
#  pragma pop_macro("BoolVec4")
# else
	Transform<SLDataType::BoolVec4> BoolVec4;
# endif
#endif
#if defined GL_FLOAT_MAT2
# if defined FloatMat2
#  pragma push_macro("FloatMat2")
#  undef FloatMat2
	Transform<SLDataType::FloatMat2> FloatMat2;
#  pragma pop_macro("FloatMat2")
# else
	Transform<SLDataType::FloatMat2> FloatMat2;
# endif
#endif
#if defined GL_FLOAT_MAT3
# if defined FloatMat3
#  pragma push_macro("FloatMat3")
#  undef FloatMat3
	Transform<SLDataType::FloatMat3> FloatMat3;
#  pragma pop_macro("FloatMat3")
# else
	Transform<SLDataType::FloatMat3> FloatMat3;
# endif
#endif
#if defined GL_FLOAT_MAT4
# if defined FloatMat4
#  pragma push_macro("FloatMat4")
#  undef FloatMat4
	Transform<SLDataType::FloatMat4> FloatMat4;
#  pragma pop_macro("FloatMat4")
# else
	Transform<SLDataType::FloatMat4> FloatMat4;
# endif
#endif
#if defined GL_FLOAT_MAT2x3
# if defined FloatMat2x3
#  pragma push_macro("FloatMat2x3")
#  undef FloatMat2x3
	Transform<SLDataType::FloatMat2x3> FloatMat2x3;
#  pragma pop_macro("FloatMat2x3")
# else
	Transform<SLDataType::FloatMat2x3> FloatMat2x3;
# endif
#endif
#if defined GL_FLOAT_MAT2x4
# if defined FloatMat2x4
#  pragma push_macro("FloatMat2x4")
#  undef FloatMat2x4
	Transform<SLDataType::FloatMat2x4> FloatMat2x4;
#  pragma pop_macro("FloatMat2x4")
# else
	Transform<SLDataType::FloatMat2x4> FloatMat2x4;
# endif
#endif
#if defined GL_FLOAT_MAT3x2
# if defined FloatMat3x2
#  pragma push_macro("FloatMat3x2")
#  undef FloatMat3x2
	Transform<SLDataType::FloatMat3x2> FloatMat3x2;
#  pragma pop_macro("FloatMat3x2")
# else
	Transform<SLDataType::FloatMat3x2> FloatMat3x2;
# endif
#endif
#if defined GL_FLOAT_MAT3x4
# if defined FloatMat3x4
#  pragma push_macro("FloatMat3x4")
#  undef FloatMat3x4
	Transform<SLDataType::FloatMat3x4> FloatMat3x4;
#  pragma pop_macro("FloatMat3x4")
# else
	Transform<SLDataType::FloatMat3x4> FloatMat3x4;
# endif
#endif
#if defined GL_FLOAT_MAT4x2
# if defined FloatMat4x2
#  pragma push_macro("FloatMat4x2")
#  undef FloatMat4x2
	Transform<SLDataType::FloatMat4x2> FloatMat4x2;
#  pragma pop_macro("FloatMat4x2")
# else
	Transform<SLDataType::FloatMat4x2> FloatMat4x2;
# endif
#endif
#if defined GL_FLOAT_MAT4x3
# if defined FloatMat4x3
#  pragma push_macro("FloatMat4x3")
#  undef FloatMat4x3
	Transform<SLDataType::FloatMat4x3> FloatMat4x3;
#  pragma pop_macro("FloatMat4x3")
# else
	Transform<SLDataType::FloatMat4x3> FloatMat4x3;
# endif
#endif
#if defined GL_DOUBLE_MAT2
# if defined DoubleMat2
#  pragma push_macro("DoubleMat2")
#  undef DoubleMat2
	Transform<SLDataType::DoubleMat2> DoubleMat2;
#  pragma pop_macro("DoubleMat2")
# else
	Transform<SLDataType::DoubleMat2> DoubleMat2;
# endif
#endif
#if defined GL_DOUBLE_MAT3
# if defined DoubleMat3
#  pragma push_macro("DoubleMat3")
#  undef DoubleMat3
	Transform<SLDataType::DoubleMat3> DoubleMat3;
#  pragma pop_macro("DoubleMat3")
# else
	Transform<SLDataType::DoubleMat3> DoubleMat3;
# endif
#endif
#if defined GL_DOUBLE_MAT4
# if defined DoubleMat4
#  pragma push_macro("DoubleMat4")
#  undef DoubleMat4
	Transform<SLDataType::DoubleMat4> DoubleMat4;
#  pragma pop_macro("DoubleMat4")
# else
	Transform<SLDataType::DoubleMat4> DoubleMat4;
# endif
#endif
#if defined GL_DOUBLE_MAT2x3
# if defined DoubleMat2x3
#  pragma push_macro("DoubleMat2x3")
#  undef DoubleMat2x3
	Transform<SLDataType::DoubleMat2x3> DoubleMat2x3;
#  pragma pop_macro("DoubleMat2x3")
# else
	Transform<SLDataType::DoubleMat2x3> DoubleMat2x3;
# endif
#endif
#if defined GL_DOUBLE_MAT2x4
# if defined DoubleMat2x4
#  pragma push_macro("DoubleMat2x4")
#  undef DoubleMat2x4
	Transform<SLDataType::DoubleMat2x4> DoubleMat2x4;
#  pragma pop_macro("DoubleMat2x4")
# else
	Transform<SLDataType::DoubleMat2x4> DoubleMat2x4;
# endif
#endif
#if defined GL_DOUBLE_MAT3x2
# if defined DoubleMat3x2
#  pragma push_macro("DoubleMat3x2")
#  undef DoubleMat3x2
	Transform<SLDataType::DoubleMat3x2> DoubleMat3x2;
#  pragma pop_macro("DoubleMat3x2")
# else
	Transform<SLDataType::DoubleMat3x2> DoubleMat3x2;
# endif
#endif
#if defined GL_DOUBLE_MAT3x4
# if defined DoubleMat3x4
#  pragma push_macro("DoubleMat3x4")
#  undef DoubleMat3x4
	Transform<SLDataType::DoubleMat3x4> DoubleMat3x4;
#  pragma pop_macro("DoubleMat3x4")
# else
	Transform<SLDataType::DoubleMat3x4> DoubleMat3x4;
# endif
#endif
#if defined GL_DOUBLE_MAT4x2
# if defined DoubleMat4x2
#  pragma push_macro("DoubleMat4x2")
#  undef DoubleMat4x2
	Transform<SLDataType::DoubleMat4x2> DoubleMat4x2;
#  pragma pop_macro("DoubleMat4x2")
# else
	Transform<SLDataType::DoubleMat4x2> DoubleMat4x2;
# endif
#endif
#if defined GL_DOUBLE_MAT4x3
# if defined DoubleMat4x3
#  pragma push_macro("DoubleMat4x3")
#  undef DoubleMat4x3
	Transform<SLDataType::DoubleMat4x3> DoubleMat4x3;
#  pragma pop_macro("DoubleMat4x3")
# else
	Transform<SLDataType::DoubleMat4x3> DoubleMat4x3;
# endif
#endif
#if defined GL_SAMPLER_1D
# if defined Sampler1D
#  pragma push_macro("Sampler1D")
#  undef Sampler1D
	Transform<SLDataType::Sampler1D> Sampler1D;
#  pragma pop_macro("Sampler1D")
# else
	Transform<SLDataType::Sampler1D> Sampler1D;
# endif
#endif
#if defined GL_SAMPLER_2D
# if defined Sampler2D
#  pragma push_macro("Sampler2D")
#  undef Sampler2D
	Transform<SLDataType::Sampler2D> Sampler2D;
#  pragma pop_macro("Sampler2D")
# else
	Transform<SLDataType::Sampler2D> Sampler2D;
# endif
#endif
#if defined GL_SAMPLER_3D
# if defined Sampler3D
#  pragma push_macro("Sampler3D")
#  undef Sampler3D
	Transform<SLDataType::Sampler3D> Sampler3D;
#  pragma pop_macro("Sampler3D")
# else
	Transform<SLDataType::Sampler3D> Sampler3D;
# endif
#endif
#if defined GL_SAMPLER_CUBE
# if defined SamplerCube
#  pragma push_macro("SamplerCube")
#  undef SamplerCube
	Transform<SLDataType::SamplerCube> SamplerCube;
#  pragma pop_macro("SamplerCube")
# else
	Transform<SLDataType::SamplerCube> SamplerCube;
# endif
#endif
#if defined GL_SAMPLER_1D_SHADOW
# if defined Sampler1DShadow
#  pragma push_macro("Sampler1DShadow")
#  undef Sampler1DShadow
	Transform<SLDataType::Sampler1DShadow> Sampler1DShadow;
#  pragma pop_macro("Sampler1DShadow")
# else
	Transform<SLDataType::Sampler1DShadow> Sampler1DShadow;
# endif
#endif
#if defined GL_SAMPLER_2D_SHADOW
# if defined Sampler2DShadow
#  pragma push_macro("Sampler2DShadow")
#  undef Sampler2DShadow
	Transform<SLDataType::Sampler2DShadow> Sampler2DShadow;
#  pragma pop_macro("Sampler2DShadow")
# else
	Transform<SLDataType::Sampler2DShadow> Sampler2DShadow;
# endif
#endif
#if defined GL_SAMPLER_1D_ARRAY
# if defined Sampler1DArray
#  pragma push_macro("Sampler1DArray")
#  undef Sampler1DArray
	Transform<SLDataType::Sampler1DArray> Sampler1DArray;
#  pragma pop_macro("Sampler1DArray")
# else
	Transform<SLDataType::Sampler1DArray> Sampler1DArray;
# endif
#endif
#if defined GL_SAMPLER_2D_ARRAY
# if defined Sampler2DArray
#  pragma push_macro("Sampler2DArray")
#  undef Sampler2DArray
	Transform<SLDataType::Sampler2DArray> Sampler2DArray;
#  pragma pop_macro("Sampler2DArray")
# else
	Transform<SLDataType::Sampler2DArray> Sampler2DArray;
# endif
#endif
#if defined GL_SAMPLER_CUBE_MAP_ARRAY
# if defined SamplerCubeMapArray
#  pragma push_macro("SamplerCubeMapArray")
#  undef SamplerCubeMapArray
	Transform<SLDataType::SamplerCubeMapArray> SamplerCubeMapArray;
#  pragma pop_macro("SamplerCubeMapArray")
# else
	Transform<SLDataType::SamplerCubeMapArray> SamplerCubeMapArray;
# endif
#endif
#if defined GL_SAMPLER_1D_ARRAY_SHADOW
# if defined Sampler1DArrayShadow
#  pragma push_macro("Sampler1DArrayShadow")
#  undef Sampler1DArrayShadow
	Transform<SLDataType::Sampler1DArrayShadow> Sampler1DArrayShadow;
#  pragma pop_macro("Sampler1DArrayShadow")
# else
	Transform<SLDataType::Sampler1DArrayShadow> Sampler1DArrayShadow;
# endif
#endif
#if defined GL_SAMPLER_2D_ARRAY_SHADOW
# if defined Sampler2DArrayShadow
#  pragma push_macro("Sampler2DArrayShadow")
#  undef Sampler2DArrayShadow
	Transform<SLDataType::Sampler2DArrayShadow> Sampler2DArrayShadow;
#  pragma pop_macro("Sampler2DArrayShadow")
# else
	Transform<SLDataType::Sampler2DArrayShadow> Sampler2DArrayShadow;
# endif
#endif
#if defined GL_SAMPLER_2D_MULTISAMPLE
# if defined Sampler2DMultisample
#  pragma push_macro("Sampler2DMultisample")
#  undef Sampler2DMultisample
	Transform<SLDataType::Sampler2DMultisample> Sampler2DMultisample;
#  pragma pop_macro("Sampler2DMultisample")
# else
	Transform<SLDataType::Sampler2DMultisample> Sampler2DMultisample;
# endif
#endif
#if defined GL_SAMPLER_2D_MULTISAMPLE_ARRAY
# if defined Sampler2DMultisampleArray
#  pragma push_macro("Sampler2DMultisampleArray")
#  undef Sampler2DMultisampleArray
	Transform<SLDataType::Sampler2DMultisampleArray> Sampler2DMultisampleArray;
#  pragma pop_macro("Sampler2DMultisampleArray")
# else
	Transform<SLDataType::Sampler2DMultisampleArray> Sampler2DMultisampleArray;
# endif
#endif
#if defined GL_SAMPLER_CUBE_SHADOW
# if defined SamplerCubeShadow
#  pragma push_macro("SamplerCubeShadow")
#  undef SamplerCubeShadow
	Transform<SLDataType::SamplerCubeShadow> SamplerCubeShadow;
#  pragma pop_macro("SamplerCubeShadow")
# else
	Transform<SLDataType::SamplerCubeShadow> SamplerCubeShadow;
# endif
#endif
#if defined GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW
# if defined SamplerCubeMapArrayShadow
#  pragma push_macro("SamplerCubeMapArrayShadow")
#  undef SamplerCubeMapArrayShadow
	Transform<SLDataType::SamplerCubeMapArrayShadow> SamplerCubeMapArrayShadow;
#  pragma pop_macro("SamplerCubeMapArrayShadow")
# else
	Transform<SLDataType::SamplerCubeMapArrayShadow> SamplerCubeMapArrayShadow;
# endif
#endif
#if defined GL_SAMPLER_BUFFER
# if defined SamplerBuffer
#  pragma push_macro("SamplerBuffer")
#  undef SamplerBuffer
	Transform<SLDataType::SamplerBuffer> SamplerBuffer;
#  pragma pop_macro("SamplerBuffer")
# else
	Transform<SLDataType::SamplerBuffer> SamplerBuffer;
# endif
#endif
#if defined GL_SAMPLER_2D_RECT
# if defined Sampler2DRect
#  pragma push_macro("Sampler2DRect")
#  undef Sampler2DRect
	Transform<SLDataType::Sampler2DRect> Sampler2DRect;
#  pragma pop_macro("Sampler2DRect")
# else
	Transform<SLDataType::Sampler2DRect> Sampler2DRect;
# endif
#endif
#if defined GL_SAMPLER_2D_RECT_SHADOW
# if defined Sampler2DRectShadow
#  pragma push_macro("Sampler2DRectShadow")
#  undef Sampler2DRectShadow
	Transform<SLDataType::Sampler2DRectShadow> Sampler2DRectShadow;
#  pragma pop_macro("Sampler2DRectShadow")
# else
	Transform<SLDataType::Sampler2DRectShadow> Sampler2DRectShadow;
# endif
#endif
#if defined GL_INT_SAMPLER_1D
# if defined IntSampler1D
#  pragma push_macro("IntSampler1D")
#  undef IntSampler1D
	Transform<SLDataType::IntSampler1D> IntSampler1D;
#  pragma pop_macro("IntSampler1D")
# else
	Transform<SLDataType::IntSampler1D> IntSampler1D;
# endif
#endif
#if defined GL_INT_SAMPLER_2D
# if defined IntSampler2D
#  pragma push_macro("IntSampler2D")
#  undef IntSampler2D
	Transform<SLDataType::IntSampler2D> IntSampler2D;
#  pragma pop_macro("IntSampler2D")
# else
	Transform<SLDataType::IntSampler2D> IntSampler2D;
# endif
#endif
#if defined GL_INT_SAMPLER_3D
# if defined IntSampler3D
#  pragma push_macro("IntSampler3D")
#  undef IntSampler3D
	Transform<SLDataType::IntSampler3D> IntSampler3D;
#  pragma pop_macro("IntSampler3D")
# else
	Transform<SLDataType::IntSampler3D> IntSampler3D;
# endif
#endif
#if defined GL_INT_SAMPLER_CUBE
# if defined IntSamplerCube
#  pragma push_macro("IntSamplerCube")
#  undef IntSamplerCube
	Transform<SLDataType::IntSamplerCube> IntSamplerCube;
#  pragma pop_macro("IntSamplerCube")
# else
	Transform<SLDataType::IntSamplerCube> IntSamplerCube;
# endif
#endif
#if defined GL_INT_SAMPLER_1D_ARRAY
# if defined IntSampler1DArray
#  pragma push_macro("IntSampler1DArray")
#  undef IntSampler1DArray
	Transform<SLDataType::IntSampler1DArray> IntSampler1DArray;
#  pragma pop_macro("IntSampler1DArray")
# else
	Transform<SLDataType::IntSampler1DArray> IntSampler1DArray;
# endif
#endif
#if defined GL_INT_SAMPLER_2D_ARRAY
# if defined IntSampler2DArray
#  pragma push_macro("IntSampler2DArray")
#  undef IntSampler2DArray
	Transform<SLDataType::IntSampler2DArray> IntSampler2DArray;
#  pragma pop_macro("IntSampler2DArray")
# else
	Transform<SLDataType::IntSampler2DArray> IntSampler2DArray;
# endif
#endif
#if defined GL_INT_SAMPLER_CUBE_MAP_ARRAY
# if defined IntSamplerCubeMapArray
#  pragma push_macro("IntSamplerCubeMapArray")
#  undef IntSamplerCubeMapArray
	Transform<SLDataType::IntSamplerCubeMapArray> IntSamplerCubeMapArray;
#  pragma pop_macro("IntSamplerCubeMapArray")
# else
	Transform<SLDataType::IntSamplerCubeMapArray> IntSamplerCubeMapArray;
# endif
#endif
#if defined GL_INT_SAMPLER_2D_MULTISAMPLE
# if defined IntSampler2DMultisample
#  pragma push_macro("IntSampler2DMultisample")
#  undef IntSampler2DMultisample
	Transform<SLDataType::IntSampler2DMultisample> IntSampler2DMultisample;
#  pragma pop_macro("IntSampler2DMultisample")
# else
	Transform<SLDataType::IntSampler2DMultisample> IntSampler2DMultisample;
# endif
#endif
#if defined GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
# if defined IntSampler2DMultisampleArray
#  pragma push_macro("IntSampler2DMultisampleArray")
#  undef IntSampler2DMultisampleArray
	Transform<SLDataType::IntSampler2DMultisampleArray> IntSampler2DMultisampleArray;
#  pragma pop_macro("IntSampler2DMultisampleArray")
# else
	Transform<SLDataType::IntSampler2DMultisampleArray> IntSampler2DMultisampleArray;
# endif
#endif
#if defined GL_INT_SAMPLER_BUFFER
# if defined IntSamplerBuffer
#  pragma push_macro("IntSamplerBuffer")
#  undef IntSamplerBuffer
	Transform<SLDataType::IntSamplerBuffer> IntSamplerBuffer;
#  pragma pop_macro("IntSamplerBuffer")
# else
	Transform<SLDataType::IntSamplerBuffer> IntSamplerBuffer;
# endif
#endif
#if defined GL_INT_SAMPLER_2D_RECT
# if defined IntSampler2DRect
#  pragma push_macro("IntSampler2DRect")
#  undef IntSampler2DRect
	Transform<SLDataType::IntSampler2DRect> IntSampler2DRect;
#  pragma pop_macro("IntSampler2DRect")
# else
	Transform<SLDataType::IntSampler2DRect> IntSampler2DRect;
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_1D
# if defined UnsignedIntSampler1D
#  pragma push_macro("UnsignedIntSampler1D")
#  undef UnsignedIntSampler1D
	Transform<SLDataType::UnsignedIntSampler1D> UnsignedIntSampler1D;
#  pragma pop_macro("UnsignedIntSampler1D")
# else
	Transform<SLDataType::UnsignedIntSampler1D> UnsignedIntSampler1D;
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_2D
# if defined UnsignedIntSampler2D
#  pragma push_macro("UnsignedIntSampler2D")
#  undef UnsignedIntSampler2D
	Transform<SLDataType::UnsignedIntSampler2D> UnsignedIntSampler2D;
#  pragma pop_macro("UnsignedIntSampler2D")
# else
	Transform<SLDataType::UnsignedIntSampler2D> UnsignedIntSampler2D;
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_3D
# if defined UnsignedIntSampler3D
#  pragma push_macro("UnsignedIntSampler3D")
#  undef UnsignedIntSampler3D
	Transform<SLDataType::UnsignedIntSampler3D> UnsignedIntSampler3D;
#  pragma pop_macro("UnsignedIntSampler3D")
# else
	Transform<SLDataType::UnsignedIntSampler3D> UnsignedIntSampler3D;
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_CUBE
# if defined UnsignedIntSamplerCube
#  pragma push_macro("UnsignedIntSamplerCube")
#  undef UnsignedIntSamplerCube
	Transform<SLDataType::UnsignedIntSamplerCube> UnsignedIntSamplerCube;
#  pragma pop_macro("UnsignedIntSamplerCube")
# else
	Transform<SLDataType::UnsignedIntSamplerCube> UnsignedIntSamplerCube;
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_1D_ARRAY
# if defined UnsignedIntSampler1DArray
#  pragma push_macro("UnsignedIntSampler1DArray")
#  undef UnsignedIntSampler1DArray
	Transform<SLDataType::UnsignedIntSampler1DArray> UnsignedIntSampler1DArray;
#  pragma pop_macro("UnsignedIntSampler1DArray")
# else
	Transform<SLDataType::UnsignedIntSampler1DArray> UnsignedIntSampler1DArray;
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_2D_ARRAY
# if defined UnsignedIntSampler2DArray
#  pragma push_macro("UnsignedIntSampler2DArray")
#  undef UnsignedIntSampler2DArray
	Transform<SLDataType::UnsignedIntSampler2DArray> UnsignedIntSampler2DArray;
#  pragma pop_macro("UnsignedIntSampler2DArray")
# else
	Transform<SLDataType::UnsignedIntSampler2DArray> UnsignedIntSampler2DArray;
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY
# if defined UnsignedIntSamplerCubeMapArray
#  pragma push_macro("UnsignedIntSamplerCubeMapArray")
#  undef UnsignedIntSamplerCubeMapArray
	Transform<SLDataType::UnsignedIntSamplerCubeMapArray> UnsignedIntSamplerCubeMapArray;
#  pragma pop_macro("UnsignedIntSamplerCubeMapArray")
# else
	Transform<SLDataType::UnsignedIntSamplerCubeMapArray> UnsignedIntSamplerCubeMapArray;
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE
# if defined UnsignedIntSampler2DMultisample
#  pragma push_macro("UnsignedIntSampler2DMultisample")
#  undef UnsignedIntSampler2DMultisample
	Transform<SLDataType::UnsignedIntSampler2DMultisample> UnsignedIntSampler2DMultisample;
#  pragma pop_macro("UnsignedIntSampler2DMultisample")
# else
	Transform<SLDataType::UnsignedIntSampler2DMultisample> UnsignedIntSampler2DMultisample;
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
# if defined UnsignedIntSampler2DMultisampleArray
#  pragma push_macro("UnsignedIntSampler2DMultisampleArray")
#  undef UnsignedIntSampler2DMultisampleArray
	Transform<SLDataType::UnsignedIntSampler2DMultisampleArray> UnsignedIntSampler2DMultisampleArray;
#  pragma pop_macro("UnsignedIntSampler2DMultisampleArray")
# else
	Transform<SLDataType::UnsignedIntSampler2DMultisampleArray> UnsignedIntSampler2DMultisampleArray;
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_BUFFER
# if defined UnsignedIntSamplerBuffer
#  pragma push_macro("UnsignedIntSamplerBuffer")
#  undef UnsignedIntSamplerBuffer
	Transform<SLDataType::UnsignedIntSamplerBuffer> UnsignedIntSamplerBuffer;
#  pragma pop_macro("UnsignedIntSamplerBuffer")
# else
	Transform<SLDataType::UnsignedIntSamplerBuffer> UnsignedIntSamplerBuffer;
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_2D_RECT
# if defined UnsignedIntSampler2DRect
#  pragma push_macro("UnsignedIntSampler2DRect")
#  undef UnsignedIntSampler2DRect
	Transform<SLDataType::UnsignedIntSampler2DRect> UnsignedIntSampler2DRect;
#  pragma pop_macro("UnsignedIntSampler2DRect")
# else
	Transform<SLDataType::UnsignedIntSampler2DRect> UnsignedIntSampler2DRect;
# endif
#endif
#if defined GL_IMAGE_1D
# if defined Image1D
#  pragma push_macro("Image1D")
#  undef Image1D
	Transform<SLDataType::Image1D> Image1D;
#  pragma pop_macro("Image1D")
# else
	Transform<SLDataType::Image1D> Image1D;
# endif
#endif
#if defined GL_IMAGE_2D
# if defined Image2D
#  pragma push_macro("Image2D")
#  undef Image2D
	Transform<SLDataType::Image2D> Image2D;
#  pragma pop_macro("Image2D")
# else
	Transform<SLDataType::Image2D> Image2D;
# endif
#endif
#if defined GL_IMAGE_3D
# if defined Image3D
#  pragma push_macro("Image3D")
#  undef Image3D
	Transform<SLDataType::Image3D> Image3D;
#  pragma pop_macro("Image3D")
# else
	Transform<SLDataType::Image3D> Image3D;
# endif
#endif
#if defined GL_IMAGE_2D_RECT
# if defined Image2DRect
#  pragma push_macro("Image2DRect")
#  undef Image2DRect
	Transform<SLDataType::Image2DRect> Image2DRect;
#  pragma pop_macro("Image2DRect")
# else
	Transform<SLDataType::Image2DRect> Image2DRect;
# endif
#endif
#if defined GL_IMAGE_CUBE
# if defined ImageCube
#  pragma push_macro("ImageCube")
#  undef ImageCube
	Transform<SLDataType::ImageCube> ImageCube;
#  pragma pop_macro("ImageCube")
# else
	Transform<SLDataType::ImageCube> ImageCube;
# endif
#endif
#if defined GL_IMAGE_BUFFER
# if defined ImageBuffer
#  pragma push_macro("ImageBuffer")
#  undef ImageBuffer
	Transform<SLDataType::ImageBuffer> ImageBuffer;
#  pragma pop_macro("ImageBuffer")
# else
	Transform<SLDataType::ImageBuffer> ImageBuffer;
# endif
#endif
#if defined GL_IMAGE_1D_ARRAY
# if defined Image1DArray
#  pragma push_macro("Image1DArray")
#  undef Image1DArray
	Transform<SLDataType::Image1DArray> Image1DArray;
#  pragma pop_macro("Image1DArray")
# else
	Transform<SLDataType::Image1DArray> Image1DArray;
# endif
#endif
#if defined GL_IMAGE_2D_ARRAY
# if defined Image2DArray
#  pragma push_macro("Image2DArray")
#  undef Image2DArray
	Transform<SLDataType::Image2DArray> Image2DArray;
#  pragma pop_macro("Image2DArray")
# else
	Transform<SLDataType::Image2DArray> Image2DArray;
# endif
#endif
#if defined GL_IMAGE_2D_MULTISAMPLE
# if defined Image2DMultisample
#  pragma push_macro("Image2DMultisample")
#  undef Image2DMultisample
	Transform<SLDataType::Image2DMultisample> Image2DMultisample;
#  pragma pop_macro("Image2DMultisample")
# else
	Transform<SLDataType::Image2DMultisample> Image2DMultisample;
# endif
#endif
#if defined GL_IMAGE_2D_MULTISAMPLE_ARRAY
# if defined Image2DMultisampleArray
#  pragma push_macro("Image2DMultisampleArray")
#  undef Image2DMultisampleArray
	Transform<SLDataType::Image2DMultisampleArray> Image2DMultisampleArray;
#  pragma pop_macro("Image2DMultisampleArray")
# else
	Transform<SLDataType::Image2DMultisampleArray> Image2DMultisampleArray;
# endif
#endif
#if defined GL_INT_IMAGE_1D
# if defined IntImage1D
#  pragma push_macro("IntImage1D")
#  undef IntImage1D
	Transform<SLDataType::IntImage1D> IntImage1D;
#  pragma pop_macro("IntImage1D")
# else
	Transform<SLDataType::IntImage1D> IntImage1D;
# endif
#endif
#if defined GL_INT_IMAGE_2D
# if defined IntImage2D
#  pragma push_macro("IntImage2D")
#  undef IntImage2D
	Transform<SLDataType::IntImage2D> IntImage2D;
#  pragma pop_macro("IntImage2D")
# else
	Transform<SLDataType::IntImage2D> IntImage2D;
# endif
#endif
#if defined GL_INT_IMAGE_3D
# if defined IntImage3D
#  pragma push_macro("IntImage3D")
#  undef IntImage3D
	Transform<SLDataType::IntImage3D> IntImage3D;
#  pragma pop_macro("IntImage3D")
# else
	Transform<SLDataType::IntImage3D> IntImage3D;
# endif
#endif
#if defined GL_INT_IMAGE_2D_RECT
# if defined IntImage2DRect
#  pragma push_macro("IntImage2DRect")
#  undef IntImage2DRect
	Transform<SLDataType::IntImage2DRect> IntImage2DRect;
#  pragma pop_macro("IntImage2DRect")
# else
	Transform<SLDataType::IntImage2DRect> IntImage2DRect;
# endif
#endif
#if defined GL_INT_IMAGE_CUBE
# if defined IntImageCube
#  pragma push_macro("IntImageCube")
#  undef IntImageCube
	Transform<SLDataType::IntImageCube> IntImageCube;
#  pragma pop_macro("IntImageCube")
# else
	Transform<SLDataType::IntImageCube> IntImageCube;
# endif
#endif
#if defined GL_INT_IMAGE_BUFFER
# if defined IntImageBuffer
#  pragma push_macro("IntImageBuffer")
#  undef IntImageBuffer
	Transform<SLDataType::IntImageBuffer> IntImageBuffer;
#  pragma pop_macro("IntImageBuffer")
# else
	Transform<SLDataType::IntImageBuffer> IntImageBuffer;
# endif
#endif
#if defined GL_INT_IMAGE_1D_ARRAY
# if defined IntImage1DArray
#  pragma push_macro("IntImage1DArray")
#  undef IntImage1DArray
	Transform<SLDataType::IntImage1DArray> IntImage1DArray;
#  pragma pop_macro("IntImage1DArray")
# else
	Transform<SLDataType::IntImage1DArray> IntImage1DArray;
# endif
#endif
#if defined GL_INT_IMAGE_2D_ARRAY
# if defined IntImage2DArray
#  pragma push_macro("IntImage2DArray")
#  undef IntImage2DArray
	Transform<SLDataType::IntImage2DArray> IntImage2DArray;
#  pragma pop_macro("IntImage2DArray")
# else
	Transform<SLDataType::IntImage2DArray> IntImage2DArray;
# endif
#endif
#if defined GL_INT_IMAGE_2D_MULTISAMPLE
# if defined IntImage2DMultisample
#  pragma push_macro("IntImage2DMultisample")
#  undef IntImage2DMultisample
	Transform<SLDataType::IntImage2DMultisample> IntImage2DMultisample;
#  pragma pop_macro("IntImage2DMultisample")
# else
	Transform<SLDataType::IntImage2DMultisample> IntImage2DMultisample;
# endif
#endif
#if defined GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY
# if defined IntImage2DMultisampleArray
#  pragma push_macro("IntImage2DMultisampleArray")
#  undef IntImage2DMultisampleArray
	Transform<SLDataType::IntImage2DMultisampleArray> IntImage2DMultisampleArray;
#  pragma pop_macro("IntImage2DMultisampleArray")
# else
	Transform<SLDataType::IntImage2DMultisampleArray> IntImage2DMultisampleArray;
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_1D
# if defined UnsignedIntImage1D
#  pragma push_macro("UnsignedIntImage1D")
#  undef UnsignedIntImage1D
	Transform<SLDataType::UnsignedIntImage1D> UnsignedIntImage1D;
#  pragma pop_macro("UnsignedIntImage1D")
# else
	Transform<SLDataType::UnsignedIntImage1D> UnsignedIntImage1D;
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_2D
# if defined UnsignedIntImage2D
#  pragma push_macro("UnsignedIntImage2D")
#  undef UnsignedIntImage2D
	Transform<SLDataType::UnsignedIntImage2D> UnsignedIntImage2D;
#  pragma pop_macro("UnsignedIntImage2D")
# else
	Transform<SLDataType::UnsignedIntImage2D> UnsignedIntImage2D;
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_3D
# if defined UnsignedIntImage3D
#  pragma push_macro("UnsignedIntImage3D")
#  undef UnsignedIntImage3D
	Transform<SLDataType::UnsignedIntImage3D> UnsignedIntImage3D;
#  pragma pop_macro("UnsignedIntImage3D")
# else
	Transform<SLDataType::UnsignedIntImage3D> UnsignedIntImage3D;
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_2D_RECT
# if defined UnsignedIntImage2DRect
#  pragma push_macro("UnsignedIntImage2DRect")
#  undef UnsignedIntImage2DRect
	Transform<SLDataType::UnsignedIntImage2DRect> UnsignedIntImage2DRect;
#  pragma pop_macro("UnsignedIntImage2DRect")
# else
	Transform<SLDataType::UnsignedIntImage2DRect> UnsignedIntImage2DRect;
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_CUBE
# if defined UnsignedIntImageCube
#  pragma push_macro("UnsignedIntImageCube")
#  undef UnsignedIntImageCube
	Transform<SLDataType::UnsignedIntImageCube> UnsignedIntImageCube;
#  pragma pop_macro("UnsignedIntImageCube")
# else
	Transform<SLDataType::UnsignedIntImageCube> UnsignedIntImageCube;
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_BUFFER
# if defined UnsignedIntImageBuffer
#  pragma push_macro("UnsignedIntImageBuffer")
#  undef UnsignedIntImageBuffer
	Transform<SLDataType::UnsignedIntImageBuffer> UnsignedIntImageBuffer;
#  pragma pop_macro("UnsignedIntImageBuffer")
# else
	Transform<SLDataType::UnsignedIntImageBuffer> UnsignedIntImageBuffer;
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_1D_ARRAY
# if defined UnsignedIntImage1DArray
#  pragma push_macro("UnsignedIntImage1DArray")
#  undef UnsignedIntImage1DArray
	Transform<SLDataType::UnsignedIntImage1DArray> UnsignedIntImage1DArray;
#  pragma pop_macro("UnsignedIntImage1DArray")
# else
	Transform<SLDataType::UnsignedIntImage1DArray> UnsignedIntImage1DArray;
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_2D_ARRAY
# if defined UnsignedIntImage2DArray
#  pragma push_macro("UnsignedIntImage2DArray")
#  undef UnsignedIntImage2DArray
	Transform<SLDataType::UnsignedIntImage2DArray> UnsignedIntImage2DArray;
#  pragma pop_macro("UnsignedIntImage2DArray")
# else
	Transform<SLDataType::UnsignedIntImage2DArray> UnsignedIntImage2DArray;
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE
# if defined UnsignedIntImage2DMultisample
#  pragma push_macro("UnsignedIntImage2DMultisample")
#  undef UnsignedIntImage2DMultisample
	Transform<SLDataType::UnsignedIntImage2DMultisample> UnsignedIntImage2DMultisample;
#  pragma pop_macro("UnsignedIntImage2DMultisample")
# else
	Transform<SLDataType::UnsignedIntImage2DMultisample> UnsignedIntImage2DMultisample;
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY
# if defined UnsignedIntImage2DMultisampleArray
#  pragma push_macro("UnsignedIntImage2DMultisampleArray")
#  undef UnsignedIntImage2DMultisampleArray
	Transform<SLDataType::UnsignedIntImage2DMultisampleArray> UnsignedIntImage2DMultisampleArray;
#  pragma pop_macro("UnsignedIntImage2DMultisampleArray")
# else
	Transform<SLDataType::UnsignedIntImage2DMultisampleArray> UnsignedIntImage2DMultisampleArray;
# endif
#endif
#if defined GL_UNSIGNED_INT_ATOMIC_COUNTER
# if defined UnsignedIntAtomicCounter
#  pragma push_macro("UnsignedIntAtomicCounter")
#  undef UnsignedIntAtomicCounter
	Transform<SLDataType::UnsignedIntAtomicCounter> UnsignedIntAtomicCounter;
#  pragma pop_macro("UnsignedIntAtomicCounter")
# else
	Transform<SLDataType::UnsignedIntAtomicCounter> UnsignedIntAtomicCounter;
# endif
#endif
#if defined GL_NONE
# if defined None
#  pragma push_macro("None")
#  undef None
	Transform<SLDataType::None> None;
#  pragma pop_macro("None")
# else
	Transform<SLDataType::None> None;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_FLOAT
# if defined Float
#  pragma push_macro("Float")
#  undef Float
	 , Float(_base())
#  pragma pop_macro("Float")
# else
	 , Float(_base())
# endif
#endif
#if defined GL_FLOAT_VEC2
# if defined FloatVec2
#  pragma push_macro("FloatVec2")
#  undef FloatVec2
	 , FloatVec2(_base())
#  pragma pop_macro("FloatVec2")
# else
	 , FloatVec2(_base())
# endif
#endif
#if defined GL_FLOAT_VEC3
# if defined FloatVec3
#  pragma push_macro("FloatVec3")
#  undef FloatVec3
	 , FloatVec3(_base())
#  pragma pop_macro("FloatVec3")
# else
	 , FloatVec3(_base())
# endif
#endif
#if defined GL_FLOAT_VEC4
# if defined FloatVec4
#  pragma push_macro("FloatVec4")
#  undef FloatVec4
	 , FloatVec4(_base())
#  pragma pop_macro("FloatVec4")
# else
	 , FloatVec4(_base())
# endif
#endif
#if defined GL_DOUBLE
# if defined Double
#  pragma push_macro("Double")
#  undef Double
	 , Double(_base())
#  pragma pop_macro("Double")
# else
	 , Double(_base())
# endif
#endif
#if defined GL_DOUBLE_VEC2
# if defined DoubleVec2
#  pragma push_macro("DoubleVec2")
#  undef DoubleVec2
	 , DoubleVec2(_base())
#  pragma pop_macro("DoubleVec2")
# else
	 , DoubleVec2(_base())
# endif
#endif
#if defined GL_DOUBLE_VEC3
# if defined DoubleVec3
#  pragma push_macro("DoubleVec3")
#  undef DoubleVec3
	 , DoubleVec3(_base())
#  pragma pop_macro("DoubleVec3")
# else
	 , DoubleVec3(_base())
# endif
#endif
#if defined GL_DOUBLE_VEC4
# if defined DoubleVec4
#  pragma push_macro("DoubleVec4")
#  undef DoubleVec4
	 , DoubleVec4(_base())
#  pragma pop_macro("DoubleVec4")
# else
	 , DoubleVec4(_base())
# endif
#endif
#if defined GL_INT
# if defined Int
#  pragma push_macro("Int")
#  undef Int
	 , Int(_base())
#  pragma pop_macro("Int")
# else
	 , Int(_base())
# endif
#endif
#if defined GL_INT_VEC2
# if defined IntVec2
#  pragma push_macro("IntVec2")
#  undef IntVec2
	 , IntVec2(_base())
#  pragma pop_macro("IntVec2")
# else
	 , IntVec2(_base())
# endif
#endif
#if defined GL_INT_VEC3
# if defined IntVec3
#  pragma push_macro("IntVec3")
#  undef IntVec3
	 , IntVec3(_base())
#  pragma pop_macro("IntVec3")
# else
	 , IntVec3(_base())
# endif
#endif
#if defined GL_INT_VEC4
# if defined IntVec4
#  pragma push_macro("IntVec4")
#  undef IntVec4
	 , IntVec4(_base())
#  pragma pop_macro("IntVec4")
# else
	 , IntVec4(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT
# if defined UnsignedInt
#  pragma push_macro("UnsignedInt")
#  undef UnsignedInt
	 , UnsignedInt(_base())
#  pragma pop_macro("UnsignedInt")
# else
	 , UnsignedInt(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_VEC2
# if defined UnsignedIntVec2
#  pragma push_macro("UnsignedIntVec2")
#  undef UnsignedIntVec2
	 , UnsignedIntVec2(_base())
#  pragma pop_macro("UnsignedIntVec2")
# else
	 , UnsignedIntVec2(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_VEC3
# if defined UnsignedIntVec3
#  pragma push_macro("UnsignedIntVec3")
#  undef UnsignedIntVec3
	 , UnsignedIntVec3(_base())
#  pragma pop_macro("UnsignedIntVec3")
# else
	 , UnsignedIntVec3(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_VEC4
# if defined UnsignedIntVec4
#  pragma push_macro("UnsignedIntVec4")
#  undef UnsignedIntVec4
	 , UnsignedIntVec4(_base())
#  pragma pop_macro("UnsignedIntVec4")
# else
	 , UnsignedIntVec4(_base())
# endif
#endif
#if defined GL_BOOL
# if defined Bool
#  pragma push_macro("Bool")
#  undef Bool
	 , Bool(_base())
#  pragma pop_macro("Bool")
# else
	 , Bool(_base())
# endif
#endif
#if defined GL_BOOL_VEC2
# if defined BoolVec2
#  pragma push_macro("BoolVec2")
#  undef BoolVec2
	 , BoolVec2(_base())
#  pragma pop_macro("BoolVec2")
# else
	 , BoolVec2(_base())
# endif
#endif
#if defined GL_BOOL_VEC3
# if defined BoolVec3
#  pragma push_macro("BoolVec3")
#  undef BoolVec3
	 , BoolVec3(_base())
#  pragma pop_macro("BoolVec3")
# else
	 , BoolVec3(_base())
# endif
#endif
#if defined GL_BOOL_VEC4
# if defined BoolVec4
#  pragma push_macro("BoolVec4")
#  undef BoolVec4
	 , BoolVec4(_base())
#  pragma pop_macro("BoolVec4")
# else
	 , BoolVec4(_base())
# endif
#endif
#if defined GL_FLOAT_MAT2
# if defined FloatMat2
#  pragma push_macro("FloatMat2")
#  undef FloatMat2
	 , FloatMat2(_base())
#  pragma pop_macro("FloatMat2")
# else
	 , FloatMat2(_base())
# endif
#endif
#if defined GL_FLOAT_MAT3
# if defined FloatMat3
#  pragma push_macro("FloatMat3")
#  undef FloatMat3
	 , FloatMat3(_base())
#  pragma pop_macro("FloatMat3")
# else
	 , FloatMat3(_base())
# endif
#endif
#if defined GL_FLOAT_MAT4
# if defined FloatMat4
#  pragma push_macro("FloatMat4")
#  undef FloatMat4
	 , FloatMat4(_base())
#  pragma pop_macro("FloatMat4")
# else
	 , FloatMat4(_base())
# endif
#endif
#if defined GL_FLOAT_MAT2x3
# if defined FloatMat2x3
#  pragma push_macro("FloatMat2x3")
#  undef FloatMat2x3
	 , FloatMat2x3(_base())
#  pragma pop_macro("FloatMat2x3")
# else
	 , FloatMat2x3(_base())
# endif
#endif
#if defined GL_FLOAT_MAT2x4
# if defined FloatMat2x4
#  pragma push_macro("FloatMat2x4")
#  undef FloatMat2x4
	 , FloatMat2x4(_base())
#  pragma pop_macro("FloatMat2x4")
# else
	 , FloatMat2x4(_base())
# endif
#endif
#if defined GL_FLOAT_MAT3x2
# if defined FloatMat3x2
#  pragma push_macro("FloatMat3x2")
#  undef FloatMat3x2
	 , FloatMat3x2(_base())
#  pragma pop_macro("FloatMat3x2")
# else
	 , FloatMat3x2(_base())
# endif
#endif
#if defined GL_FLOAT_MAT3x4
# if defined FloatMat3x4
#  pragma push_macro("FloatMat3x4")
#  undef FloatMat3x4
	 , FloatMat3x4(_base())
#  pragma pop_macro("FloatMat3x4")
# else
	 , FloatMat3x4(_base())
# endif
#endif
#if defined GL_FLOAT_MAT4x2
# if defined FloatMat4x2
#  pragma push_macro("FloatMat4x2")
#  undef FloatMat4x2
	 , FloatMat4x2(_base())
#  pragma pop_macro("FloatMat4x2")
# else
	 , FloatMat4x2(_base())
# endif
#endif
#if defined GL_FLOAT_MAT4x3
# if defined FloatMat4x3
#  pragma push_macro("FloatMat4x3")
#  undef FloatMat4x3
	 , FloatMat4x3(_base())
#  pragma pop_macro("FloatMat4x3")
# else
	 , FloatMat4x3(_base())
# endif
#endif
#if defined GL_DOUBLE_MAT2
# if defined DoubleMat2
#  pragma push_macro("DoubleMat2")
#  undef DoubleMat2
	 , DoubleMat2(_base())
#  pragma pop_macro("DoubleMat2")
# else
	 , DoubleMat2(_base())
# endif
#endif
#if defined GL_DOUBLE_MAT3
# if defined DoubleMat3
#  pragma push_macro("DoubleMat3")
#  undef DoubleMat3
	 , DoubleMat3(_base())
#  pragma pop_macro("DoubleMat3")
# else
	 , DoubleMat3(_base())
# endif
#endif
#if defined GL_DOUBLE_MAT4
# if defined DoubleMat4
#  pragma push_macro("DoubleMat4")
#  undef DoubleMat4
	 , DoubleMat4(_base())
#  pragma pop_macro("DoubleMat4")
# else
	 , DoubleMat4(_base())
# endif
#endif
#if defined GL_DOUBLE_MAT2x3
# if defined DoubleMat2x3
#  pragma push_macro("DoubleMat2x3")
#  undef DoubleMat2x3
	 , DoubleMat2x3(_base())
#  pragma pop_macro("DoubleMat2x3")
# else
	 , DoubleMat2x3(_base())
# endif
#endif
#if defined GL_DOUBLE_MAT2x4
# if defined DoubleMat2x4
#  pragma push_macro("DoubleMat2x4")
#  undef DoubleMat2x4
	 , DoubleMat2x4(_base())
#  pragma pop_macro("DoubleMat2x4")
# else
	 , DoubleMat2x4(_base())
# endif
#endif
#if defined GL_DOUBLE_MAT3x2
# if defined DoubleMat3x2
#  pragma push_macro("DoubleMat3x2")
#  undef DoubleMat3x2
	 , DoubleMat3x2(_base())
#  pragma pop_macro("DoubleMat3x2")
# else
	 , DoubleMat3x2(_base())
# endif
#endif
#if defined GL_DOUBLE_MAT3x4
# if defined DoubleMat3x4
#  pragma push_macro("DoubleMat3x4")
#  undef DoubleMat3x4
	 , DoubleMat3x4(_base())
#  pragma pop_macro("DoubleMat3x4")
# else
	 , DoubleMat3x4(_base())
# endif
#endif
#if defined GL_DOUBLE_MAT4x2
# if defined DoubleMat4x2
#  pragma push_macro("DoubleMat4x2")
#  undef DoubleMat4x2
	 , DoubleMat4x2(_base())
#  pragma pop_macro("DoubleMat4x2")
# else
	 , DoubleMat4x2(_base())
# endif
#endif
#if defined GL_DOUBLE_MAT4x3
# if defined DoubleMat4x3
#  pragma push_macro("DoubleMat4x3")
#  undef DoubleMat4x3
	 , DoubleMat4x3(_base())
#  pragma pop_macro("DoubleMat4x3")
# else
	 , DoubleMat4x3(_base())
# endif
#endif
#if defined GL_SAMPLER_1D
# if defined Sampler1D
#  pragma push_macro("Sampler1D")
#  undef Sampler1D
	 , Sampler1D(_base())
#  pragma pop_macro("Sampler1D")
# else
	 , Sampler1D(_base())
# endif
#endif
#if defined GL_SAMPLER_2D
# if defined Sampler2D
#  pragma push_macro("Sampler2D")
#  undef Sampler2D
	 , Sampler2D(_base())
#  pragma pop_macro("Sampler2D")
# else
	 , Sampler2D(_base())
# endif
#endif
#if defined GL_SAMPLER_3D
# if defined Sampler3D
#  pragma push_macro("Sampler3D")
#  undef Sampler3D
	 , Sampler3D(_base())
#  pragma pop_macro("Sampler3D")
# else
	 , Sampler3D(_base())
# endif
#endif
#if defined GL_SAMPLER_CUBE
# if defined SamplerCube
#  pragma push_macro("SamplerCube")
#  undef SamplerCube
	 , SamplerCube(_base())
#  pragma pop_macro("SamplerCube")
# else
	 , SamplerCube(_base())
# endif
#endif
#if defined GL_SAMPLER_1D_SHADOW
# if defined Sampler1DShadow
#  pragma push_macro("Sampler1DShadow")
#  undef Sampler1DShadow
	 , Sampler1DShadow(_base())
#  pragma pop_macro("Sampler1DShadow")
# else
	 , Sampler1DShadow(_base())
# endif
#endif
#if defined GL_SAMPLER_2D_SHADOW
# if defined Sampler2DShadow
#  pragma push_macro("Sampler2DShadow")
#  undef Sampler2DShadow
	 , Sampler2DShadow(_base())
#  pragma pop_macro("Sampler2DShadow")
# else
	 , Sampler2DShadow(_base())
# endif
#endif
#if defined GL_SAMPLER_1D_ARRAY
# if defined Sampler1DArray
#  pragma push_macro("Sampler1DArray")
#  undef Sampler1DArray
	 , Sampler1DArray(_base())
#  pragma pop_macro("Sampler1DArray")
# else
	 , Sampler1DArray(_base())
# endif
#endif
#if defined GL_SAMPLER_2D_ARRAY
# if defined Sampler2DArray
#  pragma push_macro("Sampler2DArray")
#  undef Sampler2DArray
	 , Sampler2DArray(_base())
#  pragma pop_macro("Sampler2DArray")
# else
	 , Sampler2DArray(_base())
# endif
#endif
#if defined GL_SAMPLER_CUBE_MAP_ARRAY
# if defined SamplerCubeMapArray
#  pragma push_macro("SamplerCubeMapArray")
#  undef SamplerCubeMapArray
	 , SamplerCubeMapArray(_base())
#  pragma pop_macro("SamplerCubeMapArray")
# else
	 , SamplerCubeMapArray(_base())
# endif
#endif
#if defined GL_SAMPLER_1D_ARRAY_SHADOW
# if defined Sampler1DArrayShadow
#  pragma push_macro("Sampler1DArrayShadow")
#  undef Sampler1DArrayShadow
	 , Sampler1DArrayShadow(_base())
#  pragma pop_macro("Sampler1DArrayShadow")
# else
	 , Sampler1DArrayShadow(_base())
# endif
#endif
#if defined GL_SAMPLER_2D_ARRAY_SHADOW
# if defined Sampler2DArrayShadow
#  pragma push_macro("Sampler2DArrayShadow")
#  undef Sampler2DArrayShadow
	 , Sampler2DArrayShadow(_base())
#  pragma pop_macro("Sampler2DArrayShadow")
# else
	 , Sampler2DArrayShadow(_base())
# endif
#endif
#if defined GL_SAMPLER_2D_MULTISAMPLE
# if defined Sampler2DMultisample
#  pragma push_macro("Sampler2DMultisample")
#  undef Sampler2DMultisample
	 , Sampler2DMultisample(_base())
#  pragma pop_macro("Sampler2DMultisample")
# else
	 , Sampler2DMultisample(_base())
# endif
#endif
#if defined GL_SAMPLER_2D_MULTISAMPLE_ARRAY
# if defined Sampler2DMultisampleArray
#  pragma push_macro("Sampler2DMultisampleArray")
#  undef Sampler2DMultisampleArray
	 , Sampler2DMultisampleArray(_base())
#  pragma pop_macro("Sampler2DMultisampleArray")
# else
	 , Sampler2DMultisampleArray(_base())
# endif
#endif
#if defined GL_SAMPLER_CUBE_SHADOW
# if defined SamplerCubeShadow
#  pragma push_macro("SamplerCubeShadow")
#  undef SamplerCubeShadow
	 , SamplerCubeShadow(_base())
#  pragma pop_macro("SamplerCubeShadow")
# else
	 , SamplerCubeShadow(_base())
# endif
#endif
#if defined GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW
# if defined SamplerCubeMapArrayShadow
#  pragma push_macro("SamplerCubeMapArrayShadow")
#  undef SamplerCubeMapArrayShadow
	 , SamplerCubeMapArrayShadow(_base())
#  pragma pop_macro("SamplerCubeMapArrayShadow")
# else
	 , SamplerCubeMapArrayShadow(_base())
# endif
#endif
#if defined GL_SAMPLER_BUFFER
# if defined SamplerBuffer
#  pragma push_macro("SamplerBuffer")
#  undef SamplerBuffer
	 , SamplerBuffer(_base())
#  pragma pop_macro("SamplerBuffer")
# else
	 , SamplerBuffer(_base())
# endif
#endif
#if defined GL_SAMPLER_2D_RECT
# if defined Sampler2DRect
#  pragma push_macro("Sampler2DRect")
#  undef Sampler2DRect
	 , Sampler2DRect(_base())
#  pragma pop_macro("Sampler2DRect")
# else
	 , Sampler2DRect(_base())
# endif
#endif
#if defined GL_SAMPLER_2D_RECT_SHADOW
# if defined Sampler2DRectShadow
#  pragma push_macro("Sampler2DRectShadow")
#  undef Sampler2DRectShadow
	 , Sampler2DRectShadow(_base())
#  pragma pop_macro("Sampler2DRectShadow")
# else
	 , Sampler2DRectShadow(_base())
# endif
#endif
#if defined GL_INT_SAMPLER_1D
# if defined IntSampler1D
#  pragma push_macro("IntSampler1D")
#  undef IntSampler1D
	 , IntSampler1D(_base())
#  pragma pop_macro("IntSampler1D")
# else
	 , IntSampler1D(_base())
# endif
#endif
#if defined GL_INT_SAMPLER_2D
# if defined IntSampler2D
#  pragma push_macro("IntSampler2D")
#  undef IntSampler2D
	 , IntSampler2D(_base())
#  pragma pop_macro("IntSampler2D")
# else
	 , IntSampler2D(_base())
# endif
#endif
#if defined GL_INT_SAMPLER_3D
# if defined IntSampler3D
#  pragma push_macro("IntSampler3D")
#  undef IntSampler3D
	 , IntSampler3D(_base())
#  pragma pop_macro("IntSampler3D")
# else
	 , IntSampler3D(_base())
# endif
#endif
#if defined GL_INT_SAMPLER_CUBE
# if defined IntSamplerCube
#  pragma push_macro("IntSamplerCube")
#  undef IntSamplerCube
	 , IntSamplerCube(_base())
#  pragma pop_macro("IntSamplerCube")
# else
	 , IntSamplerCube(_base())
# endif
#endif
#if defined GL_INT_SAMPLER_1D_ARRAY
# if defined IntSampler1DArray
#  pragma push_macro("IntSampler1DArray")
#  undef IntSampler1DArray
	 , IntSampler1DArray(_base())
#  pragma pop_macro("IntSampler1DArray")
# else
	 , IntSampler1DArray(_base())
# endif
#endif
#if defined GL_INT_SAMPLER_2D_ARRAY
# if defined IntSampler2DArray
#  pragma push_macro("IntSampler2DArray")
#  undef IntSampler2DArray
	 , IntSampler2DArray(_base())
#  pragma pop_macro("IntSampler2DArray")
# else
	 , IntSampler2DArray(_base())
# endif
#endif
#if defined GL_INT_SAMPLER_CUBE_MAP_ARRAY
# if defined IntSamplerCubeMapArray
#  pragma push_macro("IntSamplerCubeMapArray")
#  undef IntSamplerCubeMapArray
	 , IntSamplerCubeMapArray(_base())
#  pragma pop_macro("IntSamplerCubeMapArray")
# else
	 , IntSamplerCubeMapArray(_base())
# endif
#endif
#if defined GL_INT_SAMPLER_2D_MULTISAMPLE
# if defined IntSampler2DMultisample
#  pragma push_macro("IntSampler2DMultisample")
#  undef IntSampler2DMultisample
	 , IntSampler2DMultisample(_base())
#  pragma pop_macro("IntSampler2DMultisample")
# else
	 , IntSampler2DMultisample(_base())
# endif
#endif
#if defined GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
# if defined IntSampler2DMultisampleArray
#  pragma push_macro("IntSampler2DMultisampleArray")
#  undef IntSampler2DMultisampleArray
	 , IntSampler2DMultisampleArray(_base())
#  pragma pop_macro("IntSampler2DMultisampleArray")
# else
	 , IntSampler2DMultisampleArray(_base())
# endif
#endif
#if defined GL_INT_SAMPLER_BUFFER
# if defined IntSamplerBuffer
#  pragma push_macro("IntSamplerBuffer")
#  undef IntSamplerBuffer
	 , IntSamplerBuffer(_base())
#  pragma pop_macro("IntSamplerBuffer")
# else
	 , IntSamplerBuffer(_base())
# endif
#endif
#if defined GL_INT_SAMPLER_2D_RECT
# if defined IntSampler2DRect
#  pragma push_macro("IntSampler2DRect")
#  undef IntSampler2DRect
	 , IntSampler2DRect(_base())
#  pragma pop_macro("IntSampler2DRect")
# else
	 , IntSampler2DRect(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_1D
# if defined UnsignedIntSampler1D
#  pragma push_macro("UnsignedIntSampler1D")
#  undef UnsignedIntSampler1D
	 , UnsignedIntSampler1D(_base())
#  pragma pop_macro("UnsignedIntSampler1D")
# else
	 , UnsignedIntSampler1D(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_2D
# if defined UnsignedIntSampler2D
#  pragma push_macro("UnsignedIntSampler2D")
#  undef UnsignedIntSampler2D
	 , UnsignedIntSampler2D(_base())
#  pragma pop_macro("UnsignedIntSampler2D")
# else
	 , UnsignedIntSampler2D(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_3D
# if defined UnsignedIntSampler3D
#  pragma push_macro("UnsignedIntSampler3D")
#  undef UnsignedIntSampler3D
	 , UnsignedIntSampler3D(_base())
#  pragma pop_macro("UnsignedIntSampler3D")
# else
	 , UnsignedIntSampler3D(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_CUBE
# if defined UnsignedIntSamplerCube
#  pragma push_macro("UnsignedIntSamplerCube")
#  undef UnsignedIntSamplerCube
	 , UnsignedIntSamplerCube(_base())
#  pragma pop_macro("UnsignedIntSamplerCube")
# else
	 , UnsignedIntSamplerCube(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_1D_ARRAY
# if defined UnsignedIntSampler1DArray
#  pragma push_macro("UnsignedIntSampler1DArray")
#  undef UnsignedIntSampler1DArray
	 , UnsignedIntSampler1DArray(_base())
#  pragma pop_macro("UnsignedIntSampler1DArray")
# else
	 , UnsignedIntSampler1DArray(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_2D_ARRAY
# if defined UnsignedIntSampler2DArray
#  pragma push_macro("UnsignedIntSampler2DArray")
#  undef UnsignedIntSampler2DArray
	 , UnsignedIntSampler2DArray(_base())
#  pragma pop_macro("UnsignedIntSampler2DArray")
# else
	 , UnsignedIntSampler2DArray(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY
# if defined UnsignedIntSamplerCubeMapArray
#  pragma push_macro("UnsignedIntSamplerCubeMapArray")
#  undef UnsignedIntSamplerCubeMapArray
	 , UnsignedIntSamplerCubeMapArray(_base())
#  pragma pop_macro("UnsignedIntSamplerCubeMapArray")
# else
	 , UnsignedIntSamplerCubeMapArray(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE
# if defined UnsignedIntSampler2DMultisample
#  pragma push_macro("UnsignedIntSampler2DMultisample")
#  undef UnsignedIntSampler2DMultisample
	 , UnsignedIntSampler2DMultisample(_base())
#  pragma pop_macro("UnsignedIntSampler2DMultisample")
# else
	 , UnsignedIntSampler2DMultisample(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
# if defined UnsignedIntSampler2DMultisampleArray
#  pragma push_macro("UnsignedIntSampler2DMultisampleArray")
#  undef UnsignedIntSampler2DMultisampleArray
	 , UnsignedIntSampler2DMultisampleArray(_base())
#  pragma pop_macro("UnsignedIntSampler2DMultisampleArray")
# else
	 , UnsignedIntSampler2DMultisampleArray(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_BUFFER
# if defined UnsignedIntSamplerBuffer
#  pragma push_macro("UnsignedIntSamplerBuffer")
#  undef UnsignedIntSamplerBuffer
	 , UnsignedIntSamplerBuffer(_base())
#  pragma pop_macro("UnsignedIntSamplerBuffer")
# else
	 , UnsignedIntSamplerBuffer(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_SAMPLER_2D_RECT
# if defined UnsignedIntSampler2DRect
#  pragma push_macro("UnsignedIntSampler2DRect")
#  undef UnsignedIntSampler2DRect
	 , UnsignedIntSampler2DRect(_base())
#  pragma pop_macro("UnsignedIntSampler2DRect")
# else
	 , UnsignedIntSampler2DRect(_base())
# endif
#endif
#if defined GL_IMAGE_1D
# if defined Image1D
#  pragma push_macro("Image1D")
#  undef Image1D
	 , Image1D(_base())
#  pragma pop_macro("Image1D")
# else
	 , Image1D(_base())
# endif
#endif
#if defined GL_IMAGE_2D
# if defined Image2D
#  pragma push_macro("Image2D")
#  undef Image2D
	 , Image2D(_base())
#  pragma pop_macro("Image2D")
# else
	 , Image2D(_base())
# endif
#endif
#if defined GL_IMAGE_3D
# if defined Image3D
#  pragma push_macro("Image3D")
#  undef Image3D
	 , Image3D(_base())
#  pragma pop_macro("Image3D")
# else
	 , Image3D(_base())
# endif
#endif
#if defined GL_IMAGE_2D_RECT
# if defined Image2DRect
#  pragma push_macro("Image2DRect")
#  undef Image2DRect
	 , Image2DRect(_base())
#  pragma pop_macro("Image2DRect")
# else
	 , Image2DRect(_base())
# endif
#endif
#if defined GL_IMAGE_CUBE
# if defined ImageCube
#  pragma push_macro("ImageCube")
#  undef ImageCube
	 , ImageCube(_base())
#  pragma pop_macro("ImageCube")
# else
	 , ImageCube(_base())
# endif
#endif
#if defined GL_IMAGE_BUFFER
# if defined ImageBuffer
#  pragma push_macro("ImageBuffer")
#  undef ImageBuffer
	 , ImageBuffer(_base())
#  pragma pop_macro("ImageBuffer")
# else
	 , ImageBuffer(_base())
# endif
#endif
#if defined GL_IMAGE_1D_ARRAY
# if defined Image1DArray
#  pragma push_macro("Image1DArray")
#  undef Image1DArray
	 , Image1DArray(_base())
#  pragma pop_macro("Image1DArray")
# else
	 , Image1DArray(_base())
# endif
#endif
#if defined GL_IMAGE_2D_ARRAY
# if defined Image2DArray
#  pragma push_macro("Image2DArray")
#  undef Image2DArray
	 , Image2DArray(_base())
#  pragma pop_macro("Image2DArray")
# else
	 , Image2DArray(_base())
# endif
#endif
#if defined GL_IMAGE_2D_MULTISAMPLE
# if defined Image2DMultisample
#  pragma push_macro("Image2DMultisample")
#  undef Image2DMultisample
	 , Image2DMultisample(_base())
#  pragma pop_macro("Image2DMultisample")
# else
	 , Image2DMultisample(_base())
# endif
#endif
#if defined GL_IMAGE_2D_MULTISAMPLE_ARRAY
# if defined Image2DMultisampleArray
#  pragma push_macro("Image2DMultisampleArray")
#  undef Image2DMultisampleArray
	 , Image2DMultisampleArray(_base())
#  pragma pop_macro("Image2DMultisampleArray")
# else
	 , Image2DMultisampleArray(_base())
# endif
#endif
#if defined GL_INT_IMAGE_1D
# if defined IntImage1D
#  pragma push_macro("IntImage1D")
#  undef IntImage1D
	 , IntImage1D(_base())
#  pragma pop_macro("IntImage1D")
# else
	 , IntImage1D(_base())
# endif
#endif
#if defined GL_INT_IMAGE_2D
# if defined IntImage2D
#  pragma push_macro("IntImage2D")
#  undef IntImage2D
	 , IntImage2D(_base())
#  pragma pop_macro("IntImage2D")
# else
	 , IntImage2D(_base())
# endif
#endif
#if defined GL_INT_IMAGE_3D
# if defined IntImage3D
#  pragma push_macro("IntImage3D")
#  undef IntImage3D
	 , IntImage3D(_base())
#  pragma pop_macro("IntImage3D")
# else
	 , IntImage3D(_base())
# endif
#endif
#if defined GL_INT_IMAGE_2D_RECT
# if defined IntImage2DRect
#  pragma push_macro("IntImage2DRect")
#  undef IntImage2DRect
	 , IntImage2DRect(_base())
#  pragma pop_macro("IntImage2DRect")
# else
	 , IntImage2DRect(_base())
# endif
#endif
#if defined GL_INT_IMAGE_CUBE
# if defined IntImageCube
#  pragma push_macro("IntImageCube")
#  undef IntImageCube
	 , IntImageCube(_base())
#  pragma pop_macro("IntImageCube")
# else
	 , IntImageCube(_base())
# endif
#endif
#if defined GL_INT_IMAGE_BUFFER
# if defined IntImageBuffer
#  pragma push_macro("IntImageBuffer")
#  undef IntImageBuffer
	 , IntImageBuffer(_base())
#  pragma pop_macro("IntImageBuffer")
# else
	 , IntImageBuffer(_base())
# endif
#endif
#if defined GL_INT_IMAGE_1D_ARRAY
# if defined IntImage1DArray
#  pragma push_macro("IntImage1DArray")
#  undef IntImage1DArray
	 , IntImage1DArray(_base())
#  pragma pop_macro("IntImage1DArray")
# else
	 , IntImage1DArray(_base())
# endif
#endif
#if defined GL_INT_IMAGE_2D_ARRAY
# if defined IntImage2DArray
#  pragma push_macro("IntImage2DArray")
#  undef IntImage2DArray
	 , IntImage2DArray(_base())
#  pragma pop_macro("IntImage2DArray")
# else
	 , IntImage2DArray(_base())
# endif
#endif
#if defined GL_INT_IMAGE_2D_MULTISAMPLE
# if defined IntImage2DMultisample
#  pragma push_macro("IntImage2DMultisample")
#  undef IntImage2DMultisample
	 , IntImage2DMultisample(_base())
#  pragma pop_macro("IntImage2DMultisample")
# else
	 , IntImage2DMultisample(_base())
# endif
#endif
#if defined GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY
# if defined IntImage2DMultisampleArray
#  pragma push_macro("IntImage2DMultisampleArray")
#  undef IntImage2DMultisampleArray
	 , IntImage2DMultisampleArray(_base())
#  pragma pop_macro("IntImage2DMultisampleArray")
# else
	 , IntImage2DMultisampleArray(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_1D
# if defined UnsignedIntImage1D
#  pragma push_macro("UnsignedIntImage1D")
#  undef UnsignedIntImage1D
	 , UnsignedIntImage1D(_base())
#  pragma pop_macro("UnsignedIntImage1D")
# else
	 , UnsignedIntImage1D(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_2D
# if defined UnsignedIntImage2D
#  pragma push_macro("UnsignedIntImage2D")
#  undef UnsignedIntImage2D
	 , UnsignedIntImage2D(_base())
#  pragma pop_macro("UnsignedIntImage2D")
# else
	 , UnsignedIntImage2D(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_3D
# if defined UnsignedIntImage3D
#  pragma push_macro("UnsignedIntImage3D")
#  undef UnsignedIntImage3D
	 , UnsignedIntImage3D(_base())
#  pragma pop_macro("UnsignedIntImage3D")
# else
	 , UnsignedIntImage3D(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_2D_RECT
# if defined UnsignedIntImage2DRect
#  pragma push_macro("UnsignedIntImage2DRect")
#  undef UnsignedIntImage2DRect
	 , UnsignedIntImage2DRect(_base())
#  pragma pop_macro("UnsignedIntImage2DRect")
# else
	 , UnsignedIntImage2DRect(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_CUBE
# if defined UnsignedIntImageCube
#  pragma push_macro("UnsignedIntImageCube")
#  undef UnsignedIntImageCube
	 , UnsignedIntImageCube(_base())
#  pragma pop_macro("UnsignedIntImageCube")
# else
	 , UnsignedIntImageCube(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_BUFFER
# if defined UnsignedIntImageBuffer
#  pragma push_macro("UnsignedIntImageBuffer")
#  undef UnsignedIntImageBuffer
	 , UnsignedIntImageBuffer(_base())
#  pragma pop_macro("UnsignedIntImageBuffer")
# else
	 , UnsignedIntImageBuffer(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_1D_ARRAY
# if defined UnsignedIntImage1DArray
#  pragma push_macro("UnsignedIntImage1DArray")
#  undef UnsignedIntImage1DArray
	 , UnsignedIntImage1DArray(_base())
#  pragma pop_macro("UnsignedIntImage1DArray")
# else
	 , UnsignedIntImage1DArray(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_2D_ARRAY
# if defined UnsignedIntImage2DArray
#  pragma push_macro("UnsignedIntImage2DArray")
#  undef UnsignedIntImage2DArray
	 , UnsignedIntImage2DArray(_base())
#  pragma pop_macro("UnsignedIntImage2DArray")
# else
	 , UnsignedIntImage2DArray(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE
# if defined UnsignedIntImage2DMultisample
#  pragma push_macro("UnsignedIntImage2DMultisample")
#  undef UnsignedIntImage2DMultisample
	 , UnsignedIntImage2DMultisample(_base())
#  pragma pop_macro("UnsignedIntImage2DMultisample")
# else
	 , UnsignedIntImage2DMultisample(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY
# if defined UnsignedIntImage2DMultisampleArray
#  pragma push_macro("UnsignedIntImage2DMultisampleArray")
#  undef UnsignedIntImage2DMultisampleArray
	 , UnsignedIntImage2DMultisampleArray(_base())
#  pragma pop_macro("UnsignedIntImage2DMultisampleArray")
# else
	 , UnsignedIntImage2DMultisampleArray(_base())
# endif
#endif
#if defined GL_UNSIGNED_INT_ATOMIC_COUNTER
# if defined UnsignedIntAtomicCounter
#  pragma push_macro("UnsignedIntAtomicCounter")
#  undef UnsignedIntAtomicCounter
	 , UnsignedIntAtomicCounter(_base())
#  pragma pop_macro("UnsignedIntAtomicCounter")
# else
	 , UnsignedIntAtomicCounter(_base())
# endif
#endif
#if defined GL_NONE
# if defined None
#  pragma push_macro("None")
#  undef None
	 , None(_base())
#  pragma pop_macro("None")
# else
	 , None(_base())
# endif
#endif
	{ }
};

} // namespace enums


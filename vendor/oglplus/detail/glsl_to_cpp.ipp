/*
 *  .file oglplus/detail/glsl_to_cpp.ipp
 *
 *  Automatically generated header file. DO NOT modify manually,
 *  edit 'source/enums/oglplus/sl_data_type.txt' instead.
 *
 *  Copyright 2010-2015 Matus Chochlik. Distributed under the Boost
 *  Software License, Version 1.0. (See accompanying file
 *  LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */

#ifdef GL_FLOAT
# ifdef Float
# pragma push_macro("Float")
# undef Float
template <> struct GLSL2Cpp<
	SLDataType::Float
> { typedef GLfloat Type; };
# pragma pop_macro("Float")
# else
template <> struct GLSL2Cpp<
	SLDataType::Float
> { typedef GLfloat Type; };
# endif
#endif // FLOAT

#ifdef GL_FLOAT_VEC2
# ifdef FloatVec2
# pragma push_macro("FloatVec2")
# undef FloatVec2
template <> struct GLSL2Cpp<
	SLDataType::FloatVec2
> { typedef oglplus::Vector<GLfloat, 2> Type; };
# pragma pop_macro("FloatVec2")
# else
template <> struct GLSL2Cpp<
	SLDataType::FloatVec2
> { typedef oglplus::Vector<GLfloat, 2> Type; };
# endif
#endif // FLOAT_VEC2

#ifdef GL_FLOAT_VEC3
# ifdef FloatVec3
# pragma push_macro("FloatVec3")
# undef FloatVec3
template <> struct GLSL2Cpp<
	SLDataType::FloatVec3
> { typedef oglplus::Vector<GLfloat, 3> Type; };
# pragma pop_macro("FloatVec3")
# else
template <> struct GLSL2Cpp<
	SLDataType::FloatVec3
> { typedef oglplus::Vector<GLfloat, 3> Type; };
# endif
#endif // FLOAT_VEC3

#ifdef GL_FLOAT_VEC4
# ifdef FloatVec4
# pragma push_macro("FloatVec4")
# undef FloatVec4
template <> struct GLSL2Cpp<
	SLDataType::FloatVec4
> { typedef oglplus::Vector<GLfloat, 4> Type; };
# pragma pop_macro("FloatVec4")
# else
template <> struct GLSL2Cpp<
	SLDataType::FloatVec4
> { typedef oglplus::Vector<GLfloat, 4> Type; };
# endif
#endif // FLOAT_VEC4

#ifdef GL_DOUBLE
# ifdef Double
# pragma push_macro("Double")
# undef Double
template <> struct GLSL2Cpp<
	SLDataType::Double
> { typedef GLdouble Type; };
# pragma pop_macro("Double")
# else
template <> struct GLSL2Cpp<
	SLDataType::Double
> { typedef GLdouble Type; };
# endif
#endif // DOUBLE

#ifdef GL_DOUBLE_VEC2
# ifdef DoubleVec2
# pragma push_macro("DoubleVec2")
# undef DoubleVec2
template <> struct GLSL2Cpp<
	SLDataType::DoubleVec2
> { typedef oglplus::Vector<GLdouble, 2> Type; };
# pragma pop_macro("DoubleVec2")
# else
template <> struct GLSL2Cpp<
	SLDataType::DoubleVec2
> { typedef oglplus::Vector<GLdouble, 2> Type; };
# endif
#endif // DOUBLE_VEC2

#ifdef GL_DOUBLE_VEC3
# ifdef DoubleVec3
# pragma push_macro("DoubleVec3")
# undef DoubleVec3
template <> struct GLSL2Cpp<
	SLDataType::DoubleVec3
> { typedef oglplus::Vector<GLdouble, 3> Type; };
# pragma pop_macro("DoubleVec3")
# else
template <> struct GLSL2Cpp<
	SLDataType::DoubleVec3
> { typedef oglplus::Vector<GLdouble, 3> Type; };
# endif
#endif // DOUBLE_VEC3

#ifdef GL_DOUBLE_VEC4
# ifdef DoubleVec4
# pragma push_macro("DoubleVec4")
# undef DoubleVec4
template <> struct GLSL2Cpp<
	SLDataType::DoubleVec4
> { typedef oglplus::Vector<GLdouble, 4> Type; };
# pragma pop_macro("DoubleVec4")
# else
template <> struct GLSL2Cpp<
	SLDataType::DoubleVec4
> { typedef oglplus::Vector<GLdouble, 4> Type; };
# endif
#endif // DOUBLE_VEC4

#ifdef GL_INT
# ifdef Int
# pragma push_macro("Int")
# undef Int
template <> struct GLSL2Cpp<
	SLDataType::Int
> { typedef GLint Type; };
# pragma pop_macro("Int")
# else
template <> struct GLSL2Cpp<
	SLDataType::Int
> { typedef GLint Type; };
# endif
#endif // INT

#ifdef GL_INT_VEC2
# ifdef IntVec2
# pragma push_macro("IntVec2")
# undef IntVec2
template <> struct GLSL2Cpp<
	SLDataType::IntVec2
> { typedef oglplus::Vector<GLint, 2> Type; };
# pragma pop_macro("IntVec2")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntVec2
> { typedef oglplus::Vector<GLint, 2> Type; };
# endif
#endif // INT_VEC2

#ifdef GL_INT_VEC3
# ifdef IntVec3
# pragma push_macro("IntVec3")
# undef IntVec3
template <> struct GLSL2Cpp<
	SLDataType::IntVec3
> { typedef oglplus::Vector<GLint, 3> Type; };
# pragma pop_macro("IntVec3")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntVec3
> { typedef oglplus::Vector<GLint, 3> Type; };
# endif
#endif // INT_VEC3

#ifdef GL_INT_VEC4
# ifdef IntVec4
# pragma push_macro("IntVec4")
# undef IntVec4
template <> struct GLSL2Cpp<
	SLDataType::IntVec4
> { typedef oglplus::Vector<GLint, 4> Type; };
# pragma pop_macro("IntVec4")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntVec4
> { typedef oglplus::Vector<GLint, 4> Type; };
# endif
#endif // INT_VEC4

#ifdef GL_UNSIGNED_INT
# ifdef UnsignedInt
# pragma push_macro("UnsignedInt")
# undef UnsignedInt
template <> struct GLSL2Cpp<
	SLDataType::UnsignedInt
> { typedef GLuint Type; };
# pragma pop_macro("UnsignedInt")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedInt
> { typedef GLuint Type; };
# endif
#endif // UNSIGNED_INT

#ifdef GL_UNSIGNED_INT_VEC2
# ifdef UnsignedIntVec2
# pragma push_macro("UnsignedIntVec2")
# undef UnsignedIntVec2
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntVec2
> { typedef oglplus::Vector<GLuint, 2> Type; };
# pragma pop_macro("UnsignedIntVec2")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntVec2
> { typedef oglplus::Vector<GLuint, 2> Type; };
# endif
#endif // UNSIGNED_INT_VEC2

#ifdef GL_UNSIGNED_INT_VEC3
# ifdef UnsignedIntVec3
# pragma push_macro("UnsignedIntVec3")
# undef UnsignedIntVec3
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntVec3
> { typedef oglplus::Vector<GLuint, 3> Type; };
# pragma pop_macro("UnsignedIntVec3")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntVec3
> { typedef oglplus::Vector<GLuint, 3> Type; };
# endif
#endif // UNSIGNED_INT_VEC3

#ifdef GL_UNSIGNED_INT_VEC4
# ifdef UnsignedIntVec4
# pragma push_macro("UnsignedIntVec4")
# undef UnsignedIntVec4
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntVec4
> { typedef oglplus::Vector<GLuint, 4> Type; };
# pragma pop_macro("UnsignedIntVec4")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntVec4
> { typedef oglplus::Vector<GLuint, 4> Type; };
# endif
#endif // UNSIGNED_INT_VEC4

#ifdef GL_BOOL
# ifdef Bool
# pragma push_macro("Bool")
# undef Bool
template <> struct GLSL2Cpp<
	SLDataType::Bool
> { typedef GLboolean Type; };
# pragma pop_macro("Bool")
# else
template <> struct GLSL2Cpp<
	SLDataType::Bool
> { typedef GLboolean Type; };
# endif
#endif // BOOL

#ifdef GL_BOOL_VEC2
# ifdef BoolVec2
# pragma push_macro("BoolVec2")
# undef BoolVec2
template <> struct GLSL2Cpp<
	SLDataType::BoolVec2
> { typedef oglplus::Vector<GLboolean, 2> Type; };
# pragma pop_macro("BoolVec2")
# else
template <> struct GLSL2Cpp<
	SLDataType::BoolVec2
> { typedef oglplus::Vector<GLboolean, 2> Type; };
# endif
#endif // BOOL_VEC2

#ifdef GL_BOOL_VEC3
# ifdef BoolVec3
# pragma push_macro("BoolVec3")
# undef BoolVec3
template <> struct GLSL2Cpp<
	SLDataType::BoolVec3
> { typedef oglplus::Vector<GLboolean, 3> Type; };
# pragma pop_macro("BoolVec3")
# else
template <> struct GLSL2Cpp<
	SLDataType::BoolVec3
> { typedef oglplus::Vector<GLboolean, 3> Type; };
# endif
#endif // BOOL_VEC3

#ifdef GL_BOOL_VEC4
# ifdef BoolVec4
# pragma push_macro("BoolVec4")
# undef BoolVec4
template <> struct GLSL2Cpp<
	SLDataType::BoolVec4
> { typedef oglplus::Vector<GLboolean, 4> Type; };
# pragma pop_macro("BoolVec4")
# else
template <> struct GLSL2Cpp<
	SLDataType::BoolVec4
> { typedef oglplus::Vector<GLboolean, 4> Type; };
# endif
#endif // BOOL_VEC4

#ifdef GL_FLOAT_MAT2
# ifdef FloatMat2
# pragma push_macro("FloatMat2")
# undef FloatMat2
template <> struct GLSL2Cpp<
	SLDataType::FloatMat2
> { typedef oglplus::Matrix<GLfloat, 2, 2> Type; };
# pragma pop_macro("FloatMat2")
# else
template <> struct GLSL2Cpp<
	SLDataType::FloatMat2
> { typedef oglplus::Matrix<GLfloat, 2, 2> Type; };
# endif
#endif // FLOAT_MAT2

#ifdef GL_FLOAT_MAT3
# ifdef FloatMat3
# pragma push_macro("FloatMat3")
# undef FloatMat3
template <> struct GLSL2Cpp<
	SLDataType::FloatMat3
> { typedef oglplus::Matrix<GLfloat, 3, 3> Type; };
# pragma pop_macro("FloatMat3")
# else
template <> struct GLSL2Cpp<
	SLDataType::FloatMat3
> { typedef oglplus::Matrix<GLfloat, 3, 3> Type; };
# endif
#endif // FLOAT_MAT3

#ifdef GL_FLOAT_MAT4
# ifdef FloatMat4
# pragma push_macro("FloatMat4")
# undef FloatMat4
template <> struct GLSL2Cpp<
	SLDataType::FloatMat4
> { typedef oglplus::Matrix<GLfloat, 4, 4> Type; };
# pragma pop_macro("FloatMat4")
# else
template <> struct GLSL2Cpp<
	SLDataType::FloatMat4
> { typedef oglplus::Matrix<GLfloat, 4, 4> Type; };
# endif
#endif // FLOAT_MAT4

#ifdef GL_FLOAT_MAT2x3
# ifdef FloatMat2x3
# pragma push_macro("FloatMat2x3")
# undef FloatMat2x3
template <> struct GLSL2Cpp<
	SLDataType::FloatMat2x3
> { typedef oglplus::Matrix<GLfloat, 3, 2> Type; };
# pragma pop_macro("FloatMat2x3")
# else
template <> struct GLSL2Cpp<
	SLDataType::FloatMat2x3
> { typedef oglplus::Matrix<GLfloat, 3, 2> Type; };
# endif
#endif // FLOAT_MAT2x3

#ifdef GL_FLOAT_MAT2x4
# ifdef FloatMat2x4
# pragma push_macro("FloatMat2x4")
# undef FloatMat2x4
template <> struct GLSL2Cpp<
	SLDataType::FloatMat2x4
> { typedef oglplus::Matrix<GLfloat, 4, 2> Type; };
# pragma pop_macro("FloatMat2x4")
# else
template <> struct GLSL2Cpp<
	SLDataType::FloatMat2x4
> { typedef oglplus::Matrix<GLfloat, 4, 2> Type; };
# endif
#endif // FLOAT_MAT2x4

#ifdef GL_FLOAT_MAT3x2
# ifdef FloatMat3x2
# pragma push_macro("FloatMat3x2")
# undef FloatMat3x2
template <> struct GLSL2Cpp<
	SLDataType::FloatMat3x2
> { typedef oglplus::Matrix<GLfloat, 2, 3> Type; };
# pragma pop_macro("FloatMat3x2")
# else
template <> struct GLSL2Cpp<
	SLDataType::FloatMat3x2
> { typedef oglplus::Matrix<GLfloat, 2, 3> Type; };
# endif
#endif // FLOAT_MAT3x2

#ifdef GL_FLOAT_MAT3x4
# ifdef FloatMat3x4
# pragma push_macro("FloatMat3x4")
# undef FloatMat3x4
template <> struct GLSL2Cpp<
	SLDataType::FloatMat3x4
> { typedef oglplus::Matrix<GLfloat, 4, 3> Type; };
# pragma pop_macro("FloatMat3x4")
# else
template <> struct GLSL2Cpp<
	SLDataType::FloatMat3x4
> { typedef oglplus::Matrix<GLfloat, 4, 3> Type; };
# endif
#endif // FLOAT_MAT3x4

#ifdef GL_FLOAT_MAT4x2
# ifdef FloatMat4x2
# pragma push_macro("FloatMat4x2")
# undef FloatMat4x2
template <> struct GLSL2Cpp<
	SLDataType::FloatMat4x2
> { typedef oglplus::Matrix<GLfloat, 2, 4> Type; };
# pragma pop_macro("FloatMat4x2")
# else
template <> struct GLSL2Cpp<
	SLDataType::FloatMat4x2
> { typedef oglplus::Matrix<GLfloat, 2, 4> Type; };
# endif
#endif // FLOAT_MAT4x2

#ifdef GL_FLOAT_MAT4x3
# ifdef FloatMat4x3
# pragma push_macro("FloatMat4x3")
# undef FloatMat4x3
template <> struct GLSL2Cpp<
	SLDataType::FloatMat4x3
> { typedef oglplus::Matrix<GLfloat, 3, 4> Type; };
# pragma pop_macro("FloatMat4x3")
# else
template <> struct GLSL2Cpp<
	SLDataType::FloatMat4x3
> { typedef oglplus::Matrix<GLfloat, 3, 4> Type; };
# endif
#endif // FLOAT_MAT4x3

#ifdef GL_DOUBLE_MAT2
# ifdef DoubleMat2
# pragma push_macro("DoubleMat2")
# undef DoubleMat2
template <> struct GLSL2Cpp<
	SLDataType::DoubleMat2
> { typedef oglplus::Matrix<GLdouble, 2, 2> Type; };
# pragma pop_macro("DoubleMat2")
# else
template <> struct GLSL2Cpp<
	SLDataType::DoubleMat2
> { typedef oglplus::Matrix<GLdouble, 2, 2> Type; };
# endif
#endif // DOUBLE_MAT2

#ifdef GL_DOUBLE_MAT3
# ifdef DoubleMat3
# pragma push_macro("DoubleMat3")
# undef DoubleMat3
template <> struct GLSL2Cpp<
	SLDataType::DoubleMat3
> { typedef oglplus::Matrix<GLdouble, 3, 3> Type; };
# pragma pop_macro("DoubleMat3")
# else
template <> struct GLSL2Cpp<
	SLDataType::DoubleMat3
> { typedef oglplus::Matrix<GLdouble, 3, 3> Type; };
# endif
#endif // DOUBLE_MAT3

#ifdef GL_DOUBLE_MAT4
# ifdef DoubleMat4
# pragma push_macro("DoubleMat4")
# undef DoubleMat4
template <> struct GLSL2Cpp<
	SLDataType::DoubleMat4
> { typedef oglplus::Matrix<GLdouble, 4, 4> Type; };
# pragma pop_macro("DoubleMat4")
# else
template <> struct GLSL2Cpp<
	SLDataType::DoubleMat4
> { typedef oglplus::Matrix<GLdouble, 4, 4> Type; };
# endif
#endif // DOUBLE_MAT4

#ifdef GL_DOUBLE_MAT2x3
# ifdef DoubleMat2x3
# pragma push_macro("DoubleMat2x3")
# undef DoubleMat2x3
template <> struct GLSL2Cpp<
	SLDataType::DoubleMat2x3
> { typedef oglplus::Matrix<GLdouble, 3, 2> Type; };
# pragma pop_macro("DoubleMat2x3")
# else
template <> struct GLSL2Cpp<
	SLDataType::DoubleMat2x3
> { typedef oglplus::Matrix<GLdouble, 3, 2> Type; };
# endif
#endif // DOUBLE_MAT2x3

#ifdef GL_DOUBLE_MAT2x4
# ifdef DoubleMat2x4
# pragma push_macro("DoubleMat2x4")
# undef DoubleMat2x4
template <> struct GLSL2Cpp<
	SLDataType::DoubleMat2x4
> { typedef oglplus::Matrix<GLdouble, 4, 2> Type; };
# pragma pop_macro("DoubleMat2x4")
# else
template <> struct GLSL2Cpp<
	SLDataType::DoubleMat2x4
> { typedef oglplus::Matrix<GLdouble, 4, 2> Type; };
# endif
#endif // DOUBLE_MAT2x4

#ifdef GL_DOUBLE_MAT3x2
# ifdef DoubleMat3x2
# pragma push_macro("DoubleMat3x2")
# undef DoubleMat3x2
template <> struct GLSL2Cpp<
	SLDataType::DoubleMat3x2
> { typedef oglplus::Matrix<GLdouble, 2, 3> Type; };
# pragma pop_macro("DoubleMat3x2")
# else
template <> struct GLSL2Cpp<
	SLDataType::DoubleMat3x2
> { typedef oglplus::Matrix<GLdouble, 2, 3> Type; };
# endif
#endif // DOUBLE_MAT3x2

#ifdef GL_DOUBLE_MAT3x4
# ifdef DoubleMat3x4
# pragma push_macro("DoubleMat3x4")
# undef DoubleMat3x4
template <> struct GLSL2Cpp<
	SLDataType::DoubleMat3x4
> { typedef oglplus::Matrix<GLdouble, 4, 3> Type; };
# pragma pop_macro("DoubleMat3x4")
# else
template <> struct GLSL2Cpp<
	SLDataType::DoubleMat3x4
> { typedef oglplus::Matrix<GLdouble, 4, 3> Type; };
# endif
#endif // DOUBLE_MAT3x4

#ifdef GL_DOUBLE_MAT4x2
# ifdef DoubleMat4x2
# pragma push_macro("DoubleMat4x2")
# undef DoubleMat4x2
template <> struct GLSL2Cpp<
	SLDataType::DoubleMat4x2
> { typedef oglplus::Matrix<GLdouble, 2, 4> Type; };
# pragma pop_macro("DoubleMat4x2")
# else
template <> struct GLSL2Cpp<
	SLDataType::DoubleMat4x2
> { typedef oglplus::Matrix<GLdouble, 2, 4> Type; };
# endif
#endif // DOUBLE_MAT4x2

#ifdef GL_DOUBLE_MAT4x3
# ifdef DoubleMat4x3
# pragma push_macro("DoubleMat4x3")
# undef DoubleMat4x3
template <> struct GLSL2Cpp<
	SLDataType::DoubleMat4x3
> { typedef oglplus::Matrix<GLdouble, 3, 4> Type; };
# pragma pop_macro("DoubleMat4x3")
# else
template <> struct GLSL2Cpp<
	SLDataType::DoubleMat4x3
> { typedef oglplus::Matrix<GLdouble, 3, 4> Type; };
# endif
#endif // DOUBLE_MAT4x3

#ifdef GL_SAMPLER_1D
# ifdef Sampler1D
# pragma push_macro("Sampler1D")
# undef Sampler1D
template <> struct GLSL2Cpp<
	SLDataType::Sampler1D
> { typedef GLint Type; };
# pragma pop_macro("Sampler1D")
# else
template <> struct GLSL2Cpp<
	SLDataType::Sampler1D
> { typedef GLint Type; };
# endif
#endif // SAMPLER_1D

#ifdef GL_SAMPLER_2D
# ifdef Sampler2D
# pragma push_macro("Sampler2D")
# undef Sampler2D
template <> struct GLSL2Cpp<
	SLDataType::Sampler2D
> { typedef GLint Type; };
# pragma pop_macro("Sampler2D")
# else
template <> struct GLSL2Cpp<
	SLDataType::Sampler2D
> { typedef GLint Type; };
# endif
#endif // SAMPLER_2D

#ifdef GL_SAMPLER_3D
# ifdef Sampler3D
# pragma push_macro("Sampler3D")
# undef Sampler3D
template <> struct GLSL2Cpp<
	SLDataType::Sampler3D
> { typedef GLint Type; };
# pragma pop_macro("Sampler3D")
# else
template <> struct GLSL2Cpp<
	SLDataType::Sampler3D
> { typedef GLint Type; };
# endif
#endif // SAMPLER_3D

#ifdef GL_SAMPLER_CUBE
# ifdef SamplerCube
# pragma push_macro("SamplerCube")
# undef SamplerCube
template <> struct GLSL2Cpp<
	SLDataType::SamplerCube
> { typedef GLint Type; };
# pragma pop_macro("SamplerCube")
# else
template <> struct GLSL2Cpp<
	SLDataType::SamplerCube
> { typedef GLint Type; };
# endif
#endif // SAMPLER_CUBE

#ifdef GL_SAMPLER_1D_SHADOW
# ifdef Sampler1DShadow
# pragma push_macro("Sampler1DShadow")
# undef Sampler1DShadow
template <> struct GLSL2Cpp<
	SLDataType::Sampler1DShadow
> { typedef GLint Type; };
# pragma pop_macro("Sampler1DShadow")
# else
template <> struct GLSL2Cpp<
	SLDataType::Sampler1DShadow
> { typedef GLint Type; };
# endif
#endif // SAMPLER_1D_SHADOW

#ifdef GL_SAMPLER_2D_SHADOW
# ifdef Sampler2DShadow
# pragma push_macro("Sampler2DShadow")
# undef Sampler2DShadow
template <> struct GLSL2Cpp<
	SLDataType::Sampler2DShadow
> { typedef GLint Type; };
# pragma pop_macro("Sampler2DShadow")
# else
template <> struct GLSL2Cpp<
	SLDataType::Sampler2DShadow
> { typedef GLint Type; };
# endif
#endif // SAMPLER_2D_SHADOW

#ifdef GL_SAMPLER_1D_ARRAY
# ifdef Sampler1DArray
# pragma push_macro("Sampler1DArray")
# undef Sampler1DArray
template <> struct GLSL2Cpp<
	SLDataType::Sampler1DArray
> { typedef GLint Type; };
# pragma pop_macro("Sampler1DArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::Sampler1DArray
> { typedef GLint Type; };
# endif
#endif // SAMPLER_1D_ARRAY

#ifdef GL_SAMPLER_2D_ARRAY
# ifdef Sampler2DArray
# pragma push_macro("Sampler2DArray")
# undef Sampler2DArray
template <> struct GLSL2Cpp<
	SLDataType::Sampler2DArray
> { typedef GLint Type; };
# pragma pop_macro("Sampler2DArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::Sampler2DArray
> { typedef GLint Type; };
# endif
#endif // SAMPLER_2D_ARRAY

#ifdef GL_SAMPLER_CUBE_MAP_ARRAY
# ifdef SamplerCubeMapArray
# pragma push_macro("SamplerCubeMapArray")
# undef SamplerCubeMapArray
template <> struct GLSL2Cpp<
	SLDataType::SamplerCubeMapArray
> { typedef GLint Type; };
# pragma pop_macro("SamplerCubeMapArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::SamplerCubeMapArray
> { typedef GLint Type; };
# endif
#endif // SAMPLER_CUBE_MAP_ARRAY

#ifdef GL_SAMPLER_1D_ARRAY_SHADOW
# ifdef Sampler1DArrayShadow
# pragma push_macro("Sampler1DArrayShadow")
# undef Sampler1DArrayShadow
template <> struct GLSL2Cpp<
	SLDataType::Sampler1DArrayShadow
> { typedef GLint Type; };
# pragma pop_macro("Sampler1DArrayShadow")
# else
template <> struct GLSL2Cpp<
	SLDataType::Sampler1DArrayShadow
> { typedef GLint Type; };
# endif
#endif // SAMPLER_1D_ARRAY_SHADOW

#ifdef GL_SAMPLER_2D_ARRAY_SHADOW
# ifdef Sampler2DArrayShadow
# pragma push_macro("Sampler2DArrayShadow")
# undef Sampler2DArrayShadow
template <> struct GLSL2Cpp<
	SLDataType::Sampler2DArrayShadow
> { typedef GLint Type; };
# pragma pop_macro("Sampler2DArrayShadow")
# else
template <> struct GLSL2Cpp<
	SLDataType::Sampler2DArrayShadow
> { typedef GLint Type; };
# endif
#endif // SAMPLER_2D_ARRAY_SHADOW

#ifdef GL_SAMPLER_2D_MULTISAMPLE
# ifdef Sampler2DMultisample
# pragma push_macro("Sampler2DMultisample")
# undef Sampler2DMultisample
template <> struct GLSL2Cpp<
	SLDataType::Sampler2DMultisample
> { typedef GLint Type; };
# pragma pop_macro("Sampler2DMultisample")
# else
template <> struct GLSL2Cpp<
	SLDataType::Sampler2DMultisample
> { typedef GLint Type; };
# endif
#endif // SAMPLER_2D_MULTISAMPLE

#ifdef GL_SAMPLER_2D_MULTISAMPLE_ARRAY
# ifdef Sampler2DMultisampleArray
# pragma push_macro("Sampler2DMultisampleArray")
# undef Sampler2DMultisampleArray
template <> struct GLSL2Cpp<
	SLDataType::Sampler2DMultisampleArray
> { typedef GLint Type; };
# pragma pop_macro("Sampler2DMultisampleArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::Sampler2DMultisampleArray
> { typedef GLint Type; };
# endif
#endif // SAMPLER_2D_MULTISAMPLE_ARRAY

#ifdef GL_SAMPLER_CUBE_SHADOW
# ifdef SamplerCubeShadow
# pragma push_macro("SamplerCubeShadow")
# undef SamplerCubeShadow
template <> struct GLSL2Cpp<
	SLDataType::SamplerCubeShadow
> { typedef GLint Type; };
# pragma pop_macro("SamplerCubeShadow")
# else
template <> struct GLSL2Cpp<
	SLDataType::SamplerCubeShadow
> { typedef GLint Type; };
# endif
#endif // SAMPLER_CUBE_SHADOW

#ifdef GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW
# ifdef SamplerCubeMapArrayShadow
# pragma push_macro("SamplerCubeMapArrayShadow")
# undef SamplerCubeMapArrayShadow
template <> struct GLSL2Cpp<
	SLDataType::SamplerCubeMapArrayShadow
> { typedef GLint Type; };
# pragma pop_macro("SamplerCubeMapArrayShadow")
# else
template <> struct GLSL2Cpp<
	SLDataType::SamplerCubeMapArrayShadow
> { typedef GLint Type; };
# endif
#endif // SAMPLER_CUBE_MAP_ARRAY_SHADOW

#ifdef GL_SAMPLER_BUFFER
# ifdef SamplerBuffer
# pragma push_macro("SamplerBuffer")
# undef SamplerBuffer
template <> struct GLSL2Cpp<
	SLDataType::SamplerBuffer
> { typedef GLint Type; };
# pragma pop_macro("SamplerBuffer")
# else
template <> struct GLSL2Cpp<
	SLDataType::SamplerBuffer
> { typedef GLint Type; };
# endif
#endif // SAMPLER_BUFFER

#ifdef GL_SAMPLER_2D_RECT
# ifdef Sampler2DRect
# pragma push_macro("Sampler2DRect")
# undef Sampler2DRect
template <> struct GLSL2Cpp<
	SLDataType::Sampler2DRect
> { typedef GLint Type; };
# pragma pop_macro("Sampler2DRect")
# else
template <> struct GLSL2Cpp<
	SLDataType::Sampler2DRect
> { typedef GLint Type; };
# endif
#endif // SAMPLER_2D_RECT

#ifdef GL_SAMPLER_2D_RECT_SHADOW
# ifdef Sampler2DRectShadow
# pragma push_macro("Sampler2DRectShadow")
# undef Sampler2DRectShadow
template <> struct GLSL2Cpp<
	SLDataType::Sampler2DRectShadow
> { typedef GLint Type; };
# pragma pop_macro("Sampler2DRectShadow")
# else
template <> struct GLSL2Cpp<
	SLDataType::Sampler2DRectShadow
> { typedef GLint Type; };
# endif
#endif // SAMPLER_2D_RECT_SHADOW

#ifdef GL_INT_SAMPLER_1D
# ifdef IntSampler1D
# pragma push_macro("IntSampler1D")
# undef IntSampler1D
template <> struct GLSL2Cpp<
	SLDataType::IntSampler1D
> { typedef GLint Type; };
# pragma pop_macro("IntSampler1D")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntSampler1D
> { typedef GLint Type; };
# endif
#endif // INT_SAMPLER_1D

#ifdef GL_INT_SAMPLER_2D
# ifdef IntSampler2D
# pragma push_macro("IntSampler2D")
# undef IntSampler2D
template <> struct GLSL2Cpp<
	SLDataType::IntSampler2D
> { typedef GLint Type; };
# pragma pop_macro("IntSampler2D")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntSampler2D
> { typedef GLint Type; };
# endif
#endif // INT_SAMPLER_2D

#ifdef GL_INT_SAMPLER_3D
# ifdef IntSampler3D
# pragma push_macro("IntSampler3D")
# undef IntSampler3D
template <> struct GLSL2Cpp<
	SLDataType::IntSampler3D
> { typedef GLint Type; };
# pragma pop_macro("IntSampler3D")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntSampler3D
> { typedef GLint Type; };
# endif
#endif // INT_SAMPLER_3D

#ifdef GL_INT_SAMPLER_CUBE
# ifdef IntSamplerCube
# pragma push_macro("IntSamplerCube")
# undef IntSamplerCube
template <> struct GLSL2Cpp<
	SLDataType::IntSamplerCube
> { typedef GLint Type; };
# pragma pop_macro("IntSamplerCube")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntSamplerCube
> { typedef GLint Type; };
# endif
#endif // INT_SAMPLER_CUBE

#ifdef GL_INT_SAMPLER_1D_ARRAY
# ifdef IntSampler1DArray
# pragma push_macro("IntSampler1DArray")
# undef IntSampler1DArray
template <> struct GLSL2Cpp<
	SLDataType::IntSampler1DArray
> { typedef GLint Type; };
# pragma pop_macro("IntSampler1DArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntSampler1DArray
> { typedef GLint Type; };
# endif
#endif // INT_SAMPLER_1D_ARRAY

#ifdef GL_INT_SAMPLER_2D_ARRAY
# ifdef IntSampler2DArray
# pragma push_macro("IntSampler2DArray")
# undef IntSampler2DArray
template <> struct GLSL2Cpp<
	SLDataType::IntSampler2DArray
> { typedef GLint Type; };
# pragma pop_macro("IntSampler2DArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntSampler2DArray
> { typedef GLint Type; };
# endif
#endif // INT_SAMPLER_2D_ARRAY

#ifdef GL_INT_SAMPLER_CUBE_MAP_ARRAY
# ifdef IntSamplerCubeMapArray
# pragma push_macro("IntSamplerCubeMapArray")
# undef IntSamplerCubeMapArray
template <> struct GLSL2Cpp<
	SLDataType::IntSamplerCubeMapArray
> { typedef GLint Type; };
# pragma pop_macro("IntSamplerCubeMapArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntSamplerCubeMapArray
> { typedef GLint Type; };
# endif
#endif // INT_SAMPLER_CUBE_MAP_ARRAY

#ifdef GL_INT_SAMPLER_2D_MULTISAMPLE
# ifdef IntSampler2DMultisample
# pragma push_macro("IntSampler2DMultisample")
# undef IntSampler2DMultisample
template <> struct GLSL2Cpp<
	SLDataType::IntSampler2DMultisample
> { typedef GLint Type; };
# pragma pop_macro("IntSampler2DMultisample")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntSampler2DMultisample
> { typedef GLint Type; };
# endif
#endif // INT_SAMPLER_2D_MULTISAMPLE

#ifdef GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
# ifdef IntSampler2DMultisampleArray
# pragma push_macro("IntSampler2DMultisampleArray")
# undef IntSampler2DMultisampleArray
template <> struct GLSL2Cpp<
	SLDataType::IntSampler2DMultisampleArray
> { typedef GLint Type; };
# pragma pop_macro("IntSampler2DMultisampleArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntSampler2DMultisampleArray
> { typedef GLint Type; };
# endif
#endif // INT_SAMPLER_2D_MULTISAMPLE_ARRAY

#ifdef GL_INT_SAMPLER_BUFFER
# ifdef IntSamplerBuffer
# pragma push_macro("IntSamplerBuffer")
# undef IntSamplerBuffer
template <> struct GLSL2Cpp<
	SLDataType::IntSamplerBuffer
> { typedef GLint Type; };
# pragma pop_macro("IntSamplerBuffer")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntSamplerBuffer
> { typedef GLint Type; };
# endif
#endif // INT_SAMPLER_BUFFER

#ifdef GL_INT_SAMPLER_2D_RECT
# ifdef IntSampler2DRect
# pragma push_macro("IntSampler2DRect")
# undef IntSampler2DRect
template <> struct GLSL2Cpp<
	SLDataType::IntSampler2DRect
> { typedef GLint Type; };
# pragma pop_macro("IntSampler2DRect")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntSampler2DRect
> { typedef GLint Type; };
# endif
#endif // INT_SAMPLER_2D_RECT

#ifdef GL_UNSIGNED_INT_SAMPLER_1D
# ifdef UnsignedIntSampler1D
# pragma push_macro("UnsignedIntSampler1D")
# undef UnsignedIntSampler1D
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSampler1D
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntSampler1D")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSampler1D
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_SAMPLER_1D

#ifdef GL_UNSIGNED_INT_SAMPLER_2D
# ifdef UnsignedIntSampler2D
# pragma push_macro("UnsignedIntSampler2D")
# undef UnsignedIntSampler2D
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSampler2D
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntSampler2D")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSampler2D
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_SAMPLER_2D

#ifdef GL_UNSIGNED_INT_SAMPLER_3D
# ifdef UnsignedIntSampler3D
# pragma push_macro("UnsignedIntSampler3D")
# undef UnsignedIntSampler3D
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSampler3D
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntSampler3D")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSampler3D
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_SAMPLER_3D

#ifdef GL_UNSIGNED_INT_SAMPLER_CUBE
# ifdef UnsignedIntSamplerCube
# pragma push_macro("UnsignedIntSamplerCube")
# undef UnsignedIntSamplerCube
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSamplerCube
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntSamplerCube")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSamplerCube
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_SAMPLER_CUBE

#ifdef GL_UNSIGNED_INT_SAMPLER_1D_ARRAY
# ifdef UnsignedIntSampler1DArray
# pragma push_macro("UnsignedIntSampler1DArray")
# undef UnsignedIntSampler1DArray
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSampler1DArray
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntSampler1DArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSampler1DArray
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_SAMPLER_1D_ARRAY

#ifdef GL_UNSIGNED_INT_SAMPLER_2D_ARRAY
# ifdef UnsignedIntSampler2DArray
# pragma push_macro("UnsignedIntSampler2DArray")
# undef UnsignedIntSampler2DArray
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSampler2DArray
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntSampler2DArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSampler2DArray
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_SAMPLER_2D_ARRAY

#ifdef GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY
# ifdef UnsignedIntSamplerCubeMapArray
# pragma push_macro("UnsignedIntSamplerCubeMapArray")
# undef UnsignedIntSamplerCubeMapArray
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSamplerCubeMapArray
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntSamplerCubeMapArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSamplerCubeMapArray
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY

#ifdef GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE
# ifdef UnsignedIntSampler2DMultisample
# pragma push_macro("UnsignedIntSampler2DMultisample")
# undef UnsignedIntSampler2DMultisample
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSampler2DMultisample
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntSampler2DMultisample")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSampler2DMultisample
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE

#ifdef GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY
# ifdef UnsignedIntSampler2DMultisampleArray
# pragma push_macro("UnsignedIntSampler2DMultisampleArray")
# undef UnsignedIntSampler2DMultisampleArray
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSampler2DMultisampleArray
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntSampler2DMultisampleArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSampler2DMultisampleArray
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY

#ifdef GL_UNSIGNED_INT_SAMPLER_BUFFER
# ifdef UnsignedIntSamplerBuffer
# pragma push_macro("UnsignedIntSamplerBuffer")
# undef UnsignedIntSamplerBuffer
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSamplerBuffer
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntSamplerBuffer")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSamplerBuffer
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_SAMPLER_BUFFER

#ifdef GL_UNSIGNED_INT_SAMPLER_2D_RECT
# ifdef UnsignedIntSampler2DRect
# pragma push_macro("UnsignedIntSampler2DRect")
# undef UnsignedIntSampler2DRect
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSampler2DRect
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntSampler2DRect")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntSampler2DRect
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_SAMPLER_2D_RECT

#ifdef GL_IMAGE_1D
# ifdef Image1D
# pragma push_macro("Image1D")
# undef Image1D
template <> struct GLSL2Cpp<
	SLDataType::Image1D
> { typedef GLint Type; };
# pragma pop_macro("Image1D")
# else
template <> struct GLSL2Cpp<
	SLDataType::Image1D
> { typedef GLint Type; };
# endif
#endif // IMAGE_1D

#ifdef GL_IMAGE_2D
# ifdef Image2D
# pragma push_macro("Image2D")
# undef Image2D
template <> struct GLSL2Cpp<
	SLDataType::Image2D
> { typedef GLint Type; };
# pragma pop_macro("Image2D")
# else
template <> struct GLSL2Cpp<
	SLDataType::Image2D
> { typedef GLint Type; };
# endif
#endif // IMAGE_2D

#ifdef GL_IMAGE_3D
# ifdef Image3D
# pragma push_macro("Image3D")
# undef Image3D
template <> struct GLSL2Cpp<
	SLDataType::Image3D
> { typedef GLint Type; };
# pragma pop_macro("Image3D")
# else
template <> struct GLSL2Cpp<
	SLDataType::Image3D
> { typedef GLint Type; };
# endif
#endif // IMAGE_3D

#ifdef GL_IMAGE_2D_RECT
# ifdef Image2DRect
# pragma push_macro("Image2DRect")
# undef Image2DRect
template <> struct GLSL2Cpp<
	SLDataType::Image2DRect
> { typedef GLint Type; };
# pragma pop_macro("Image2DRect")
# else
template <> struct GLSL2Cpp<
	SLDataType::Image2DRect
> { typedef GLint Type; };
# endif
#endif // IMAGE_2D_RECT

#ifdef GL_IMAGE_CUBE
# ifdef ImageCube
# pragma push_macro("ImageCube")
# undef ImageCube
template <> struct GLSL2Cpp<
	SLDataType::ImageCube
> { typedef GLint Type; };
# pragma pop_macro("ImageCube")
# else
template <> struct GLSL2Cpp<
	SLDataType::ImageCube
> { typedef GLint Type; };
# endif
#endif // IMAGE_CUBE

#ifdef GL_IMAGE_BUFFER
# ifdef ImageBuffer
# pragma push_macro("ImageBuffer")
# undef ImageBuffer
template <> struct GLSL2Cpp<
	SLDataType::ImageBuffer
> { typedef GLint Type; };
# pragma pop_macro("ImageBuffer")
# else
template <> struct GLSL2Cpp<
	SLDataType::ImageBuffer
> { typedef GLint Type; };
# endif
#endif // IMAGE_BUFFER

#ifdef GL_IMAGE_1D_ARRAY
# ifdef Image1DArray
# pragma push_macro("Image1DArray")
# undef Image1DArray
template <> struct GLSL2Cpp<
	SLDataType::Image1DArray
> { typedef GLint Type; };
# pragma pop_macro("Image1DArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::Image1DArray
> { typedef GLint Type; };
# endif
#endif // IMAGE_1D_ARRAY

#ifdef GL_IMAGE_2D_ARRAY
# ifdef Image2DArray
# pragma push_macro("Image2DArray")
# undef Image2DArray
template <> struct GLSL2Cpp<
	SLDataType::Image2DArray
> { typedef GLint Type; };
# pragma pop_macro("Image2DArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::Image2DArray
> { typedef GLint Type; };
# endif
#endif // IMAGE_2D_ARRAY

#ifdef GL_IMAGE_2D_MULTISAMPLE
# ifdef Image2DMultisample
# pragma push_macro("Image2DMultisample")
# undef Image2DMultisample
template <> struct GLSL2Cpp<
	SLDataType::Image2DMultisample
> { typedef GLint Type; };
# pragma pop_macro("Image2DMultisample")
# else
template <> struct GLSL2Cpp<
	SLDataType::Image2DMultisample
> { typedef GLint Type; };
# endif
#endif // IMAGE_2D_MULTISAMPLE

#ifdef GL_IMAGE_2D_MULTISAMPLE_ARRAY
# ifdef Image2DMultisampleArray
# pragma push_macro("Image2DMultisampleArray")
# undef Image2DMultisampleArray
template <> struct GLSL2Cpp<
	SLDataType::Image2DMultisampleArray
> { typedef GLint Type; };
# pragma pop_macro("Image2DMultisampleArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::Image2DMultisampleArray
> { typedef GLint Type; };
# endif
#endif // IMAGE_2D_MULTISAMPLE_ARRAY

#ifdef GL_INT_IMAGE_1D
# ifdef IntImage1D
# pragma push_macro("IntImage1D")
# undef IntImage1D
template <> struct GLSL2Cpp<
	SLDataType::IntImage1D
> { typedef GLint Type; };
# pragma pop_macro("IntImage1D")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntImage1D
> { typedef GLint Type; };
# endif
#endif // INT_IMAGE_1D

#ifdef GL_INT_IMAGE_2D
# ifdef IntImage2D
# pragma push_macro("IntImage2D")
# undef IntImage2D
template <> struct GLSL2Cpp<
	SLDataType::IntImage2D
> { typedef GLint Type; };
# pragma pop_macro("IntImage2D")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntImage2D
> { typedef GLint Type; };
# endif
#endif // INT_IMAGE_2D

#ifdef GL_INT_IMAGE_3D
# ifdef IntImage3D
# pragma push_macro("IntImage3D")
# undef IntImage3D
template <> struct GLSL2Cpp<
	SLDataType::IntImage3D
> { typedef GLint Type; };
# pragma pop_macro("IntImage3D")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntImage3D
> { typedef GLint Type; };
# endif
#endif // INT_IMAGE_3D

#ifdef GL_INT_IMAGE_2D_RECT
# ifdef IntImage2DRect
# pragma push_macro("IntImage2DRect")
# undef IntImage2DRect
template <> struct GLSL2Cpp<
	SLDataType::IntImage2DRect
> { typedef GLint Type; };
# pragma pop_macro("IntImage2DRect")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntImage2DRect
> { typedef GLint Type; };
# endif
#endif // INT_IMAGE_2D_RECT

#ifdef GL_INT_IMAGE_CUBE
# ifdef IntImageCube
# pragma push_macro("IntImageCube")
# undef IntImageCube
template <> struct GLSL2Cpp<
	SLDataType::IntImageCube
> { typedef GLint Type; };
# pragma pop_macro("IntImageCube")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntImageCube
> { typedef GLint Type; };
# endif
#endif // INT_IMAGE_CUBE

#ifdef GL_INT_IMAGE_BUFFER
# ifdef IntImageBuffer
# pragma push_macro("IntImageBuffer")
# undef IntImageBuffer
template <> struct GLSL2Cpp<
	SLDataType::IntImageBuffer
> { typedef GLint Type; };
# pragma pop_macro("IntImageBuffer")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntImageBuffer
> { typedef GLint Type; };
# endif
#endif // INT_IMAGE_BUFFER

#ifdef GL_INT_IMAGE_1D_ARRAY
# ifdef IntImage1DArray
# pragma push_macro("IntImage1DArray")
# undef IntImage1DArray
template <> struct GLSL2Cpp<
	SLDataType::IntImage1DArray
> { typedef GLint Type; };
# pragma pop_macro("IntImage1DArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntImage1DArray
> { typedef GLint Type; };
# endif
#endif // INT_IMAGE_1D_ARRAY

#ifdef GL_INT_IMAGE_2D_ARRAY
# ifdef IntImage2DArray
# pragma push_macro("IntImage2DArray")
# undef IntImage2DArray
template <> struct GLSL2Cpp<
	SLDataType::IntImage2DArray
> { typedef GLint Type; };
# pragma pop_macro("IntImage2DArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntImage2DArray
> { typedef GLint Type; };
# endif
#endif // INT_IMAGE_2D_ARRAY

#ifdef GL_INT_IMAGE_2D_MULTISAMPLE
# ifdef IntImage2DMultisample
# pragma push_macro("IntImage2DMultisample")
# undef IntImage2DMultisample
template <> struct GLSL2Cpp<
	SLDataType::IntImage2DMultisample
> { typedef GLint Type; };
# pragma pop_macro("IntImage2DMultisample")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntImage2DMultisample
> { typedef GLint Type; };
# endif
#endif // INT_IMAGE_2D_MULTISAMPLE

#ifdef GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY
# ifdef IntImage2DMultisampleArray
# pragma push_macro("IntImage2DMultisampleArray")
# undef IntImage2DMultisampleArray
template <> struct GLSL2Cpp<
	SLDataType::IntImage2DMultisampleArray
> { typedef GLint Type; };
# pragma pop_macro("IntImage2DMultisampleArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::IntImage2DMultisampleArray
> { typedef GLint Type; };
# endif
#endif // INT_IMAGE_2D_MULTISAMPLE_ARRAY

#ifdef GL_UNSIGNED_INT_IMAGE_1D
# ifdef UnsignedIntImage1D
# pragma push_macro("UnsignedIntImage1D")
# undef UnsignedIntImage1D
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImage1D
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntImage1D")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImage1D
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_IMAGE_1D

#ifdef GL_UNSIGNED_INT_IMAGE_2D
# ifdef UnsignedIntImage2D
# pragma push_macro("UnsignedIntImage2D")
# undef UnsignedIntImage2D
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImage2D
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntImage2D")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImage2D
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_IMAGE_2D

#ifdef GL_UNSIGNED_INT_IMAGE_3D
# ifdef UnsignedIntImage3D
# pragma push_macro("UnsignedIntImage3D")
# undef UnsignedIntImage3D
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImage3D
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntImage3D")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImage3D
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_IMAGE_3D

#ifdef GL_UNSIGNED_INT_IMAGE_2D_RECT
# ifdef UnsignedIntImage2DRect
# pragma push_macro("UnsignedIntImage2DRect")
# undef UnsignedIntImage2DRect
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImage2DRect
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntImage2DRect")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImage2DRect
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_IMAGE_2D_RECT

#ifdef GL_UNSIGNED_INT_IMAGE_CUBE
# ifdef UnsignedIntImageCube
# pragma push_macro("UnsignedIntImageCube")
# undef UnsignedIntImageCube
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImageCube
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntImageCube")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImageCube
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_IMAGE_CUBE

#ifdef GL_UNSIGNED_INT_IMAGE_BUFFER
# ifdef UnsignedIntImageBuffer
# pragma push_macro("UnsignedIntImageBuffer")
# undef UnsignedIntImageBuffer
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImageBuffer
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntImageBuffer")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImageBuffer
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_IMAGE_BUFFER

#ifdef GL_UNSIGNED_INT_IMAGE_1D_ARRAY
# ifdef UnsignedIntImage1DArray
# pragma push_macro("UnsignedIntImage1DArray")
# undef UnsignedIntImage1DArray
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImage1DArray
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntImage1DArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImage1DArray
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_IMAGE_1D_ARRAY

#ifdef GL_UNSIGNED_INT_IMAGE_2D_ARRAY
# ifdef UnsignedIntImage2DArray
# pragma push_macro("UnsignedIntImage2DArray")
# undef UnsignedIntImage2DArray
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImage2DArray
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntImage2DArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImage2DArray
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_IMAGE_2D_ARRAY

#ifdef GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE
# ifdef UnsignedIntImage2DMultisample
# pragma push_macro("UnsignedIntImage2DMultisample")
# undef UnsignedIntImage2DMultisample
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImage2DMultisample
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntImage2DMultisample")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImage2DMultisample
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_IMAGE_2D_MULTISAMPLE

#ifdef GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY
# ifdef UnsignedIntImage2DMultisampleArray
# pragma push_macro("UnsignedIntImage2DMultisampleArray")
# undef UnsignedIntImage2DMultisampleArray
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImage2DMultisampleArray
> { typedef GLint Type; };
# pragma pop_macro("UnsignedIntImage2DMultisampleArray")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntImage2DMultisampleArray
> { typedef GLint Type; };
# endif
#endif // UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY

#ifdef GL_UNSIGNED_INT_ATOMIC_COUNTER
# ifdef UnsignedIntAtomicCounter
# pragma push_macro("UnsignedIntAtomicCounter")
# undef UnsignedIntAtomicCounter
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntAtomicCounter
> { typedef GLuint Type; };
# pragma pop_macro("UnsignedIntAtomicCounter")
# else
template <> struct GLSL2Cpp<
	SLDataType::UnsignedIntAtomicCounter
> { typedef GLuint Type; };
# endif
#endif // UNSIGNED_INT_ATOMIC_COUNTER

#ifdef GL_NONE
# ifdef None
# pragma push_macro("None")
# undef None
template <> struct GLSL2Cpp<
	SLDataType::None
> { typedef void Type; };
# pragma pop_macro("None")
# else
template <> struct GLSL2Cpp<
	SLDataType::None
> { typedef void Type; };
# endif
#endif // NONE



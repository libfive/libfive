//  File implement/oglplus/enums/ext/nv_path_transform_type_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_transform_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PathNVTransformType> class Transform>
class EnumToClass<Base, PathNVTransformType, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_NONE
# if defined None
#  pragma push_macro("None")
#  undef None
	Transform<PathNVTransformType::None> None;
#  pragma pop_macro("None")
# else
	Transform<PathNVTransformType::None> None;
# endif
#endif
#if defined GL_TRANSLATE_X_NV
# if defined TranslateX
#  pragma push_macro("TranslateX")
#  undef TranslateX
	Transform<PathNVTransformType::TranslateX> TranslateX;
#  pragma pop_macro("TranslateX")
# else
	Transform<PathNVTransformType::TranslateX> TranslateX;
# endif
#endif
#if defined GL_TRANSLATE_Y_NV
# if defined TranslateY
#  pragma push_macro("TranslateY")
#  undef TranslateY
	Transform<PathNVTransformType::TranslateY> TranslateY;
#  pragma pop_macro("TranslateY")
# else
	Transform<PathNVTransformType::TranslateY> TranslateY;
# endif
#endif
#if defined GL_TRANSLATE_2D_NV
# if defined Translate2D
#  pragma push_macro("Translate2D")
#  undef Translate2D
	Transform<PathNVTransformType::Translate2D> Translate2D;
#  pragma pop_macro("Translate2D")
# else
	Transform<PathNVTransformType::Translate2D> Translate2D;
# endif
#endif
#if defined GL_TRANSLATE_3D_NV
# if defined Translate3D
#  pragma push_macro("Translate3D")
#  undef Translate3D
	Transform<PathNVTransformType::Translate3D> Translate3D;
#  pragma pop_macro("Translate3D")
# else
	Transform<PathNVTransformType::Translate3D> Translate3D;
# endif
#endif
#if defined GL_AFFINE_2D_NV
# if defined Affine2D
#  pragma push_macro("Affine2D")
#  undef Affine2D
	Transform<PathNVTransformType::Affine2D> Affine2D;
#  pragma pop_macro("Affine2D")
# else
	Transform<PathNVTransformType::Affine2D> Affine2D;
# endif
#endif
#if defined GL_AFFINE_3D_NV
# if defined Affine3D
#  pragma push_macro("Affine3D")
#  undef Affine3D
	Transform<PathNVTransformType::Affine3D> Affine3D;
#  pragma pop_macro("Affine3D")
# else
	Transform<PathNVTransformType::Affine3D> Affine3D;
# endif
#endif
#if defined GL_TRANSPOSE_AFFINE_2D_NV
# if defined TransposeAffine2D
#  pragma push_macro("TransposeAffine2D")
#  undef TransposeAffine2D
	Transform<PathNVTransformType::TransposeAffine2D> TransposeAffine2D;
#  pragma pop_macro("TransposeAffine2D")
# else
	Transform<PathNVTransformType::TransposeAffine2D> TransposeAffine2D;
# endif
#endif
#if defined GL_TRANSPOSE_AFFINE_3D_NV
# if defined TransposeAffine3D
#  pragma push_macro("TransposeAffine3D")
#  undef TransposeAffine3D
	Transform<PathNVTransformType::TransposeAffine3D> TransposeAffine3D;
#  pragma pop_macro("TransposeAffine3D")
# else
	Transform<PathNVTransformType::TransposeAffine3D> TransposeAffine3D;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
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
#if defined GL_TRANSLATE_X_NV
# if defined TranslateX
#  pragma push_macro("TranslateX")
#  undef TranslateX
	 , TranslateX(_base())
#  pragma pop_macro("TranslateX")
# else
	 , TranslateX(_base())
# endif
#endif
#if defined GL_TRANSLATE_Y_NV
# if defined TranslateY
#  pragma push_macro("TranslateY")
#  undef TranslateY
	 , TranslateY(_base())
#  pragma pop_macro("TranslateY")
# else
	 , TranslateY(_base())
# endif
#endif
#if defined GL_TRANSLATE_2D_NV
# if defined Translate2D
#  pragma push_macro("Translate2D")
#  undef Translate2D
	 , Translate2D(_base())
#  pragma pop_macro("Translate2D")
# else
	 , Translate2D(_base())
# endif
#endif
#if defined GL_TRANSLATE_3D_NV
# if defined Translate3D
#  pragma push_macro("Translate3D")
#  undef Translate3D
	 , Translate3D(_base())
#  pragma pop_macro("Translate3D")
# else
	 , Translate3D(_base())
# endif
#endif
#if defined GL_AFFINE_2D_NV
# if defined Affine2D
#  pragma push_macro("Affine2D")
#  undef Affine2D
	 , Affine2D(_base())
#  pragma pop_macro("Affine2D")
# else
	 , Affine2D(_base())
# endif
#endif
#if defined GL_AFFINE_3D_NV
# if defined Affine3D
#  pragma push_macro("Affine3D")
#  undef Affine3D
	 , Affine3D(_base())
#  pragma pop_macro("Affine3D")
# else
	 , Affine3D(_base())
# endif
#endif
#if defined GL_TRANSPOSE_AFFINE_2D_NV
# if defined TransposeAffine2D
#  pragma push_macro("TransposeAffine2D")
#  undef TransposeAffine2D
	 , TransposeAffine2D(_base())
#  pragma pop_macro("TransposeAffine2D")
# else
	 , TransposeAffine2D(_base())
# endif
#endif
#if defined GL_TRANSPOSE_AFFINE_3D_NV
# if defined TransposeAffine3D
#  pragma push_macro("TransposeAffine3D")
#  undef TransposeAffine3D
	 , TransposeAffine3D(_base())
#  pragma pop_macro("TransposeAffine3D")
# else
	 , TransposeAffine3D(_base())
# endif
#endif
	{ }
};

} // namespace enums


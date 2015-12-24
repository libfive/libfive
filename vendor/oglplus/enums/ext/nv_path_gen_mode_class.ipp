//  File implement/oglplus/enums/ext/nv_path_gen_mode_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_gen_mode.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PathNVGenMode> class Transform>
class EnumToClass<Base, PathNVGenMode, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_NONE
# if defined None
#  pragma push_macro("None")
#  undef None
	Transform<PathNVGenMode::None> None;
#  pragma pop_macro("None")
# else
	Transform<PathNVGenMode::None> None;
# endif
#endif
#if defined GL_EYE_LINEAR
# if defined EyeLinear
#  pragma push_macro("EyeLinear")
#  undef EyeLinear
	Transform<PathNVGenMode::EyeLinear> EyeLinear;
#  pragma pop_macro("EyeLinear")
# else
	Transform<PathNVGenMode::EyeLinear> EyeLinear;
# endif
#endif
#if defined GL_OBJECT_LINEAR
# if defined ObjectLinear
#  pragma push_macro("ObjectLinear")
#  undef ObjectLinear
	Transform<PathNVGenMode::ObjectLinear> ObjectLinear;
#  pragma pop_macro("ObjectLinear")
# else
	Transform<PathNVGenMode::ObjectLinear> ObjectLinear;
# endif
#endif
#if defined GL_PATH_OBJECT_BOUNDING_BOX_NV
# if defined ObjectBoundingBox
#  pragma push_macro("ObjectBoundingBox")
#  undef ObjectBoundingBox
	Transform<PathNVGenMode::ObjectBoundingBox> ObjectBoundingBox;
#  pragma pop_macro("ObjectBoundingBox")
# else
	Transform<PathNVGenMode::ObjectBoundingBox> ObjectBoundingBox;
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
#if defined GL_EYE_LINEAR
# if defined EyeLinear
#  pragma push_macro("EyeLinear")
#  undef EyeLinear
	 , EyeLinear(_base())
#  pragma pop_macro("EyeLinear")
# else
	 , EyeLinear(_base())
# endif
#endif
#if defined GL_OBJECT_LINEAR
# if defined ObjectLinear
#  pragma push_macro("ObjectLinear")
#  undef ObjectLinear
	 , ObjectLinear(_base())
#  pragma pop_macro("ObjectLinear")
# else
	 , ObjectLinear(_base())
# endif
#endif
#if defined GL_PATH_OBJECT_BOUNDING_BOX_NV
# if defined ObjectBoundingBox
#  pragma push_macro("ObjectBoundingBox")
#  undef ObjectBoundingBox
	 , ObjectBoundingBox(_base())
#  pragma pop_macro("ObjectBoundingBox")
# else
	 , ObjectBoundingBox(_base())
# endif
#endif
	{ }
};

} // namespace enums


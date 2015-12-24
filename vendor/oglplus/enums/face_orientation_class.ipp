//  File implement/oglplus/enums/face_orientation_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/face_orientation.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<FaceOrientation> class Transform>
class EnumToClass<Base, FaceOrientation, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_CW
# if defined CW
#  pragma push_macro("CW")
#  undef CW
	Transform<FaceOrientation::CW> CW;
#  pragma pop_macro("CW")
# else
	Transform<FaceOrientation::CW> CW;
# endif
#endif
#if defined GL_CCW
# if defined CCW
#  pragma push_macro("CCW")
#  undef CCW
	Transform<FaceOrientation::CCW> CCW;
#  pragma pop_macro("CCW")
# else
	Transform<FaceOrientation::CCW> CCW;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_CW
# if defined CW
#  pragma push_macro("CW")
#  undef CW
	 , CW(_base())
#  pragma pop_macro("CW")
# else
	 , CW(_base())
# endif
#endif
#if defined GL_CCW
# if defined CCW
#  pragma push_macro("CCW")
#  undef CCW
	 , CCW(_base())
#  pragma pop_macro("CCW")
# else
	 , CCW(_base())
# endif
#endif
	{ }
};

} // namespace enums


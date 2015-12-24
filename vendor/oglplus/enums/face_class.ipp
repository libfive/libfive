//  File implement/oglplus/enums/face_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/face.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<Face> class Transform>
class EnumToClass<Base, Face, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_FRONT
# if defined Front
#  pragma push_macro("Front")
#  undef Front
	Transform<Face::Front> Front;
#  pragma pop_macro("Front")
# else
	Transform<Face::Front> Front;
# endif
#endif
#if defined GL_BACK
# if defined Back
#  pragma push_macro("Back")
#  undef Back
	Transform<Face::Back> Back;
#  pragma pop_macro("Back")
# else
	Transform<Face::Back> Back;
# endif
#endif
#if defined GL_FRONT_AND_BACK
# if defined FrontAndBack
#  pragma push_macro("FrontAndBack")
#  undef FrontAndBack
	Transform<Face::FrontAndBack> FrontAndBack;
#  pragma pop_macro("FrontAndBack")
# else
	Transform<Face::FrontAndBack> FrontAndBack;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_FRONT
# if defined Front
#  pragma push_macro("Front")
#  undef Front
	 , Front(_base())
#  pragma pop_macro("Front")
# else
	 , Front(_base())
# endif
#endif
#if defined GL_BACK
# if defined Back
#  pragma push_macro("Back")
#  undef Back
	 , Back(_base())
#  pragma pop_macro("Back")
# else
	 , Back(_base())
# endif
#endif
#if defined GL_FRONT_AND_BACK
# if defined FrontAndBack
#  pragma push_macro("FrontAndBack")
#  undef FrontAndBack
	 , FrontAndBack(_base())
#  pragma pop_macro("FrontAndBack")
# else
	 , FrontAndBack(_base())
# endif
#endif
	{ }
};

} // namespace enums


//  File implement/oglplus/enums/single_face_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/single_face.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<SingleFace> class Transform>
class EnumToClass<Base, SingleFace, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_FRONT
# if defined Front
#  pragma push_macro("Front")
#  undef Front
	Transform<SingleFace::Front> Front;
#  pragma pop_macro("Front")
# else
	Transform<SingleFace::Front> Front;
# endif
#endif
#if defined GL_BACK
# if defined Back
#  pragma push_macro("Back")
#  undef Back
	Transform<SingleFace::Back> Back;
#  pragma pop_macro("Back")
# else
	Transform<SingleFace::Back> Back;
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
	{ }
};

} // namespace enums


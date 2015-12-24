//  File implement/oglplus/enums/color_buffer_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/color_buffer.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<ColorBuffer> class Transform>
class EnumToClass<Base, ColorBuffer, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_NONE
# if defined None
#  pragma push_macro("None")
#  undef None
	Transform<ColorBuffer::None> None;
#  pragma pop_macro("None")
# else
	Transform<ColorBuffer::None> None;
# endif
#endif
#if defined GL_FRONT_LEFT
# if defined FrontLeft
#  pragma push_macro("FrontLeft")
#  undef FrontLeft
	Transform<ColorBuffer::FrontLeft> FrontLeft;
#  pragma pop_macro("FrontLeft")
# else
	Transform<ColorBuffer::FrontLeft> FrontLeft;
# endif
#endif
#if defined GL_FRONT_RIGHT
# if defined FrontRight
#  pragma push_macro("FrontRight")
#  undef FrontRight
	Transform<ColorBuffer::FrontRight> FrontRight;
#  pragma pop_macro("FrontRight")
# else
	Transform<ColorBuffer::FrontRight> FrontRight;
# endif
#endif
#if defined GL_BACK_LEFT
# if defined BackLeft
#  pragma push_macro("BackLeft")
#  undef BackLeft
	Transform<ColorBuffer::BackLeft> BackLeft;
#  pragma pop_macro("BackLeft")
# else
	Transform<ColorBuffer::BackLeft> BackLeft;
# endif
#endif
#if defined GL_BACK_RIGHT
# if defined BackRight
#  pragma push_macro("BackRight")
#  undef BackRight
	Transform<ColorBuffer::BackRight> BackRight;
#  pragma pop_macro("BackRight")
# else
	Transform<ColorBuffer::BackRight> BackRight;
# endif
#endif
#if defined GL_FRONT
# if defined Front
#  pragma push_macro("Front")
#  undef Front
	Transform<ColorBuffer::Front> Front;
#  pragma pop_macro("Front")
# else
	Transform<ColorBuffer::Front> Front;
# endif
#endif
#if defined GL_BACK
# if defined Back
#  pragma push_macro("Back")
#  undef Back
	Transform<ColorBuffer::Back> Back;
#  pragma pop_macro("Back")
# else
	Transform<ColorBuffer::Back> Back;
# endif
#endif
#if defined GL_LEFT
# if defined Left
#  pragma push_macro("Left")
#  undef Left
	Transform<ColorBuffer::Left> Left;
#  pragma pop_macro("Left")
# else
	Transform<ColorBuffer::Left> Left;
# endif
#endif
#if defined GL_RIGHT
# if defined Right
#  pragma push_macro("Right")
#  undef Right
	Transform<ColorBuffer::Right> Right;
#  pragma pop_macro("Right")
# else
	Transform<ColorBuffer::Right> Right;
# endif
#endif
#if defined GL_FRONT_AND_BACK
# if defined FrontAndBack
#  pragma push_macro("FrontAndBack")
#  undef FrontAndBack
	Transform<ColorBuffer::FrontAndBack> FrontAndBack;
#  pragma pop_macro("FrontAndBack")
# else
	Transform<ColorBuffer::FrontAndBack> FrontAndBack;
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
#if defined GL_FRONT_LEFT
# if defined FrontLeft
#  pragma push_macro("FrontLeft")
#  undef FrontLeft
	 , FrontLeft(_base())
#  pragma pop_macro("FrontLeft")
# else
	 , FrontLeft(_base())
# endif
#endif
#if defined GL_FRONT_RIGHT
# if defined FrontRight
#  pragma push_macro("FrontRight")
#  undef FrontRight
	 , FrontRight(_base())
#  pragma pop_macro("FrontRight")
# else
	 , FrontRight(_base())
# endif
#endif
#if defined GL_BACK_LEFT
# if defined BackLeft
#  pragma push_macro("BackLeft")
#  undef BackLeft
	 , BackLeft(_base())
#  pragma pop_macro("BackLeft")
# else
	 , BackLeft(_base())
# endif
#endif
#if defined GL_BACK_RIGHT
# if defined BackRight
#  pragma push_macro("BackRight")
#  undef BackRight
	 , BackRight(_base())
#  pragma pop_macro("BackRight")
# else
	 , BackRight(_base())
# endif
#endif
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
#if defined GL_LEFT
# if defined Left
#  pragma push_macro("Left")
#  undef Left
	 , Left(_base())
#  pragma pop_macro("Left")
# else
	 , Left(_base())
# endif
#endif
#if defined GL_RIGHT
# if defined Right
#  pragma push_macro("Right")
#  undef Right
	 , Right(_base())
#  pragma pop_macro("Right")
# else
	 , Right(_base())
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


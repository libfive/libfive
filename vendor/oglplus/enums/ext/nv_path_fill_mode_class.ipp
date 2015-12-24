//  File implement/oglplus/enums/ext/nv_path_fill_mode_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_fill_mode.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PathNVFillMode> class Transform>
class EnumToClass<Base, PathNVFillMode, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_INVERT
# if defined Invert
#  pragma push_macro("Invert")
#  undef Invert
	Transform<PathNVFillMode::Invert> Invert;
#  pragma pop_macro("Invert")
# else
	Transform<PathNVFillMode::Invert> Invert;
# endif
#endif
#if defined GL_COUNT_UP_NV
# if defined CountUp
#  pragma push_macro("CountUp")
#  undef CountUp
	Transform<PathNVFillMode::CountUp> CountUp;
#  pragma pop_macro("CountUp")
# else
	Transform<PathNVFillMode::CountUp> CountUp;
# endif
#endif
#if defined GL_COUNT_DOWN_NV
# if defined CountDown
#  pragma push_macro("CountDown")
#  undef CountDown
	Transform<PathNVFillMode::CountDown> CountDown;
#  pragma pop_macro("CountDown")
# else
	Transform<PathNVFillMode::CountDown> CountDown;
# endif
#endif
#if defined GL_PATH_FILL_MODE_NV
# if defined FillMode
#  pragma push_macro("FillMode")
#  undef FillMode
	Transform<PathNVFillMode::FillMode> FillMode;
#  pragma pop_macro("FillMode")
# else
	Transform<PathNVFillMode::FillMode> FillMode;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_INVERT
# if defined Invert
#  pragma push_macro("Invert")
#  undef Invert
	 , Invert(_base())
#  pragma pop_macro("Invert")
# else
	 , Invert(_base())
# endif
#endif
#if defined GL_COUNT_UP_NV
# if defined CountUp
#  pragma push_macro("CountUp")
#  undef CountUp
	 , CountUp(_base())
#  pragma pop_macro("CountUp")
# else
	 , CountUp(_base())
# endif
#endif
#if defined GL_COUNT_DOWN_NV
# if defined CountDown
#  pragma push_macro("CountDown")
#  undef CountDown
	 , CountDown(_base())
#  pragma pop_macro("CountDown")
# else
	 , CountDown(_base())
# endif
#endif
#if defined GL_PATH_FILL_MODE_NV
# if defined FillMode
#  pragma push_macro("FillMode")
#  undef FillMode
	 , FillMode(_base())
#  pragma pop_macro("FillMode")
# else
	 , FillMode(_base())
# endif
#endif
	{ }
};

} // namespace enums


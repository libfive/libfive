//  File implement/oglplus/enums/hint_option_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/hint_option.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<HintOption> class Transform>
class EnumToClass<Base, HintOption, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_FASTEST
# if defined Fastest
#  pragma push_macro("Fastest")
#  undef Fastest
	Transform<HintOption::Fastest> Fastest;
#  pragma pop_macro("Fastest")
# else
	Transform<HintOption::Fastest> Fastest;
# endif
#endif
#if defined GL_NICEST
# if defined Nicest
#  pragma push_macro("Nicest")
#  undef Nicest
	Transform<HintOption::Nicest> Nicest;
#  pragma pop_macro("Nicest")
# else
	Transform<HintOption::Nicest> Nicest;
# endif
#endif
#if defined GL_DONT_CARE
# if defined DontCare
#  pragma push_macro("DontCare")
#  undef DontCare
	Transform<HintOption::DontCare> DontCare;
#  pragma pop_macro("DontCare")
# else
	Transform<HintOption::DontCare> DontCare;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_FASTEST
# if defined Fastest
#  pragma push_macro("Fastest")
#  undef Fastest
	 , Fastest(_base())
#  pragma pop_macro("Fastest")
# else
	 , Fastest(_base())
# endif
#endif
#if defined GL_NICEST
# if defined Nicest
#  pragma push_macro("Nicest")
#  undef Nicest
	 , Nicest(_base())
#  pragma pop_macro("Nicest")
# else
	 , Nicest(_base())
# endif
#endif
#if defined GL_DONT_CARE
# if defined DontCare
#  pragma push_macro("DontCare")
#  undef DontCare
	 , DontCare(_base())
#  pragma pop_macro("DontCare")
# else
	 , DontCare(_base())
# endif
#endif
	{ }
};

} // namespace enums


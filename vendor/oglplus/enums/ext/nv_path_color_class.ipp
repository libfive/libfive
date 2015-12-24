//  File implement/oglplus/enums/ext/nv_path_color_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_color.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PathNVColor> class Transform>
class EnumToClass<Base, PathNVColor, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_PRIMARY_COLOR_NV
# if defined Primary
#  pragma push_macro("Primary")
#  undef Primary
	Transform<PathNVColor::Primary> Primary;
#  pragma pop_macro("Primary")
# else
	Transform<PathNVColor::Primary> Primary;
# endif
#endif
#if defined GL_SECONDARY_COLOR_NV
# if defined Secondary
#  pragma push_macro("Secondary")
#  undef Secondary
	Transform<PathNVColor::Secondary> Secondary;
#  pragma pop_macro("Secondary")
# else
	Transform<PathNVColor::Secondary> Secondary;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_PRIMARY_COLOR_NV
# if defined Primary
#  pragma push_macro("Primary")
#  undef Primary
	 , Primary(_base())
#  pragma pop_macro("Primary")
# else
	 , Primary(_base())
# endif
#endif
#if defined GL_SECONDARY_COLOR_NV
# if defined Secondary
#  pragma push_macro("Secondary")
#  undef Secondary
	 , Secondary(_base())
#  pragma pop_macro("Secondary")
# else
	 , Secondary(_base())
# endif
#endif
	{ }
};

} // namespace enums


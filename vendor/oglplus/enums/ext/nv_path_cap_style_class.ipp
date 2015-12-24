//  File implement/oglplus/enums/ext/nv_path_cap_style_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_cap_style.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PathNVCapStyle> class Transform>
class EnumToClass<Base, PathNVCapStyle, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_FLAT
# if defined Flat
#  pragma push_macro("Flat")
#  undef Flat
	Transform<PathNVCapStyle::Flat> Flat;
#  pragma pop_macro("Flat")
# else
	Transform<PathNVCapStyle::Flat> Flat;
# endif
#endif
#if defined GL_SQUARE_NV
# if defined Square
#  pragma push_macro("Square")
#  undef Square
	Transform<PathNVCapStyle::Square> Square;
#  pragma pop_macro("Square")
# else
	Transform<PathNVCapStyle::Square> Square;
# endif
#endif
#if defined GL_ROUND_NV
# if defined Round
#  pragma push_macro("Round")
#  undef Round
	Transform<PathNVCapStyle::Round> Round;
#  pragma pop_macro("Round")
# else
	Transform<PathNVCapStyle::Round> Round;
# endif
#endif
#if defined GL_TRIANGULAR_NV
# if defined Triangular
#  pragma push_macro("Triangular")
#  undef Triangular
	Transform<PathNVCapStyle::Triangular> Triangular;
#  pragma pop_macro("Triangular")
# else
	Transform<PathNVCapStyle::Triangular> Triangular;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_FLAT
# if defined Flat
#  pragma push_macro("Flat")
#  undef Flat
	 , Flat(_base())
#  pragma pop_macro("Flat")
# else
	 , Flat(_base())
# endif
#endif
#if defined GL_SQUARE_NV
# if defined Square
#  pragma push_macro("Square")
#  undef Square
	 , Square(_base())
#  pragma pop_macro("Square")
# else
	 , Square(_base())
# endif
#endif
#if defined GL_ROUND_NV
# if defined Round
#  pragma push_macro("Round")
#  undef Round
	 , Round(_base())
#  pragma pop_macro("Round")
# else
	 , Round(_base())
# endif
#endif
#if defined GL_TRIANGULAR_NV
# if defined Triangular
#  pragma push_macro("Triangular")
#  undef Triangular
	 , Triangular(_base())
#  pragma pop_macro("Triangular")
# else
	 , Triangular(_base())
# endif
#endif
	{ }
};

} // namespace enums


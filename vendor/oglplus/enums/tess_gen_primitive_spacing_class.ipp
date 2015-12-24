//  File implement/oglplus/enums/tess_gen_primitive_spacing_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/tess_gen_primitive_spacing.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<TessGenPrimitiveSpacing> class Transform>
class EnumToClass<Base, TessGenPrimitiveSpacing, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_FRACTIONAL_EVEN
# if defined FractionalEven
#  pragma push_macro("FractionalEven")
#  undef FractionalEven
	Transform<TessGenPrimitiveSpacing::FractionalEven> FractionalEven;
#  pragma pop_macro("FractionalEven")
# else
	Transform<TessGenPrimitiveSpacing::FractionalEven> FractionalEven;
# endif
#endif
#if defined GL_FRACTIONAL_ODD
# if defined FractionalOdd
#  pragma push_macro("FractionalOdd")
#  undef FractionalOdd
	Transform<TessGenPrimitiveSpacing::FractionalOdd> FractionalOdd;
#  pragma pop_macro("FractionalOdd")
# else
	Transform<TessGenPrimitiveSpacing::FractionalOdd> FractionalOdd;
# endif
#endif
#if defined GL_EQUAL
# if defined Equal
#  pragma push_macro("Equal")
#  undef Equal
	Transform<TessGenPrimitiveSpacing::Equal> Equal;
#  pragma pop_macro("Equal")
# else
	Transform<TessGenPrimitiveSpacing::Equal> Equal;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_FRACTIONAL_EVEN
# if defined FractionalEven
#  pragma push_macro("FractionalEven")
#  undef FractionalEven
	 , FractionalEven(_base())
#  pragma pop_macro("FractionalEven")
# else
	 , FractionalEven(_base())
# endif
#endif
#if defined GL_FRACTIONAL_ODD
# if defined FractionalOdd
#  pragma push_macro("FractionalOdd")
#  undef FractionalOdd
	 , FractionalOdd(_base())
#  pragma pop_macro("FractionalOdd")
# else
	 , FractionalOdd(_base())
# endif
#endif
#if defined GL_EQUAL
# if defined Equal
#  pragma push_macro("Equal")
#  undef Equal
	 , Equal(_base())
#  pragma pop_macro("Equal")
# else
	 , Equal(_base())
# endif
#endif
	{ }
};

} // namespace enums


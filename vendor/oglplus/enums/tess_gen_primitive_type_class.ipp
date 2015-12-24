//  File implement/oglplus/enums/tess_gen_primitive_type_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/tess_gen_primitive_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<TessGenPrimitiveType> class Transform>
class EnumToClass<Base, TessGenPrimitiveType, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_QUADS
# if defined Quads
#  pragma push_macro("Quads")
#  undef Quads
	Transform<TessGenPrimitiveType::Quads> Quads;
#  pragma pop_macro("Quads")
# else
	Transform<TessGenPrimitiveType::Quads> Quads;
# endif
#endif
#if defined GL_TRIANGLES
# if defined Triangles
#  pragma push_macro("Triangles")
#  undef Triangles
	Transform<TessGenPrimitiveType::Triangles> Triangles;
#  pragma pop_macro("Triangles")
# else
	Transform<TessGenPrimitiveType::Triangles> Triangles;
# endif
#endif
#if defined GL_ISOLINES
# if defined Isolines
#  pragma push_macro("Isolines")
#  undef Isolines
	Transform<TessGenPrimitiveType::Isolines> Isolines;
#  pragma pop_macro("Isolines")
# else
	Transform<TessGenPrimitiveType::Isolines> Isolines;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_QUADS
# if defined Quads
#  pragma push_macro("Quads")
#  undef Quads
	 , Quads(_base())
#  pragma pop_macro("Quads")
# else
	 , Quads(_base())
# endif
#endif
#if defined GL_TRIANGLES
# if defined Triangles
#  pragma push_macro("Triangles")
#  undef Triangles
	 , Triangles(_base())
#  pragma pop_macro("Triangles")
# else
	 , Triangles(_base())
# endif
#endif
#if defined GL_ISOLINES
# if defined Isolines
#  pragma push_macro("Isolines")
#  undef Isolines
	 , Isolines(_base())
#  pragma pop_macro("Isolines")
# else
	 , Isolines(_base())
# endif
#endif
	{ }
};

} // namespace enums


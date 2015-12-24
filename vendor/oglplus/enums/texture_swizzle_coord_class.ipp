//  File implement/oglplus/enums/texture_swizzle_coord_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/texture_swizzle_coord.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<TextureSwizzleCoord> class Transform>
class EnumToClass<Base, TextureSwizzleCoord, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_TEXTURE_SWIZZLE_R
# if defined R
#  pragma push_macro("R")
#  undef R
	Transform<TextureSwizzleCoord::R> R;
#  pragma pop_macro("R")
# else
	Transform<TextureSwizzleCoord::R> R;
# endif
#endif
#if defined GL_TEXTURE_SWIZZLE_G
# if defined G
#  pragma push_macro("G")
#  undef G
	Transform<TextureSwizzleCoord::G> G;
#  pragma pop_macro("G")
# else
	Transform<TextureSwizzleCoord::G> G;
# endif
#endif
#if defined GL_TEXTURE_SWIZZLE_B
# if defined B
#  pragma push_macro("B")
#  undef B
	Transform<TextureSwizzleCoord::B> B;
#  pragma pop_macro("B")
# else
	Transform<TextureSwizzleCoord::B> B;
# endif
#endif
#if defined GL_TEXTURE_SWIZZLE_A
# if defined A
#  pragma push_macro("A")
#  undef A
	Transform<TextureSwizzleCoord::A> A;
#  pragma pop_macro("A")
# else
	Transform<TextureSwizzleCoord::A> A;
# endif
#endif
#if defined GL_TEXTURE_SWIZZLE_RGBA
# if defined RGBA
#  pragma push_macro("RGBA")
#  undef RGBA
	Transform<TextureSwizzleCoord::RGBA> RGBA;
#  pragma pop_macro("RGBA")
# else
	Transform<TextureSwizzleCoord::RGBA> RGBA;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_TEXTURE_SWIZZLE_R
# if defined R
#  pragma push_macro("R")
#  undef R
	 , R(_base())
#  pragma pop_macro("R")
# else
	 , R(_base())
# endif
#endif
#if defined GL_TEXTURE_SWIZZLE_G
# if defined G
#  pragma push_macro("G")
#  undef G
	 , G(_base())
#  pragma pop_macro("G")
# else
	 , G(_base())
# endif
#endif
#if defined GL_TEXTURE_SWIZZLE_B
# if defined B
#  pragma push_macro("B")
#  undef B
	 , B(_base())
#  pragma pop_macro("B")
# else
	 , B(_base())
# endif
#endif
#if defined GL_TEXTURE_SWIZZLE_A
# if defined A
#  pragma push_macro("A")
#  undef A
	 , A(_base())
#  pragma pop_macro("A")
# else
	 , A(_base())
# endif
#endif
#if defined GL_TEXTURE_SWIZZLE_RGBA
# if defined RGBA
#  pragma push_macro("RGBA")
#  undef RGBA
	 , RGBA(_base())
#  pragma pop_macro("RGBA")
# else
	 , RGBA(_base())
# endif
#endif
	{ }
};

} // namespace enums


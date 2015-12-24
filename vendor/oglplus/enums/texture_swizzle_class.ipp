//  File implement/oglplus/enums/texture_swizzle_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/texture_swizzle.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<TextureSwizzle> class Transform>
class EnumToClass<Base, TextureSwizzle, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_RED
# if defined Red
#  pragma push_macro("Red")
#  undef Red
	Transform<TextureSwizzle::Red> Red;
#  pragma pop_macro("Red")
# else
	Transform<TextureSwizzle::Red> Red;
# endif
#endif
#if defined GL_GREEN
# if defined Green
#  pragma push_macro("Green")
#  undef Green
	Transform<TextureSwizzle::Green> Green;
#  pragma pop_macro("Green")
# else
	Transform<TextureSwizzle::Green> Green;
# endif
#endif
#if defined GL_BLUE
# if defined Blue
#  pragma push_macro("Blue")
#  undef Blue
	Transform<TextureSwizzle::Blue> Blue;
#  pragma pop_macro("Blue")
# else
	Transform<TextureSwizzle::Blue> Blue;
# endif
#endif
#if defined GL_ALPHA
# if defined Alpha
#  pragma push_macro("Alpha")
#  undef Alpha
	Transform<TextureSwizzle::Alpha> Alpha;
#  pragma pop_macro("Alpha")
# else
	Transform<TextureSwizzle::Alpha> Alpha;
# endif
#endif
#if defined GL_ZERO
# if defined Zero
#  pragma push_macro("Zero")
#  undef Zero
	Transform<TextureSwizzle::Zero> Zero;
#  pragma pop_macro("Zero")
# else
	Transform<TextureSwizzle::Zero> Zero;
# endif
#endif
#if defined GL_ONE
# if defined One
#  pragma push_macro("One")
#  undef One
	Transform<TextureSwizzle::One> One;
#  pragma pop_macro("One")
# else
	Transform<TextureSwizzle::One> One;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_RED
# if defined Red
#  pragma push_macro("Red")
#  undef Red
	 , Red(_base())
#  pragma pop_macro("Red")
# else
	 , Red(_base())
# endif
#endif
#if defined GL_GREEN
# if defined Green
#  pragma push_macro("Green")
#  undef Green
	 , Green(_base())
#  pragma pop_macro("Green")
# else
	 , Green(_base())
# endif
#endif
#if defined GL_BLUE
# if defined Blue
#  pragma push_macro("Blue")
#  undef Blue
	 , Blue(_base())
#  pragma pop_macro("Blue")
# else
	 , Blue(_base())
# endif
#endif
#if defined GL_ALPHA
# if defined Alpha
#  pragma push_macro("Alpha")
#  undef Alpha
	 , Alpha(_base())
#  pragma pop_macro("Alpha")
# else
	 , Alpha(_base())
# endif
#endif
#if defined GL_ZERO
# if defined Zero
#  pragma push_macro("Zero")
#  undef Zero
	 , Zero(_base())
#  pragma pop_macro("Zero")
# else
	 , Zero(_base())
# endif
#endif
#if defined GL_ONE
# if defined One
#  pragma push_macro("One")
#  undef One
	 , One(_base())
#  pragma pop_macro("One")
# else
	 , One(_base())
# endif
#endif
	{ }
};

} // namespace enums


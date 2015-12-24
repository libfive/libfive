//  File implement/oglplus/enums/texture_wrap_coord_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/texture_wrap_coord.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<TextureWrapCoord> class Transform>
class EnumToClass<Base, TextureWrapCoord, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_TEXTURE_WRAP_S
# if defined S
#  pragma push_macro("S")
#  undef S
	Transform<TextureWrapCoord::S> S;
#  pragma pop_macro("S")
# else
	Transform<TextureWrapCoord::S> S;
# endif
#endif
#if defined GL_TEXTURE_WRAP_T
# if defined T
#  pragma push_macro("T")
#  undef T
	Transform<TextureWrapCoord::T> T;
#  pragma pop_macro("T")
# else
	Transform<TextureWrapCoord::T> T;
# endif
#endif
#if defined GL_TEXTURE_WRAP_R
# if defined R
#  pragma push_macro("R")
#  undef R
	Transform<TextureWrapCoord::R> R;
#  pragma pop_macro("R")
# else
	Transform<TextureWrapCoord::R> R;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_TEXTURE_WRAP_S
# if defined S
#  pragma push_macro("S")
#  undef S
	 , S(_base())
#  pragma pop_macro("S")
# else
	 , S(_base())
# endif
#endif
#if defined GL_TEXTURE_WRAP_T
# if defined T
#  pragma push_macro("T")
#  undef T
	 , T(_base())
#  pragma pop_macro("T")
# else
	 , T(_base())
# endif
#endif
#if defined GL_TEXTURE_WRAP_R
# if defined R
#  pragma push_macro("R")
#  undef R
	 , R(_base())
#  pragma pop_macro("R")
# else
	 , R(_base())
# endif
#endif
	{ }
};

} // namespace enums


//  File implement/oglplus/enums/texture_filter_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/texture_filter.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<TextureFilter> class Transform>
class EnumToClass<Base, TextureFilter, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_NEAREST
# if defined Nearest
#  pragma push_macro("Nearest")
#  undef Nearest
	Transform<TextureFilter::Nearest> Nearest;
#  pragma pop_macro("Nearest")
# else
	Transform<TextureFilter::Nearest> Nearest;
# endif
#endif
#if defined GL_LINEAR
# if defined Linear
#  pragma push_macro("Linear")
#  undef Linear
	Transform<TextureFilter::Linear> Linear;
#  pragma pop_macro("Linear")
# else
	Transform<TextureFilter::Linear> Linear;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_NEAREST
# if defined Nearest
#  pragma push_macro("Nearest")
#  undef Nearest
	 , Nearest(_base())
#  pragma pop_macro("Nearest")
# else
	 , Nearest(_base())
# endif
#endif
#if defined GL_LINEAR
# if defined Linear
#  pragma push_macro("Linear")
#  undef Linear
	 , Linear(_base())
#  pragma pop_macro("Linear")
# else
	 , Linear(_base())
# endif
#endif
	{ }
};

} // namespace enums


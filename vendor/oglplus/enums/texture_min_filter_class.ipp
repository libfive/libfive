//  File implement/oglplus/enums/texture_min_filter_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/texture_min_filter.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<TextureMinFilter> class Transform>
class EnumToClass<Base, TextureMinFilter, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_NEAREST
# if defined Nearest
#  pragma push_macro("Nearest")
#  undef Nearest
	Transform<TextureMinFilter::Nearest> Nearest;
#  pragma pop_macro("Nearest")
# else
	Transform<TextureMinFilter::Nearest> Nearest;
# endif
#endif
#if defined GL_LINEAR
# if defined Linear
#  pragma push_macro("Linear")
#  undef Linear
	Transform<TextureMinFilter::Linear> Linear;
#  pragma pop_macro("Linear")
# else
	Transform<TextureMinFilter::Linear> Linear;
# endif
#endif
#if defined GL_NEAREST_MIPMAP_NEAREST
# if defined NearestMipmapNearest
#  pragma push_macro("NearestMipmapNearest")
#  undef NearestMipmapNearest
	Transform<TextureMinFilter::NearestMipmapNearest> NearestMipmapNearest;
#  pragma pop_macro("NearestMipmapNearest")
# else
	Transform<TextureMinFilter::NearestMipmapNearest> NearestMipmapNearest;
# endif
#endif
#if defined GL_NEAREST_MIPMAP_LINEAR
# if defined NearestMipmapLinear
#  pragma push_macro("NearestMipmapLinear")
#  undef NearestMipmapLinear
	Transform<TextureMinFilter::NearestMipmapLinear> NearestMipmapLinear;
#  pragma pop_macro("NearestMipmapLinear")
# else
	Transform<TextureMinFilter::NearestMipmapLinear> NearestMipmapLinear;
# endif
#endif
#if defined GL_LINEAR_MIPMAP_NEAREST
# if defined LinearMipmapNearest
#  pragma push_macro("LinearMipmapNearest")
#  undef LinearMipmapNearest
	Transform<TextureMinFilter::LinearMipmapNearest> LinearMipmapNearest;
#  pragma pop_macro("LinearMipmapNearest")
# else
	Transform<TextureMinFilter::LinearMipmapNearest> LinearMipmapNearest;
# endif
#endif
#if defined GL_LINEAR_MIPMAP_LINEAR
# if defined LinearMipmapLinear
#  pragma push_macro("LinearMipmapLinear")
#  undef LinearMipmapLinear
	Transform<TextureMinFilter::LinearMipmapLinear> LinearMipmapLinear;
#  pragma pop_macro("LinearMipmapLinear")
# else
	Transform<TextureMinFilter::LinearMipmapLinear> LinearMipmapLinear;
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
#if defined GL_NEAREST_MIPMAP_NEAREST
# if defined NearestMipmapNearest
#  pragma push_macro("NearestMipmapNearest")
#  undef NearestMipmapNearest
	 , NearestMipmapNearest(_base())
#  pragma pop_macro("NearestMipmapNearest")
# else
	 , NearestMipmapNearest(_base())
# endif
#endif
#if defined GL_NEAREST_MIPMAP_LINEAR
# if defined NearestMipmapLinear
#  pragma push_macro("NearestMipmapLinear")
#  undef NearestMipmapLinear
	 , NearestMipmapLinear(_base())
#  pragma pop_macro("NearestMipmapLinear")
# else
	 , NearestMipmapLinear(_base())
# endif
#endif
#if defined GL_LINEAR_MIPMAP_NEAREST
# if defined LinearMipmapNearest
#  pragma push_macro("LinearMipmapNearest")
#  undef LinearMipmapNearest
	 , LinearMipmapNearest(_base())
#  pragma pop_macro("LinearMipmapNearest")
# else
	 , LinearMipmapNearest(_base())
# endif
#endif
#if defined GL_LINEAR_MIPMAP_LINEAR
# if defined LinearMipmapLinear
#  pragma push_macro("LinearMipmapLinear")
#  undef LinearMipmapLinear
	 , LinearMipmapLinear(_base())
#  pragma pop_macro("LinearMipmapLinear")
# else
	 , LinearMipmapLinear(_base())
# endif
#endif
	{ }
};

} // namespace enums


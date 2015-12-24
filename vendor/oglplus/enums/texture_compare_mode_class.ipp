//  File implement/oglplus/enums/texture_compare_mode_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/texture_compare_mode.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<TextureCompareMode> class Transform>
class EnumToClass<Base, TextureCompareMode, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_NONE
# if defined None
#  pragma push_macro("None")
#  undef None
	Transform<TextureCompareMode::None> None;
#  pragma pop_macro("None")
# else
	Transform<TextureCompareMode::None> None;
# endif
#endif
#if defined GL_COMPARE_REF_TO_TEXTURE
# if defined CompareRefToTexture
#  pragma push_macro("CompareRefToTexture")
#  undef CompareRefToTexture
	Transform<TextureCompareMode::CompareRefToTexture> CompareRefToTexture;
#  pragma pop_macro("CompareRefToTexture")
# else
	Transform<TextureCompareMode::CompareRefToTexture> CompareRefToTexture;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_NONE
# if defined None
#  pragma push_macro("None")
#  undef None
	 , None(_base())
#  pragma pop_macro("None")
# else
	 , None(_base())
# endif
#endif
#if defined GL_COMPARE_REF_TO_TEXTURE
# if defined CompareRefToTexture
#  pragma push_macro("CompareRefToTexture")
#  undef CompareRefToTexture
	 , CompareRefToTexture(_base())
#  pragma pop_macro("CompareRefToTexture")
# else
	 , CompareRefToTexture(_base())
# endif
#endif
	{ }
};

} // namespace enums


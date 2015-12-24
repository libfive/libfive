//  File implement/oglplus/enums/ext/nv_path_font_style_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_font_style.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PathNVFontStyle> class Transform>
class EnumToClass<Base, PathNVFontStyle, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_BOLD_BIT_NV
# if defined Bold
#  pragma push_macro("Bold")
#  undef Bold
	Transform<PathNVFontStyle::Bold> Bold;
#  pragma pop_macro("Bold")
# else
	Transform<PathNVFontStyle::Bold> Bold;
# endif
#endif
#if defined GL_ITALIC_BIT_NV
# if defined Italic
#  pragma push_macro("Italic")
#  undef Italic
	Transform<PathNVFontStyle::Italic> Italic;
#  pragma pop_macro("Italic")
# else
	Transform<PathNVFontStyle::Italic> Italic;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_BOLD_BIT_NV
# if defined Bold
#  pragma push_macro("Bold")
#  undef Bold
	 , Bold(_base())
#  pragma pop_macro("Bold")
# else
	 , Bold(_base())
# endif
#endif
#if defined GL_ITALIC_BIT_NV
# if defined Italic
#  pragma push_macro("Italic")
#  undef Italic
	 , Italic(_base())
#  pragma pop_macro("Italic")
# else
	 , Italic(_base())
# endif
#endif
	{ }
};

} // namespace enums


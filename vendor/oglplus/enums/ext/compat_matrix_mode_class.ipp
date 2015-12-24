//  File implement/oglplus/enums/ext/compat_matrix_mode_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/compat_matrix_mode.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<CompatibilityMatrixMode> class Transform>
class EnumToClass<Base, CompatibilityMatrixMode, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_PROJECTION
# if defined Projection
#  pragma push_macro("Projection")
#  undef Projection
	Transform<CompatibilityMatrixMode::Projection> Projection;
#  pragma pop_macro("Projection")
# else
	Transform<CompatibilityMatrixMode::Projection> Projection;
# endif
#endif
#if defined GL_MODELVIEW
# if defined Modelview
#  pragma push_macro("Modelview")
#  undef Modelview
	Transform<CompatibilityMatrixMode::Modelview> Modelview;
#  pragma pop_macro("Modelview")
# else
	Transform<CompatibilityMatrixMode::Modelview> Modelview;
# endif
#endif
#if defined GL_TEXTURE
# if defined Texture
#  pragma push_macro("Texture")
#  undef Texture
	Transform<CompatibilityMatrixMode::Texture> Texture;
#  pragma pop_macro("Texture")
# else
	Transform<CompatibilityMatrixMode::Texture> Texture;
# endif
#endif
#if defined GL_COLOR
# if defined Color
#  pragma push_macro("Color")
#  undef Color
	Transform<CompatibilityMatrixMode::Color> Color;
#  pragma pop_macro("Color")
# else
	Transform<CompatibilityMatrixMode::Color> Color;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_PROJECTION
# if defined Projection
#  pragma push_macro("Projection")
#  undef Projection
	 , Projection(_base())
#  pragma pop_macro("Projection")
# else
	 , Projection(_base())
# endif
#endif
#if defined GL_MODELVIEW
# if defined Modelview
#  pragma push_macro("Modelview")
#  undef Modelview
	 , Modelview(_base())
#  pragma pop_macro("Modelview")
# else
	 , Modelview(_base())
# endif
#endif
#if defined GL_TEXTURE
# if defined Texture
#  pragma push_macro("Texture")
#  undef Texture
	 , Texture(_base())
#  pragma pop_macro("Texture")
# else
	 , Texture(_base())
# endif
#endif
#if defined GL_COLOR
# if defined Color
#  pragma push_macro("Color")
#  undef Color
	 , Color(_base())
#  pragma pop_macro("Color")
# else
	 , Color(_base())
# endif
#endif
	{ }
};

} // namespace enums


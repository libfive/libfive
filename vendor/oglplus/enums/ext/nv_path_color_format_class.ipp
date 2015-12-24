//  File implement/oglplus/enums/ext/nv_path_color_format_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_color_format.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PathNVColorFormat> class Transform>
class EnumToClass<Base, PathNVColorFormat, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_LUMINANCE
# if defined Luminance
#  pragma push_macro("Luminance")
#  undef Luminance
	Transform<PathNVColorFormat::Luminance> Luminance;
#  pragma pop_macro("Luminance")
# else
	Transform<PathNVColorFormat::Luminance> Luminance;
# endif
#endif
#if defined GL_ALPHA
# if defined Alpha
#  pragma push_macro("Alpha")
#  undef Alpha
	Transform<PathNVColorFormat::Alpha> Alpha;
#  pragma pop_macro("Alpha")
# else
	Transform<PathNVColorFormat::Alpha> Alpha;
# endif
#endif
#if defined GL_INTENSITY
# if defined Intensity
#  pragma push_macro("Intensity")
#  undef Intensity
	Transform<PathNVColorFormat::Intensity> Intensity;
#  pragma pop_macro("Intensity")
# else
	Transform<PathNVColorFormat::Intensity> Intensity;
# endif
#endif
#if defined GL_LUMINANCE_ALPHA
# if defined LuminanceAlpha
#  pragma push_macro("LuminanceAlpha")
#  undef LuminanceAlpha
	Transform<PathNVColorFormat::LuminanceAlpha> LuminanceAlpha;
#  pragma pop_macro("LuminanceAlpha")
# else
	Transform<PathNVColorFormat::LuminanceAlpha> LuminanceAlpha;
# endif
#endif
#if defined GL_RGB
# if defined RGB
#  pragma push_macro("RGB")
#  undef RGB
	Transform<PathNVColorFormat::RGB> RGB;
#  pragma pop_macro("RGB")
# else
	Transform<PathNVColorFormat::RGB> RGB;
# endif
#endif
#if defined GL_RGBA
# if defined RGBA
#  pragma push_macro("RGBA")
#  undef RGBA
	Transform<PathNVColorFormat::RGBA> RGBA;
#  pragma pop_macro("RGBA")
# else
	Transform<PathNVColorFormat::RGBA> RGBA;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_LUMINANCE
# if defined Luminance
#  pragma push_macro("Luminance")
#  undef Luminance
	 , Luminance(_base())
#  pragma pop_macro("Luminance")
# else
	 , Luminance(_base())
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
#if defined GL_INTENSITY
# if defined Intensity
#  pragma push_macro("Intensity")
#  undef Intensity
	 , Intensity(_base())
#  pragma pop_macro("Intensity")
# else
	 , Intensity(_base())
# endif
#endif
#if defined GL_LUMINANCE_ALPHA
# if defined LuminanceAlpha
#  pragma push_macro("LuminanceAlpha")
#  undef LuminanceAlpha
	 , LuminanceAlpha(_base())
#  pragma pop_macro("LuminanceAlpha")
# else
	 , LuminanceAlpha(_base())
# endif
#endif
#if defined GL_RGB
# if defined RGB
#  pragma push_macro("RGB")
#  undef RGB
	 , RGB(_base())
#  pragma pop_macro("RGB")
# else
	 , RGB(_base())
# endif
#endif
#if defined GL_RGBA
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


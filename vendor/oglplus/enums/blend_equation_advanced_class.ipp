//  File implement/oglplus/enums/blend_equation_advanced_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/blend_equation_advanced.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<BlendEquationAdvanced> class Transform>
class EnumToClass<Base, BlendEquationAdvanced, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_MULTIPLY_KHR
# if defined Multiply
#  pragma push_macro("Multiply")
#  undef Multiply
	Transform<BlendEquationAdvanced::Multiply> Multiply;
#  pragma pop_macro("Multiply")
# else
	Transform<BlendEquationAdvanced::Multiply> Multiply;
# endif
#endif
#if defined GL_SCREEN_KHR
# if defined Screen
#  pragma push_macro("Screen")
#  undef Screen
	Transform<BlendEquationAdvanced::Screen> Screen;
#  pragma pop_macro("Screen")
# else
	Transform<BlendEquationAdvanced::Screen> Screen;
# endif
#endif
#if defined GL_OVERLAY_KHR
# if defined Overlay
#  pragma push_macro("Overlay")
#  undef Overlay
	Transform<BlendEquationAdvanced::Overlay> Overlay;
#  pragma pop_macro("Overlay")
# else
	Transform<BlendEquationAdvanced::Overlay> Overlay;
# endif
#endif
#if defined GL_DARKEN_KHR
# if defined Darken
#  pragma push_macro("Darken")
#  undef Darken
	Transform<BlendEquationAdvanced::Darken> Darken;
#  pragma pop_macro("Darken")
# else
	Transform<BlendEquationAdvanced::Darken> Darken;
# endif
#endif
#if defined GL_LIGHTEN_KHR
# if defined Lighten
#  pragma push_macro("Lighten")
#  undef Lighten
	Transform<BlendEquationAdvanced::Lighten> Lighten;
#  pragma pop_macro("Lighten")
# else
	Transform<BlendEquationAdvanced::Lighten> Lighten;
# endif
#endif
#if defined GL_COLORDODGE_KHR
# if defined Colordodge
#  pragma push_macro("Colordodge")
#  undef Colordodge
	Transform<BlendEquationAdvanced::Colordodge> Colordodge;
#  pragma pop_macro("Colordodge")
# else
	Transform<BlendEquationAdvanced::Colordodge> Colordodge;
# endif
#endif
#if defined GL_COLORBURN_KHR
# if defined Colorburn
#  pragma push_macro("Colorburn")
#  undef Colorburn
	Transform<BlendEquationAdvanced::Colorburn> Colorburn;
#  pragma pop_macro("Colorburn")
# else
	Transform<BlendEquationAdvanced::Colorburn> Colorburn;
# endif
#endif
#if defined GL_HARDLIGHT_KHR
# if defined Hardlight
#  pragma push_macro("Hardlight")
#  undef Hardlight
	Transform<BlendEquationAdvanced::Hardlight> Hardlight;
#  pragma pop_macro("Hardlight")
# else
	Transform<BlendEquationAdvanced::Hardlight> Hardlight;
# endif
#endif
#if defined GL_SOFTLIGHT_KHR
# if defined Softlight
#  pragma push_macro("Softlight")
#  undef Softlight
	Transform<BlendEquationAdvanced::Softlight> Softlight;
#  pragma pop_macro("Softlight")
# else
	Transform<BlendEquationAdvanced::Softlight> Softlight;
# endif
#endif
#if defined GL_DIFFERENCE_KHR
# if defined Difference
#  pragma push_macro("Difference")
#  undef Difference
	Transform<BlendEquationAdvanced::Difference> Difference;
#  pragma pop_macro("Difference")
# else
	Transform<BlendEquationAdvanced::Difference> Difference;
# endif
#endif
#if defined GL_EXCLUSION_KHR
# if defined Exclusion
#  pragma push_macro("Exclusion")
#  undef Exclusion
	Transform<BlendEquationAdvanced::Exclusion> Exclusion;
#  pragma pop_macro("Exclusion")
# else
	Transform<BlendEquationAdvanced::Exclusion> Exclusion;
# endif
#endif
#if defined GL_HSL_HUE_KHR
# if defined HSLHue
#  pragma push_macro("HSLHue")
#  undef HSLHue
	Transform<BlendEquationAdvanced::HSLHue> HSLHue;
#  pragma pop_macro("HSLHue")
# else
	Transform<BlendEquationAdvanced::HSLHue> HSLHue;
# endif
#endif
#if defined GL_HSL_SATURATION_KHR
# if defined HSLSaturation
#  pragma push_macro("HSLSaturation")
#  undef HSLSaturation
	Transform<BlendEquationAdvanced::HSLSaturation> HSLSaturation;
#  pragma pop_macro("HSLSaturation")
# else
	Transform<BlendEquationAdvanced::HSLSaturation> HSLSaturation;
# endif
#endif
#if defined GL_HSL_COLOR_KHR
# if defined HSLColor
#  pragma push_macro("HSLColor")
#  undef HSLColor
	Transform<BlendEquationAdvanced::HSLColor> HSLColor;
#  pragma pop_macro("HSLColor")
# else
	Transform<BlendEquationAdvanced::HSLColor> HSLColor;
# endif
#endif
#if defined GL_HSL_LUMINOSITY_KHR
# if defined HSLLuminosity
#  pragma push_macro("HSLLuminosity")
#  undef HSLLuminosity
	Transform<BlendEquationAdvanced::HSLLuminosity> HSLLuminosity;
#  pragma pop_macro("HSLLuminosity")
# else
	Transform<BlendEquationAdvanced::HSLLuminosity> HSLLuminosity;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_MULTIPLY_KHR
# if defined Multiply
#  pragma push_macro("Multiply")
#  undef Multiply
	 , Multiply(_base())
#  pragma pop_macro("Multiply")
# else
	 , Multiply(_base())
# endif
#endif
#if defined GL_SCREEN_KHR
# if defined Screen
#  pragma push_macro("Screen")
#  undef Screen
	 , Screen(_base())
#  pragma pop_macro("Screen")
# else
	 , Screen(_base())
# endif
#endif
#if defined GL_OVERLAY_KHR
# if defined Overlay
#  pragma push_macro("Overlay")
#  undef Overlay
	 , Overlay(_base())
#  pragma pop_macro("Overlay")
# else
	 , Overlay(_base())
# endif
#endif
#if defined GL_DARKEN_KHR
# if defined Darken
#  pragma push_macro("Darken")
#  undef Darken
	 , Darken(_base())
#  pragma pop_macro("Darken")
# else
	 , Darken(_base())
# endif
#endif
#if defined GL_LIGHTEN_KHR
# if defined Lighten
#  pragma push_macro("Lighten")
#  undef Lighten
	 , Lighten(_base())
#  pragma pop_macro("Lighten")
# else
	 , Lighten(_base())
# endif
#endif
#if defined GL_COLORDODGE_KHR
# if defined Colordodge
#  pragma push_macro("Colordodge")
#  undef Colordodge
	 , Colordodge(_base())
#  pragma pop_macro("Colordodge")
# else
	 , Colordodge(_base())
# endif
#endif
#if defined GL_COLORBURN_KHR
# if defined Colorburn
#  pragma push_macro("Colorburn")
#  undef Colorburn
	 , Colorburn(_base())
#  pragma pop_macro("Colorburn")
# else
	 , Colorburn(_base())
# endif
#endif
#if defined GL_HARDLIGHT_KHR
# if defined Hardlight
#  pragma push_macro("Hardlight")
#  undef Hardlight
	 , Hardlight(_base())
#  pragma pop_macro("Hardlight")
# else
	 , Hardlight(_base())
# endif
#endif
#if defined GL_SOFTLIGHT_KHR
# if defined Softlight
#  pragma push_macro("Softlight")
#  undef Softlight
	 , Softlight(_base())
#  pragma pop_macro("Softlight")
# else
	 , Softlight(_base())
# endif
#endif
#if defined GL_DIFFERENCE_KHR
# if defined Difference
#  pragma push_macro("Difference")
#  undef Difference
	 , Difference(_base())
#  pragma pop_macro("Difference")
# else
	 , Difference(_base())
# endif
#endif
#if defined GL_EXCLUSION_KHR
# if defined Exclusion
#  pragma push_macro("Exclusion")
#  undef Exclusion
	 , Exclusion(_base())
#  pragma pop_macro("Exclusion")
# else
	 , Exclusion(_base())
# endif
#endif
#if defined GL_HSL_HUE_KHR
# if defined HSLHue
#  pragma push_macro("HSLHue")
#  undef HSLHue
	 , HSLHue(_base())
#  pragma pop_macro("HSLHue")
# else
	 , HSLHue(_base())
# endif
#endif
#if defined GL_HSL_SATURATION_KHR
# if defined HSLSaturation
#  pragma push_macro("HSLSaturation")
#  undef HSLSaturation
	 , HSLSaturation(_base())
#  pragma pop_macro("HSLSaturation")
# else
	 , HSLSaturation(_base())
# endif
#endif
#if defined GL_HSL_COLOR_KHR
# if defined HSLColor
#  pragma push_macro("HSLColor")
#  undef HSLColor
	 , HSLColor(_base())
#  pragma pop_macro("HSLColor")
# else
	 , HSLColor(_base())
# endif
#endif
#if defined GL_HSL_LUMINOSITY_KHR
# if defined HSLLuminosity
#  pragma push_macro("HSLLuminosity")
#  undef HSLLuminosity
	 , HSLLuminosity(_base())
#  pragma pop_macro("HSLLuminosity")
# else
	 , HSLLuminosity(_base())
# endif
#endif
	{ }
};

} // namespace enums


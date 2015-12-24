//  File implement/oglplus/enums/blend_equation_advanced_range.ipp
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
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLenum*,
	BlendEquationAdvanced
> ValueRange_(BlendEquationAdvanced*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_BLENDEQUATIONADVANCED)
#define OGLPLUS_IMPL_EVR_BLENDEQUATIONADVANCED
{
static const GLenum _values[] = {
#if defined GL_MULTIPLY_KHR
GL_MULTIPLY_KHR,
#endif
#if defined GL_SCREEN_KHR
GL_SCREEN_KHR,
#endif
#if defined GL_OVERLAY_KHR
GL_OVERLAY_KHR,
#endif
#if defined GL_DARKEN_KHR
GL_DARKEN_KHR,
#endif
#if defined GL_LIGHTEN_KHR
GL_LIGHTEN_KHR,
#endif
#if defined GL_COLORDODGE_KHR
GL_COLORDODGE_KHR,
#endif
#if defined GL_COLORBURN_KHR
GL_COLORBURN_KHR,
#endif
#if defined GL_HARDLIGHT_KHR
GL_HARDLIGHT_KHR,
#endif
#if defined GL_SOFTLIGHT_KHR
GL_SOFTLIGHT_KHR,
#endif
#if defined GL_DIFFERENCE_KHR
GL_DIFFERENCE_KHR,
#endif
#if defined GL_EXCLUSION_KHR
GL_EXCLUSION_KHR,
#endif
#if defined GL_HSL_HUE_KHR
GL_HSL_HUE_KHR,
#endif
#if defined GL_HSL_SATURATION_KHR
GL_HSL_SATURATION_KHR,
#endif
#if defined GL_HSL_COLOR_KHR
GL_HSL_COLOR_KHR,
#endif
#if defined GL_HSL_LUMINOSITY_KHR
GL_HSL_LUMINOSITY_KHR,
#endif
0
};
return aux::CastIterRange<
	const GLenum*,
	BlendEquationAdvanced
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


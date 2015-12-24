//  File implement/oglplus/enums/ext/compat_attrib_group_range.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/compat_attrib_group.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLbitfield*,
	CompatibilityAttributeGroup
> ValueRange_(CompatibilityAttributeGroup*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_COMPATIBILITYATTRIBUTEGROUP)
#define OGLPLUS_IMPL_EVR_COMPATIBILITYATTRIBUTEGROUP
{
static const GLbitfield _values[] = {
#if defined GL_ACCUM_BUFFER_BIT
GL_ACCUM_BUFFER_BIT,
#endif
#if defined GL_COLOR_BUFFER_BIT
GL_COLOR_BUFFER_BIT,
#endif
#if defined GL_CURRENT_BIT
GL_CURRENT_BIT,
#endif
#if defined GL_DEPTH_BUFFER_BIT
GL_DEPTH_BUFFER_BIT,
#endif
#if defined GL_ENABLE_BIT
GL_ENABLE_BIT,
#endif
#if defined GL_EVAL_BIT
GL_EVAL_BIT,
#endif
#if defined GL_FOG_BIT
GL_FOG_BIT,
#endif
#if defined GL_HINT_BIT
GL_HINT_BIT,
#endif
#if defined GL_LIGHTING_BIT
GL_LIGHTING_BIT,
#endif
#if defined GL_LINE_BIT
GL_LINE_BIT,
#endif
#if defined GL_LIST_BIT
GL_LIST_BIT,
#endif
#if defined GL_MULTISAMPLE_BIT
GL_MULTISAMPLE_BIT,
#endif
#if defined GL_PIXEL_MODE_BIT
GL_PIXEL_MODE_BIT,
#endif
#if defined GL_POINT_BIT
GL_POINT_BIT,
#endif
#if defined GL_POLYGON_BIT
GL_POLYGON_BIT,
#endif
#if defined GL_POLYGON_STIPPLE_BIT
GL_POLYGON_STIPPLE_BIT,
#endif
#if defined GL_SCISSOR_BIT
GL_SCISSOR_BIT,
#endif
#if defined GL_STENCIL_BUFFER_BIT
GL_STENCIL_BUFFER_BIT,
#endif
#if defined GL_TEXTURE_BIT
GL_TEXTURE_BIT,
#endif
#if defined GL_TRANSFORM_BIT
GL_TRANSFORM_BIT,
#endif
#if defined GL_VIEWPORT_BIT
GL_VIEWPORT_BIT,
#endif
#if defined GL_ALL_ATTRIB_BITS
GL_ALL_ATTRIB_BITS,
#endif
0
};
return aux::CastIterRange<
	const GLbitfield*,
	CompatibilityAttributeGroup
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


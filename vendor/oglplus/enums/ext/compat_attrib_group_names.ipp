//  File implement/oglplus/enums/ext/compat_attrib_group_names.ipp
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
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	CompatibilityAttributeGroup*,
	GLbitfield value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_COMPATIBILITYATTRIBUTEGROUP)
#define OGLPLUS_IMPL_EVN_COMPATIBILITYATTRIBUTEGROUP
{
switch(value)
{
#if defined GL_ACCUM_BUFFER_BIT
	case GL_ACCUM_BUFFER_BIT: return StrCRef("ACCUM_BUFFER_BIT");
#endif
#if defined GL_COLOR_BUFFER_BIT
	case GL_COLOR_BUFFER_BIT: return StrCRef("COLOR_BUFFER_BIT");
#endif
#if defined GL_CURRENT_BIT
	case GL_CURRENT_BIT: return StrCRef("CURRENT_BIT");
#endif
#if defined GL_DEPTH_BUFFER_BIT
	case GL_DEPTH_BUFFER_BIT: return StrCRef("DEPTH_BUFFER_BIT");
#endif
#if defined GL_ENABLE_BIT
	case GL_ENABLE_BIT: return StrCRef("ENABLE_BIT");
#endif
#if defined GL_EVAL_BIT
	case GL_EVAL_BIT: return StrCRef("EVAL_BIT");
#endif
#if defined GL_FOG_BIT
	case GL_FOG_BIT: return StrCRef("FOG_BIT");
#endif
#if defined GL_HINT_BIT
	case GL_HINT_BIT: return StrCRef("HINT_BIT");
#endif
#if defined GL_LIGHTING_BIT
	case GL_LIGHTING_BIT: return StrCRef("LIGHTING_BIT");
#endif
#if defined GL_LINE_BIT
	case GL_LINE_BIT: return StrCRef("LINE_BIT");
#endif
#if defined GL_LIST_BIT
	case GL_LIST_BIT: return StrCRef("LIST_BIT");
#endif
#if defined GL_MULTISAMPLE_BIT
	case GL_MULTISAMPLE_BIT: return StrCRef("MULTISAMPLE_BIT");
#endif
#if defined GL_PIXEL_MODE_BIT
	case GL_PIXEL_MODE_BIT: return StrCRef("PIXEL_MODE_BIT");
#endif
#if defined GL_POINT_BIT
	case GL_POINT_BIT: return StrCRef("POINT_BIT");
#endif
#if defined GL_POLYGON_BIT
	case GL_POLYGON_BIT: return StrCRef("POLYGON_BIT");
#endif
#if defined GL_POLYGON_STIPPLE_BIT
	case GL_POLYGON_STIPPLE_BIT: return StrCRef("POLYGON_STIPPLE_BIT");
#endif
#if defined GL_SCISSOR_BIT
	case GL_SCISSOR_BIT: return StrCRef("SCISSOR_BIT");
#endif
#if defined GL_STENCIL_BUFFER_BIT
	case GL_STENCIL_BUFFER_BIT: return StrCRef("STENCIL_BUFFER_BIT");
#endif
#if defined GL_TEXTURE_BIT
	case GL_TEXTURE_BIT: return StrCRef("TEXTURE_BIT");
#endif
#if defined GL_TRANSFORM_BIT
	case GL_TRANSFORM_BIT: return StrCRef("TRANSFORM_BIT");
#endif
#if defined GL_VIEWPORT_BIT
	case GL_VIEWPORT_BIT: return StrCRef("VIEWPORT_BIT");
#endif
#if defined GL_ALL_ATTRIB_BITS
	case GL_ALL_ATTRIB_BITS: return StrCRef("ALL_ATTRIB_BITS");
#endif
	default:;
}
OGLPLUS_FAKE_USE(value);
return StrCRef();
}
#else
;
#endif
} // namespace enums


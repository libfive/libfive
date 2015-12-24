//  File implement/oglplus/enums/ext/nv_path_command_names.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/nv_path_command.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
OGLPLUS_LIB_FUNC StrCRef ValueName_(
	PathNVCommand*,
	GLubyte value
)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVN_PATHNVCOMMAND)
#define OGLPLUS_IMPL_EVN_PATHNVCOMMAND
{
switch(value)
{
#if defined GL_CLOSE_PATH_NV
	case GL_CLOSE_PATH_NV: return StrCRef("CLOSE_PATH_NV");
#endif
#if defined GL_MOVE_TO_NV
	case GL_MOVE_TO_NV: return StrCRef("MOVE_TO_NV");
#endif
#if defined GL_RELATIVE_MOVE_TO_NV
	case GL_RELATIVE_MOVE_TO_NV: return StrCRef("RELATIVE_MOVE_TO_NV");
#endif
#if defined GL_LINE_TO_NV
	case GL_LINE_TO_NV: return StrCRef("LINE_TO_NV");
#endif
#if defined GL_RELATIVE_LINE_TO_NV
	case GL_RELATIVE_LINE_TO_NV: return StrCRef("RELATIVE_LINE_TO_NV");
#endif
#if defined GL_HORIZONTAL_LINE_TO_NV
	case GL_HORIZONTAL_LINE_TO_NV: return StrCRef("HORIZONTAL_LINE_TO_NV");
#endif
#if defined GL_RELATIVE_HORIZONTAL_LINE_TO_NV
	case GL_RELATIVE_HORIZONTAL_LINE_TO_NV: return StrCRef("RELATIVE_HORIZONTAL_LINE_TO_NV");
#endif
#if defined GL_VERTICAL_LINE_TO_NV
	case GL_VERTICAL_LINE_TO_NV: return StrCRef("VERTICAL_LINE_TO_NV");
#endif
#if defined GL_RELATIVE_VERTICAL_LINE_TO_NV
	case GL_RELATIVE_VERTICAL_LINE_TO_NV: return StrCRef("RELATIVE_VERTICAL_LINE_TO_NV");
#endif
#if defined GL_QUADRATIC_CURVE_TO_NV
	case GL_QUADRATIC_CURVE_TO_NV: return StrCRef("QUADRATIC_CURVE_TO_NV");
#endif
#if defined GL_RELATIVE_QUADRATIC_CURVE_TO_NV
	case GL_RELATIVE_QUADRATIC_CURVE_TO_NV: return StrCRef("RELATIVE_QUADRATIC_CURVE_TO_NV");
#endif
#if defined GL_CUBIC_CURVE_TO_NV
	case GL_CUBIC_CURVE_TO_NV: return StrCRef("CUBIC_CURVE_TO_NV");
#endif
#if defined GL_RELATIVE_CUBIC_CURVE_TO_NV
	case GL_RELATIVE_CUBIC_CURVE_TO_NV: return StrCRef("RELATIVE_CUBIC_CURVE_TO_NV");
#endif
#if defined GL_SMOOTH_QUADRATIC_CURVE_TO_NV
	case GL_SMOOTH_QUADRATIC_CURVE_TO_NV: return StrCRef("SMOOTH_QUADRATIC_CURVE_TO_NV");
#endif
#if defined GL_RELATIVE_SMOOTH_QUADRATIC_CURVE_TO_NV
	case GL_RELATIVE_SMOOTH_QUADRATIC_CURVE_TO_NV: return StrCRef("RELATIVE_SMOOTH_QUADRATIC_CURVE_TO_NV");
#endif
#if defined GL_SMOOTH_CUBIC_CURVE_TO_NV
	case GL_SMOOTH_CUBIC_CURVE_TO_NV: return StrCRef("SMOOTH_CUBIC_CURVE_TO_NV");
#endif
#if defined GL_RELATIVE_SMOOTH_CUBIC_CURVE_TO_NV
	case GL_RELATIVE_SMOOTH_CUBIC_CURVE_TO_NV: return StrCRef("RELATIVE_SMOOTH_CUBIC_CURVE_TO_NV");
#endif
#if defined GL_SMALL_CCW_ARC_TO_NV
	case GL_SMALL_CCW_ARC_TO_NV: return StrCRef("SMALL_CCW_ARC_TO_NV");
#endif
#if defined GL_RELATIVE_SMALL_CCW_ARC_TO_NV
	case GL_RELATIVE_SMALL_CCW_ARC_TO_NV: return StrCRef("RELATIVE_SMALL_CCW_ARC_TO_NV");
#endif
#if defined GL_SMALL_CW_ARC_TO_NV
	case GL_SMALL_CW_ARC_TO_NV: return StrCRef("SMALL_CW_ARC_TO_NV");
#endif
#if defined GL_RELATIVE_SMALL_CW_ARC_TO_NV
	case GL_RELATIVE_SMALL_CW_ARC_TO_NV: return StrCRef("RELATIVE_SMALL_CW_ARC_TO_NV");
#endif
#if defined GL_LARGE_CCW_ARC_TO_NV
	case GL_LARGE_CCW_ARC_TO_NV: return StrCRef("LARGE_CCW_ARC_TO_NV");
#endif
#if defined GL_RELATIVE_LARGE_CCW_ARC_TO_NV
	case GL_RELATIVE_LARGE_CCW_ARC_TO_NV: return StrCRef("RELATIVE_LARGE_CCW_ARC_TO_NV");
#endif
#if defined GL_LARGE_CW_ARC_TO_NV
	case GL_LARGE_CW_ARC_TO_NV: return StrCRef("LARGE_CW_ARC_TO_NV");
#endif
#if defined GL_RELATIVE_LARGE_CW_ARC_TO_NV
	case GL_RELATIVE_LARGE_CW_ARC_TO_NV: return StrCRef("RELATIVE_LARGE_CW_ARC_TO_NV");
#endif
#if defined GL_RESTART_PATH_NV
	case GL_RESTART_PATH_NV: return StrCRef("RESTART_PATH_NV");
#endif
#if defined GL_DUP_FIRST_CUBIC_CURVE_TO_NV
	case GL_DUP_FIRST_CUBIC_CURVE_TO_NV: return StrCRef("DUP_FIRST_CUBIC_CURVE_TO_NV");
#endif
#if defined GL_DUP_LAST_CUBIC_CURVE_TO_NV
	case GL_DUP_LAST_CUBIC_CURVE_TO_NV: return StrCRef("DUP_LAST_CUBIC_CURVE_TO_NV");
#endif
#if defined GL_RECT_NV
	case GL_RECT_NV: return StrCRef("RECT_NV");
#endif
#if defined GL_CIRCULAR_CCW_ARC_TO_NV
	case GL_CIRCULAR_CCW_ARC_TO_NV: return StrCRef("CIRCULAR_CCW_ARC_TO_NV");
#endif
#if defined GL_CIRCULAR_CW_ARC_TO_NV
	case GL_CIRCULAR_CW_ARC_TO_NV: return StrCRef("CIRCULAR_CW_ARC_TO_NV");
#endif
#if defined GL_CIRCULAR_TANGENT_ARC_TO_NV
	case GL_CIRCULAR_TANGENT_ARC_TO_NV: return StrCRef("CIRCULAR_TANGENT_ARC_TO_NV");
#endif
#if defined GL_ARC_TO_NV
	case GL_ARC_TO_NV: return StrCRef("ARC_TO_NV");
#endif
#if defined GL_RELATIVE_ARC_TO_NV
	case GL_RELATIVE_ARC_TO_NV: return StrCRef("RELATIVE_ARC_TO_NV");
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


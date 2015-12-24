//  File implement/oglplus/enums/ext/nv_path_command_range.ipp
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
OGLPLUS_LIB_FUNC aux::CastIterRange<
	const GLubyte*,
	PathNVCommand
> ValueRange_(PathNVCommand*)
#if (!OGLPLUS_LINK_LIBRARY || defined(OGLPLUS_IMPLEMENTING_LIBRARY)) && \
	!defined(OGLPLUS_IMPL_EVR_PATHNVCOMMAND)
#define OGLPLUS_IMPL_EVR_PATHNVCOMMAND
{
static const GLubyte _values[] = {
#if defined GL_CLOSE_PATH_NV
GL_CLOSE_PATH_NV,
#endif
#if defined GL_MOVE_TO_NV
GL_MOVE_TO_NV,
#endif
#if defined GL_RELATIVE_MOVE_TO_NV
GL_RELATIVE_MOVE_TO_NV,
#endif
#if defined GL_LINE_TO_NV
GL_LINE_TO_NV,
#endif
#if defined GL_RELATIVE_LINE_TO_NV
GL_RELATIVE_LINE_TO_NV,
#endif
#if defined GL_HORIZONTAL_LINE_TO_NV
GL_HORIZONTAL_LINE_TO_NV,
#endif
#if defined GL_RELATIVE_HORIZONTAL_LINE_TO_NV
GL_RELATIVE_HORIZONTAL_LINE_TO_NV,
#endif
#if defined GL_VERTICAL_LINE_TO_NV
GL_VERTICAL_LINE_TO_NV,
#endif
#if defined GL_RELATIVE_VERTICAL_LINE_TO_NV
GL_RELATIVE_VERTICAL_LINE_TO_NV,
#endif
#if defined GL_QUADRATIC_CURVE_TO_NV
GL_QUADRATIC_CURVE_TO_NV,
#endif
#if defined GL_RELATIVE_QUADRATIC_CURVE_TO_NV
GL_RELATIVE_QUADRATIC_CURVE_TO_NV,
#endif
#if defined GL_CUBIC_CURVE_TO_NV
GL_CUBIC_CURVE_TO_NV,
#endif
#if defined GL_RELATIVE_CUBIC_CURVE_TO_NV
GL_RELATIVE_CUBIC_CURVE_TO_NV,
#endif
#if defined GL_SMOOTH_QUADRATIC_CURVE_TO_NV
GL_SMOOTH_QUADRATIC_CURVE_TO_NV,
#endif
#if defined GL_RELATIVE_SMOOTH_QUADRATIC_CURVE_TO_NV
GL_RELATIVE_SMOOTH_QUADRATIC_CURVE_TO_NV,
#endif
#if defined GL_SMOOTH_CUBIC_CURVE_TO_NV
GL_SMOOTH_CUBIC_CURVE_TO_NV,
#endif
#if defined GL_RELATIVE_SMOOTH_CUBIC_CURVE_TO_NV
GL_RELATIVE_SMOOTH_CUBIC_CURVE_TO_NV,
#endif
#if defined GL_SMALL_CCW_ARC_TO_NV
GL_SMALL_CCW_ARC_TO_NV,
#endif
#if defined GL_RELATIVE_SMALL_CCW_ARC_TO_NV
GL_RELATIVE_SMALL_CCW_ARC_TO_NV,
#endif
#if defined GL_SMALL_CW_ARC_TO_NV
GL_SMALL_CW_ARC_TO_NV,
#endif
#if defined GL_RELATIVE_SMALL_CW_ARC_TO_NV
GL_RELATIVE_SMALL_CW_ARC_TO_NV,
#endif
#if defined GL_LARGE_CCW_ARC_TO_NV
GL_LARGE_CCW_ARC_TO_NV,
#endif
#if defined GL_RELATIVE_LARGE_CCW_ARC_TO_NV
GL_RELATIVE_LARGE_CCW_ARC_TO_NV,
#endif
#if defined GL_LARGE_CW_ARC_TO_NV
GL_LARGE_CW_ARC_TO_NV,
#endif
#if defined GL_RELATIVE_LARGE_CW_ARC_TO_NV
GL_RELATIVE_LARGE_CW_ARC_TO_NV,
#endif
#if defined GL_RESTART_PATH_NV
GL_RESTART_PATH_NV,
#endif
#if defined GL_DUP_FIRST_CUBIC_CURVE_TO_NV
GL_DUP_FIRST_CUBIC_CURVE_TO_NV,
#endif
#if defined GL_DUP_LAST_CUBIC_CURVE_TO_NV
GL_DUP_LAST_CUBIC_CURVE_TO_NV,
#endif
#if defined GL_RECT_NV
GL_RECT_NV,
#endif
#if defined GL_CIRCULAR_CCW_ARC_TO_NV
GL_CIRCULAR_CCW_ARC_TO_NV,
#endif
#if defined GL_CIRCULAR_CW_ARC_TO_NV
GL_CIRCULAR_CW_ARC_TO_NV,
#endif
#if defined GL_CIRCULAR_TANGENT_ARC_TO_NV
GL_CIRCULAR_TANGENT_ARC_TO_NV,
#endif
#if defined GL_ARC_TO_NV
GL_ARC_TO_NV,
#endif
#if defined GL_RELATIVE_ARC_TO_NV
GL_RELATIVE_ARC_TO_NV,
#endif
0
};
return aux::CastIterRange<
	const GLubyte*,
	PathNVCommand
>(_values, _values+sizeof(_values)/sizeof(_values[0])-1);
}
#else
;
#endif
} // namespace enums


//  File implement/oglplus/enums/ext/nv_path_command_class.ipp
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
template <typename Base, template<PathNVCommand> class Transform>
class EnumToClass<Base, PathNVCommand, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_CLOSE_PATH_NV
# if defined Close
#  pragma push_macro("Close")
#  undef Close
	Transform<PathNVCommand::Close> Close;
#  pragma pop_macro("Close")
# else
	Transform<PathNVCommand::Close> Close;
# endif
#endif
#if defined GL_MOVE_TO_NV
# if defined MoveTo
#  pragma push_macro("MoveTo")
#  undef MoveTo
	Transform<PathNVCommand::MoveTo> MoveTo;
#  pragma pop_macro("MoveTo")
# else
	Transform<PathNVCommand::MoveTo> MoveTo;
# endif
#endif
#if defined GL_RELATIVE_MOVE_TO_NV
# if defined RelativeMoveTo
#  pragma push_macro("RelativeMoveTo")
#  undef RelativeMoveTo
	Transform<PathNVCommand::RelativeMoveTo> RelativeMoveTo;
#  pragma pop_macro("RelativeMoveTo")
# else
	Transform<PathNVCommand::RelativeMoveTo> RelativeMoveTo;
# endif
#endif
#if defined GL_LINE_TO_NV
# if defined LineTo
#  pragma push_macro("LineTo")
#  undef LineTo
	Transform<PathNVCommand::LineTo> LineTo;
#  pragma pop_macro("LineTo")
# else
	Transform<PathNVCommand::LineTo> LineTo;
# endif
#endif
#if defined GL_RELATIVE_LINE_TO_NV
# if defined RelativeLineTo
#  pragma push_macro("RelativeLineTo")
#  undef RelativeLineTo
	Transform<PathNVCommand::RelativeLineTo> RelativeLineTo;
#  pragma pop_macro("RelativeLineTo")
# else
	Transform<PathNVCommand::RelativeLineTo> RelativeLineTo;
# endif
#endif
#if defined GL_HORIZONTAL_LINE_TO_NV
# if defined HorizontalLineTo
#  pragma push_macro("HorizontalLineTo")
#  undef HorizontalLineTo
	Transform<PathNVCommand::HorizontalLineTo> HorizontalLineTo;
#  pragma pop_macro("HorizontalLineTo")
# else
	Transform<PathNVCommand::HorizontalLineTo> HorizontalLineTo;
# endif
#endif
#if defined GL_RELATIVE_HORIZONTAL_LINE_TO_NV
# if defined RelativeHorizontalLineTo
#  pragma push_macro("RelativeHorizontalLineTo")
#  undef RelativeHorizontalLineTo
	Transform<PathNVCommand::RelativeHorizontalLineTo> RelativeHorizontalLineTo;
#  pragma pop_macro("RelativeHorizontalLineTo")
# else
	Transform<PathNVCommand::RelativeHorizontalLineTo> RelativeHorizontalLineTo;
# endif
#endif
#if defined GL_VERTICAL_LINE_TO_NV
# if defined VerticalLineTo
#  pragma push_macro("VerticalLineTo")
#  undef VerticalLineTo
	Transform<PathNVCommand::VerticalLineTo> VerticalLineTo;
#  pragma pop_macro("VerticalLineTo")
# else
	Transform<PathNVCommand::VerticalLineTo> VerticalLineTo;
# endif
#endif
#if defined GL_RELATIVE_VERTICAL_LINE_TO_NV
# if defined RelativeVerticalLineTo
#  pragma push_macro("RelativeVerticalLineTo")
#  undef RelativeVerticalLineTo
	Transform<PathNVCommand::RelativeVerticalLineTo> RelativeVerticalLineTo;
#  pragma pop_macro("RelativeVerticalLineTo")
# else
	Transform<PathNVCommand::RelativeVerticalLineTo> RelativeVerticalLineTo;
# endif
#endif
#if defined GL_QUADRATIC_CURVE_TO_NV
# if defined QuadraticCurveTo
#  pragma push_macro("QuadraticCurveTo")
#  undef QuadraticCurveTo
	Transform<PathNVCommand::QuadraticCurveTo> QuadraticCurveTo;
#  pragma pop_macro("QuadraticCurveTo")
# else
	Transform<PathNVCommand::QuadraticCurveTo> QuadraticCurveTo;
# endif
#endif
#if defined GL_RELATIVE_QUADRATIC_CURVE_TO_NV
# if defined RelativeQuadraticCurveTo
#  pragma push_macro("RelativeQuadraticCurveTo")
#  undef RelativeQuadraticCurveTo
	Transform<PathNVCommand::RelativeQuadraticCurveTo> RelativeQuadraticCurveTo;
#  pragma pop_macro("RelativeQuadraticCurveTo")
# else
	Transform<PathNVCommand::RelativeQuadraticCurveTo> RelativeQuadraticCurveTo;
# endif
#endif
#if defined GL_CUBIC_CURVE_TO_NV
# if defined CubicCurveTo
#  pragma push_macro("CubicCurveTo")
#  undef CubicCurveTo
	Transform<PathNVCommand::CubicCurveTo> CubicCurveTo;
#  pragma pop_macro("CubicCurveTo")
# else
	Transform<PathNVCommand::CubicCurveTo> CubicCurveTo;
# endif
#endif
#if defined GL_RELATIVE_CUBIC_CURVE_TO_NV
# if defined RelativeCubicCurveTo
#  pragma push_macro("RelativeCubicCurveTo")
#  undef RelativeCubicCurveTo
	Transform<PathNVCommand::RelativeCubicCurveTo> RelativeCubicCurveTo;
#  pragma pop_macro("RelativeCubicCurveTo")
# else
	Transform<PathNVCommand::RelativeCubicCurveTo> RelativeCubicCurveTo;
# endif
#endif
#if defined GL_SMOOTH_QUADRATIC_CURVE_TO_NV
# if defined SmoothQuadraticCurveTo
#  pragma push_macro("SmoothQuadraticCurveTo")
#  undef SmoothQuadraticCurveTo
	Transform<PathNVCommand::SmoothQuadraticCurveTo> SmoothQuadraticCurveTo;
#  pragma pop_macro("SmoothQuadraticCurveTo")
# else
	Transform<PathNVCommand::SmoothQuadraticCurveTo> SmoothQuadraticCurveTo;
# endif
#endif
#if defined GL_RELATIVE_SMOOTH_QUADRATIC_CURVE_TO_NV
# if defined RelativeSmoothQuadraticCurveTo
#  pragma push_macro("RelativeSmoothQuadraticCurveTo")
#  undef RelativeSmoothQuadraticCurveTo
	Transform<PathNVCommand::RelativeSmoothQuadraticCurveTo> RelativeSmoothQuadraticCurveTo;
#  pragma pop_macro("RelativeSmoothQuadraticCurveTo")
# else
	Transform<PathNVCommand::RelativeSmoothQuadraticCurveTo> RelativeSmoothQuadraticCurveTo;
# endif
#endif
#if defined GL_SMOOTH_CUBIC_CURVE_TO_NV
# if defined SmoothCubicCurveTo
#  pragma push_macro("SmoothCubicCurveTo")
#  undef SmoothCubicCurveTo
	Transform<PathNVCommand::SmoothCubicCurveTo> SmoothCubicCurveTo;
#  pragma pop_macro("SmoothCubicCurveTo")
# else
	Transform<PathNVCommand::SmoothCubicCurveTo> SmoothCubicCurveTo;
# endif
#endif
#if defined GL_RELATIVE_SMOOTH_CUBIC_CURVE_TO_NV
# if defined RelativeSmoothCubicCurveTo
#  pragma push_macro("RelativeSmoothCubicCurveTo")
#  undef RelativeSmoothCubicCurveTo
	Transform<PathNVCommand::RelativeSmoothCubicCurveTo> RelativeSmoothCubicCurveTo;
#  pragma pop_macro("RelativeSmoothCubicCurveTo")
# else
	Transform<PathNVCommand::RelativeSmoothCubicCurveTo> RelativeSmoothCubicCurveTo;
# endif
#endif
#if defined GL_SMALL_CCW_ARC_TO_NV
# if defined SmallCCWArcTo
#  pragma push_macro("SmallCCWArcTo")
#  undef SmallCCWArcTo
	Transform<PathNVCommand::SmallCCWArcTo> SmallCCWArcTo;
#  pragma pop_macro("SmallCCWArcTo")
# else
	Transform<PathNVCommand::SmallCCWArcTo> SmallCCWArcTo;
# endif
#endif
#if defined GL_RELATIVE_SMALL_CCW_ARC_TO_NV
# if defined RelativeSmallCCWArcTo
#  pragma push_macro("RelativeSmallCCWArcTo")
#  undef RelativeSmallCCWArcTo
	Transform<PathNVCommand::RelativeSmallCCWArcTo> RelativeSmallCCWArcTo;
#  pragma pop_macro("RelativeSmallCCWArcTo")
# else
	Transform<PathNVCommand::RelativeSmallCCWArcTo> RelativeSmallCCWArcTo;
# endif
#endif
#if defined GL_SMALL_CW_ARC_TO_NV
# if defined SmallCWArcTo
#  pragma push_macro("SmallCWArcTo")
#  undef SmallCWArcTo
	Transform<PathNVCommand::SmallCWArcTo> SmallCWArcTo;
#  pragma pop_macro("SmallCWArcTo")
# else
	Transform<PathNVCommand::SmallCWArcTo> SmallCWArcTo;
# endif
#endif
#if defined GL_RELATIVE_SMALL_CW_ARC_TO_NV
# if defined RelativeSmallCWArcTo
#  pragma push_macro("RelativeSmallCWArcTo")
#  undef RelativeSmallCWArcTo
	Transform<PathNVCommand::RelativeSmallCWArcTo> RelativeSmallCWArcTo;
#  pragma pop_macro("RelativeSmallCWArcTo")
# else
	Transform<PathNVCommand::RelativeSmallCWArcTo> RelativeSmallCWArcTo;
# endif
#endif
#if defined GL_LARGE_CCW_ARC_TO_NV
# if defined LargeCCWArcTo
#  pragma push_macro("LargeCCWArcTo")
#  undef LargeCCWArcTo
	Transform<PathNVCommand::LargeCCWArcTo> LargeCCWArcTo;
#  pragma pop_macro("LargeCCWArcTo")
# else
	Transform<PathNVCommand::LargeCCWArcTo> LargeCCWArcTo;
# endif
#endif
#if defined GL_RELATIVE_LARGE_CCW_ARC_TO_NV
# if defined RelativeLargeCCWArcTo
#  pragma push_macro("RelativeLargeCCWArcTo")
#  undef RelativeLargeCCWArcTo
	Transform<PathNVCommand::RelativeLargeCCWArcTo> RelativeLargeCCWArcTo;
#  pragma pop_macro("RelativeLargeCCWArcTo")
# else
	Transform<PathNVCommand::RelativeLargeCCWArcTo> RelativeLargeCCWArcTo;
# endif
#endif
#if defined GL_LARGE_CW_ARC_TO_NV
# if defined LargeCWArcTo
#  pragma push_macro("LargeCWArcTo")
#  undef LargeCWArcTo
	Transform<PathNVCommand::LargeCWArcTo> LargeCWArcTo;
#  pragma pop_macro("LargeCWArcTo")
# else
	Transform<PathNVCommand::LargeCWArcTo> LargeCWArcTo;
# endif
#endif
#if defined GL_RELATIVE_LARGE_CW_ARC_TO_NV
# if defined RelativeLargeCWArcTo
#  pragma push_macro("RelativeLargeCWArcTo")
#  undef RelativeLargeCWArcTo
	Transform<PathNVCommand::RelativeLargeCWArcTo> RelativeLargeCWArcTo;
#  pragma pop_macro("RelativeLargeCWArcTo")
# else
	Transform<PathNVCommand::RelativeLargeCWArcTo> RelativeLargeCWArcTo;
# endif
#endif
#if defined GL_RESTART_PATH_NV
# if defined Restart
#  pragma push_macro("Restart")
#  undef Restart
	Transform<PathNVCommand::Restart> Restart;
#  pragma pop_macro("Restart")
# else
	Transform<PathNVCommand::Restart> Restart;
# endif
#endif
#if defined GL_DUP_FIRST_CUBIC_CURVE_TO_NV
# if defined DupFirstCubicCurveTo
#  pragma push_macro("DupFirstCubicCurveTo")
#  undef DupFirstCubicCurveTo
	Transform<PathNVCommand::DupFirstCubicCurveTo> DupFirstCubicCurveTo;
#  pragma pop_macro("DupFirstCubicCurveTo")
# else
	Transform<PathNVCommand::DupFirstCubicCurveTo> DupFirstCubicCurveTo;
# endif
#endif
#if defined GL_DUP_LAST_CUBIC_CURVE_TO_NV
# if defined DupLastCubicCurveTo
#  pragma push_macro("DupLastCubicCurveTo")
#  undef DupLastCubicCurveTo
	Transform<PathNVCommand::DupLastCubicCurveTo> DupLastCubicCurveTo;
#  pragma pop_macro("DupLastCubicCurveTo")
# else
	Transform<PathNVCommand::DupLastCubicCurveTo> DupLastCubicCurveTo;
# endif
#endif
#if defined GL_RECT_NV
# if defined Rect
#  pragma push_macro("Rect")
#  undef Rect
	Transform<PathNVCommand::Rect> Rect;
#  pragma pop_macro("Rect")
# else
	Transform<PathNVCommand::Rect> Rect;
# endif
#endif
#if defined GL_CIRCULAR_CCW_ARC_TO_NV
# if defined CircularCCWArcTo
#  pragma push_macro("CircularCCWArcTo")
#  undef CircularCCWArcTo
	Transform<PathNVCommand::CircularCCWArcTo> CircularCCWArcTo;
#  pragma pop_macro("CircularCCWArcTo")
# else
	Transform<PathNVCommand::CircularCCWArcTo> CircularCCWArcTo;
# endif
#endif
#if defined GL_CIRCULAR_CW_ARC_TO_NV
# if defined CircularCWArcTo
#  pragma push_macro("CircularCWArcTo")
#  undef CircularCWArcTo
	Transform<PathNVCommand::CircularCWArcTo> CircularCWArcTo;
#  pragma pop_macro("CircularCWArcTo")
# else
	Transform<PathNVCommand::CircularCWArcTo> CircularCWArcTo;
# endif
#endif
#if defined GL_CIRCULAR_TANGENT_ARC_TO_NV
# if defined CircularTangentArcTo
#  pragma push_macro("CircularTangentArcTo")
#  undef CircularTangentArcTo
	Transform<PathNVCommand::CircularTangentArcTo> CircularTangentArcTo;
#  pragma pop_macro("CircularTangentArcTo")
# else
	Transform<PathNVCommand::CircularTangentArcTo> CircularTangentArcTo;
# endif
#endif
#if defined GL_ARC_TO_NV
# if defined ArcTo
#  pragma push_macro("ArcTo")
#  undef ArcTo
	Transform<PathNVCommand::ArcTo> ArcTo;
#  pragma pop_macro("ArcTo")
# else
	Transform<PathNVCommand::ArcTo> ArcTo;
# endif
#endif
#if defined GL_RELATIVE_ARC_TO_NV
# if defined RelativeArcTo
#  pragma push_macro("RelativeArcTo")
#  undef RelativeArcTo
	Transform<PathNVCommand::RelativeArcTo> RelativeArcTo;
#  pragma pop_macro("RelativeArcTo")
# else
	Transform<PathNVCommand::RelativeArcTo> RelativeArcTo;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_CLOSE_PATH_NV
# if defined Close
#  pragma push_macro("Close")
#  undef Close
	 , Close(_base())
#  pragma pop_macro("Close")
# else
	 , Close(_base())
# endif
#endif
#if defined GL_MOVE_TO_NV
# if defined MoveTo
#  pragma push_macro("MoveTo")
#  undef MoveTo
	 , MoveTo(_base())
#  pragma pop_macro("MoveTo")
# else
	 , MoveTo(_base())
# endif
#endif
#if defined GL_RELATIVE_MOVE_TO_NV
# if defined RelativeMoveTo
#  pragma push_macro("RelativeMoveTo")
#  undef RelativeMoveTo
	 , RelativeMoveTo(_base())
#  pragma pop_macro("RelativeMoveTo")
# else
	 , RelativeMoveTo(_base())
# endif
#endif
#if defined GL_LINE_TO_NV
# if defined LineTo
#  pragma push_macro("LineTo")
#  undef LineTo
	 , LineTo(_base())
#  pragma pop_macro("LineTo")
# else
	 , LineTo(_base())
# endif
#endif
#if defined GL_RELATIVE_LINE_TO_NV
# if defined RelativeLineTo
#  pragma push_macro("RelativeLineTo")
#  undef RelativeLineTo
	 , RelativeLineTo(_base())
#  pragma pop_macro("RelativeLineTo")
# else
	 , RelativeLineTo(_base())
# endif
#endif
#if defined GL_HORIZONTAL_LINE_TO_NV
# if defined HorizontalLineTo
#  pragma push_macro("HorizontalLineTo")
#  undef HorizontalLineTo
	 , HorizontalLineTo(_base())
#  pragma pop_macro("HorizontalLineTo")
# else
	 , HorizontalLineTo(_base())
# endif
#endif
#if defined GL_RELATIVE_HORIZONTAL_LINE_TO_NV
# if defined RelativeHorizontalLineTo
#  pragma push_macro("RelativeHorizontalLineTo")
#  undef RelativeHorizontalLineTo
	 , RelativeHorizontalLineTo(_base())
#  pragma pop_macro("RelativeHorizontalLineTo")
# else
	 , RelativeHorizontalLineTo(_base())
# endif
#endif
#if defined GL_VERTICAL_LINE_TO_NV
# if defined VerticalLineTo
#  pragma push_macro("VerticalLineTo")
#  undef VerticalLineTo
	 , VerticalLineTo(_base())
#  pragma pop_macro("VerticalLineTo")
# else
	 , VerticalLineTo(_base())
# endif
#endif
#if defined GL_RELATIVE_VERTICAL_LINE_TO_NV
# if defined RelativeVerticalLineTo
#  pragma push_macro("RelativeVerticalLineTo")
#  undef RelativeVerticalLineTo
	 , RelativeVerticalLineTo(_base())
#  pragma pop_macro("RelativeVerticalLineTo")
# else
	 , RelativeVerticalLineTo(_base())
# endif
#endif
#if defined GL_QUADRATIC_CURVE_TO_NV
# if defined QuadraticCurveTo
#  pragma push_macro("QuadraticCurveTo")
#  undef QuadraticCurveTo
	 , QuadraticCurveTo(_base())
#  pragma pop_macro("QuadraticCurveTo")
# else
	 , QuadraticCurveTo(_base())
# endif
#endif
#if defined GL_RELATIVE_QUADRATIC_CURVE_TO_NV
# if defined RelativeQuadraticCurveTo
#  pragma push_macro("RelativeQuadraticCurveTo")
#  undef RelativeQuadraticCurveTo
	 , RelativeQuadraticCurveTo(_base())
#  pragma pop_macro("RelativeQuadraticCurveTo")
# else
	 , RelativeQuadraticCurveTo(_base())
# endif
#endif
#if defined GL_CUBIC_CURVE_TO_NV
# if defined CubicCurveTo
#  pragma push_macro("CubicCurveTo")
#  undef CubicCurveTo
	 , CubicCurveTo(_base())
#  pragma pop_macro("CubicCurveTo")
# else
	 , CubicCurveTo(_base())
# endif
#endif
#if defined GL_RELATIVE_CUBIC_CURVE_TO_NV
# if defined RelativeCubicCurveTo
#  pragma push_macro("RelativeCubicCurveTo")
#  undef RelativeCubicCurveTo
	 , RelativeCubicCurveTo(_base())
#  pragma pop_macro("RelativeCubicCurveTo")
# else
	 , RelativeCubicCurveTo(_base())
# endif
#endif
#if defined GL_SMOOTH_QUADRATIC_CURVE_TO_NV
# if defined SmoothQuadraticCurveTo
#  pragma push_macro("SmoothQuadraticCurveTo")
#  undef SmoothQuadraticCurveTo
	 , SmoothQuadraticCurveTo(_base())
#  pragma pop_macro("SmoothQuadraticCurveTo")
# else
	 , SmoothQuadraticCurveTo(_base())
# endif
#endif
#if defined GL_RELATIVE_SMOOTH_QUADRATIC_CURVE_TO_NV
# if defined RelativeSmoothQuadraticCurveTo
#  pragma push_macro("RelativeSmoothQuadraticCurveTo")
#  undef RelativeSmoothQuadraticCurveTo
	 , RelativeSmoothQuadraticCurveTo(_base())
#  pragma pop_macro("RelativeSmoothQuadraticCurveTo")
# else
	 , RelativeSmoothQuadraticCurveTo(_base())
# endif
#endif
#if defined GL_SMOOTH_CUBIC_CURVE_TO_NV
# if defined SmoothCubicCurveTo
#  pragma push_macro("SmoothCubicCurveTo")
#  undef SmoothCubicCurveTo
	 , SmoothCubicCurveTo(_base())
#  pragma pop_macro("SmoothCubicCurveTo")
# else
	 , SmoothCubicCurveTo(_base())
# endif
#endif
#if defined GL_RELATIVE_SMOOTH_CUBIC_CURVE_TO_NV
# if defined RelativeSmoothCubicCurveTo
#  pragma push_macro("RelativeSmoothCubicCurveTo")
#  undef RelativeSmoothCubicCurveTo
	 , RelativeSmoothCubicCurveTo(_base())
#  pragma pop_macro("RelativeSmoothCubicCurveTo")
# else
	 , RelativeSmoothCubicCurveTo(_base())
# endif
#endif
#if defined GL_SMALL_CCW_ARC_TO_NV
# if defined SmallCCWArcTo
#  pragma push_macro("SmallCCWArcTo")
#  undef SmallCCWArcTo
	 , SmallCCWArcTo(_base())
#  pragma pop_macro("SmallCCWArcTo")
# else
	 , SmallCCWArcTo(_base())
# endif
#endif
#if defined GL_RELATIVE_SMALL_CCW_ARC_TO_NV
# if defined RelativeSmallCCWArcTo
#  pragma push_macro("RelativeSmallCCWArcTo")
#  undef RelativeSmallCCWArcTo
	 , RelativeSmallCCWArcTo(_base())
#  pragma pop_macro("RelativeSmallCCWArcTo")
# else
	 , RelativeSmallCCWArcTo(_base())
# endif
#endif
#if defined GL_SMALL_CW_ARC_TO_NV
# if defined SmallCWArcTo
#  pragma push_macro("SmallCWArcTo")
#  undef SmallCWArcTo
	 , SmallCWArcTo(_base())
#  pragma pop_macro("SmallCWArcTo")
# else
	 , SmallCWArcTo(_base())
# endif
#endif
#if defined GL_RELATIVE_SMALL_CW_ARC_TO_NV
# if defined RelativeSmallCWArcTo
#  pragma push_macro("RelativeSmallCWArcTo")
#  undef RelativeSmallCWArcTo
	 , RelativeSmallCWArcTo(_base())
#  pragma pop_macro("RelativeSmallCWArcTo")
# else
	 , RelativeSmallCWArcTo(_base())
# endif
#endif
#if defined GL_LARGE_CCW_ARC_TO_NV
# if defined LargeCCWArcTo
#  pragma push_macro("LargeCCWArcTo")
#  undef LargeCCWArcTo
	 , LargeCCWArcTo(_base())
#  pragma pop_macro("LargeCCWArcTo")
# else
	 , LargeCCWArcTo(_base())
# endif
#endif
#if defined GL_RELATIVE_LARGE_CCW_ARC_TO_NV
# if defined RelativeLargeCCWArcTo
#  pragma push_macro("RelativeLargeCCWArcTo")
#  undef RelativeLargeCCWArcTo
	 , RelativeLargeCCWArcTo(_base())
#  pragma pop_macro("RelativeLargeCCWArcTo")
# else
	 , RelativeLargeCCWArcTo(_base())
# endif
#endif
#if defined GL_LARGE_CW_ARC_TO_NV
# if defined LargeCWArcTo
#  pragma push_macro("LargeCWArcTo")
#  undef LargeCWArcTo
	 , LargeCWArcTo(_base())
#  pragma pop_macro("LargeCWArcTo")
# else
	 , LargeCWArcTo(_base())
# endif
#endif
#if defined GL_RELATIVE_LARGE_CW_ARC_TO_NV
# if defined RelativeLargeCWArcTo
#  pragma push_macro("RelativeLargeCWArcTo")
#  undef RelativeLargeCWArcTo
	 , RelativeLargeCWArcTo(_base())
#  pragma pop_macro("RelativeLargeCWArcTo")
# else
	 , RelativeLargeCWArcTo(_base())
# endif
#endif
#if defined GL_RESTART_PATH_NV
# if defined Restart
#  pragma push_macro("Restart")
#  undef Restart
	 , Restart(_base())
#  pragma pop_macro("Restart")
# else
	 , Restart(_base())
# endif
#endif
#if defined GL_DUP_FIRST_CUBIC_CURVE_TO_NV
# if defined DupFirstCubicCurveTo
#  pragma push_macro("DupFirstCubicCurveTo")
#  undef DupFirstCubicCurveTo
	 , DupFirstCubicCurveTo(_base())
#  pragma pop_macro("DupFirstCubicCurveTo")
# else
	 , DupFirstCubicCurveTo(_base())
# endif
#endif
#if defined GL_DUP_LAST_CUBIC_CURVE_TO_NV
# if defined DupLastCubicCurveTo
#  pragma push_macro("DupLastCubicCurveTo")
#  undef DupLastCubicCurveTo
	 , DupLastCubicCurveTo(_base())
#  pragma pop_macro("DupLastCubicCurveTo")
# else
	 , DupLastCubicCurveTo(_base())
# endif
#endif
#if defined GL_RECT_NV
# if defined Rect
#  pragma push_macro("Rect")
#  undef Rect
	 , Rect(_base())
#  pragma pop_macro("Rect")
# else
	 , Rect(_base())
# endif
#endif
#if defined GL_CIRCULAR_CCW_ARC_TO_NV
# if defined CircularCCWArcTo
#  pragma push_macro("CircularCCWArcTo")
#  undef CircularCCWArcTo
	 , CircularCCWArcTo(_base())
#  pragma pop_macro("CircularCCWArcTo")
# else
	 , CircularCCWArcTo(_base())
# endif
#endif
#if defined GL_CIRCULAR_CW_ARC_TO_NV
# if defined CircularCWArcTo
#  pragma push_macro("CircularCWArcTo")
#  undef CircularCWArcTo
	 , CircularCWArcTo(_base())
#  pragma pop_macro("CircularCWArcTo")
# else
	 , CircularCWArcTo(_base())
# endif
#endif
#if defined GL_CIRCULAR_TANGENT_ARC_TO_NV
# if defined CircularTangentArcTo
#  pragma push_macro("CircularTangentArcTo")
#  undef CircularTangentArcTo
	 , CircularTangentArcTo(_base())
#  pragma pop_macro("CircularTangentArcTo")
# else
	 , CircularTangentArcTo(_base())
# endif
#endif
#if defined GL_ARC_TO_NV
# if defined ArcTo
#  pragma push_macro("ArcTo")
#  undef ArcTo
	 , ArcTo(_base())
#  pragma pop_macro("ArcTo")
# else
	 , ArcTo(_base())
# endif
#endif
#if defined GL_RELATIVE_ARC_TO_NV
# if defined RelativeArcTo
#  pragma push_macro("RelativeArcTo")
#  undef RelativeArcTo
	 , RelativeArcTo(_base())
#  pragma pop_macro("RelativeArcTo")
# else
	 , RelativeArcTo(_base())
# endif
#endif
	{ }
};

} // namespace enums


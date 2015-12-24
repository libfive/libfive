//  File implement/oglplus/enums/polygon_mode_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/polygon_mode.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PolygonMode> class Transform>
class EnumToClass<Base, PolygonMode, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_POINT
# if defined Point
#  pragma push_macro("Point")
#  undef Point
	Transform<PolygonMode::Point> Point;
#  pragma pop_macro("Point")
# else
	Transform<PolygonMode::Point> Point;
# endif
#endif
#if defined GL_LINE
# if defined Line
#  pragma push_macro("Line")
#  undef Line
	Transform<PolygonMode::Line> Line;
#  pragma pop_macro("Line")
# else
	Transform<PolygonMode::Line> Line;
# endif
#endif
#if defined GL_FILL
# if defined Fill
#  pragma push_macro("Fill")
#  undef Fill
	Transform<PolygonMode::Fill> Fill;
#  pragma pop_macro("Fill")
# else
	Transform<PolygonMode::Fill> Fill;
# endif
#endif
#if defined GL_FILL_RECTANGLE_NV
# if defined FillRectangle
#  pragma push_macro("FillRectangle")
#  undef FillRectangle
	Transform<PolygonMode::FillRectangle> FillRectangle;
#  pragma pop_macro("FillRectangle")
# else
	Transform<PolygonMode::FillRectangle> FillRectangle;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_POINT
# if defined Point
#  pragma push_macro("Point")
#  undef Point
	 , Point(_base())
#  pragma pop_macro("Point")
# else
	 , Point(_base())
# endif
#endif
#if defined GL_LINE
# if defined Line
#  pragma push_macro("Line")
#  undef Line
	 , Line(_base())
#  pragma pop_macro("Line")
# else
	 , Line(_base())
# endif
#endif
#if defined GL_FILL
# if defined Fill
#  pragma push_macro("Fill")
#  undef Fill
	 , Fill(_base())
#  pragma pop_macro("Fill")
# else
	 , Fill(_base())
# endif
#endif
#if defined GL_FILL_RECTANGLE_NV
# if defined FillRectangle
#  pragma push_macro("FillRectangle")
#  undef FillRectangle
	 , FillRectangle(_base())
#  pragma pop_macro("FillRectangle")
# else
	 , FillRectangle(_base())
# endif
#endif
	{ }
};

} // namespace enums


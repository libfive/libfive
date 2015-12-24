//  File implement/oglplus/enums/ext/compat_prim_type_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/ext/compat_prim_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<CompatibilityPrimitiveType> class Transform>
class EnumToClass<Base, CompatibilityPrimitiveType, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_POINTS
# if defined Points
#  pragma push_macro("Points")
#  undef Points
	Transform<CompatibilityPrimitiveType::Points> Points;
#  pragma pop_macro("Points")
# else
	Transform<CompatibilityPrimitiveType::Points> Points;
# endif
#endif
#if defined GL_LINE_STRIP
# if defined LineStrip
#  pragma push_macro("LineStrip")
#  undef LineStrip
	Transform<CompatibilityPrimitiveType::LineStrip> LineStrip;
#  pragma pop_macro("LineStrip")
# else
	Transform<CompatibilityPrimitiveType::LineStrip> LineStrip;
# endif
#endif
#if defined GL_LINE_LOOP
# if defined LineLoop
#  pragma push_macro("LineLoop")
#  undef LineLoop
	Transform<CompatibilityPrimitiveType::LineLoop> LineLoop;
#  pragma pop_macro("LineLoop")
# else
	Transform<CompatibilityPrimitiveType::LineLoop> LineLoop;
# endif
#endif
#if defined GL_LINES
# if defined Lines
#  pragma push_macro("Lines")
#  undef Lines
	Transform<CompatibilityPrimitiveType::Lines> Lines;
#  pragma pop_macro("Lines")
# else
	Transform<CompatibilityPrimitiveType::Lines> Lines;
# endif
#endif
#if defined GL_TRIANGLE_STRIP
# if defined TriangleStrip
#  pragma push_macro("TriangleStrip")
#  undef TriangleStrip
	Transform<CompatibilityPrimitiveType::TriangleStrip> TriangleStrip;
#  pragma pop_macro("TriangleStrip")
# else
	Transform<CompatibilityPrimitiveType::TriangleStrip> TriangleStrip;
# endif
#endif
#if defined GL_TRIANGLE_FAN
# if defined TriangleFan
#  pragma push_macro("TriangleFan")
#  undef TriangleFan
	Transform<CompatibilityPrimitiveType::TriangleFan> TriangleFan;
#  pragma pop_macro("TriangleFan")
# else
	Transform<CompatibilityPrimitiveType::TriangleFan> TriangleFan;
# endif
#endif
#if defined GL_TRIANGLES
# if defined Triangles
#  pragma push_macro("Triangles")
#  undef Triangles
	Transform<CompatibilityPrimitiveType::Triangles> Triangles;
#  pragma pop_macro("Triangles")
# else
	Transform<CompatibilityPrimitiveType::Triangles> Triangles;
# endif
#endif
#if defined GL_QUADS
# if defined Quads
#  pragma push_macro("Quads")
#  undef Quads
	Transform<CompatibilityPrimitiveType::Quads> Quads;
#  pragma pop_macro("Quads")
# else
	Transform<CompatibilityPrimitiveType::Quads> Quads;
# endif
#endif
#if defined GL_QUAD_STRIP
# if defined QuadStrip
#  pragma push_macro("QuadStrip")
#  undef QuadStrip
	Transform<CompatibilityPrimitiveType::QuadStrip> QuadStrip;
#  pragma pop_macro("QuadStrip")
# else
	Transform<CompatibilityPrimitiveType::QuadStrip> QuadStrip;
# endif
#endif
#if defined GL_POLYGON
# if defined Polygon
#  pragma push_macro("Polygon")
#  undef Polygon
	Transform<CompatibilityPrimitiveType::Polygon> Polygon;
#  pragma pop_macro("Polygon")
# else
	Transform<CompatibilityPrimitiveType::Polygon> Polygon;
# endif
#endif

	EnumToClass(void) { }
	EnumToClass(Base&& base)
	 : Base(std::move(base))
#if defined GL_POINTS
# if defined Points
#  pragma push_macro("Points")
#  undef Points
	 , Points(_base())
#  pragma pop_macro("Points")
# else
	 , Points(_base())
# endif
#endif
#if defined GL_LINE_STRIP
# if defined LineStrip
#  pragma push_macro("LineStrip")
#  undef LineStrip
	 , LineStrip(_base())
#  pragma pop_macro("LineStrip")
# else
	 , LineStrip(_base())
# endif
#endif
#if defined GL_LINE_LOOP
# if defined LineLoop
#  pragma push_macro("LineLoop")
#  undef LineLoop
	 , LineLoop(_base())
#  pragma pop_macro("LineLoop")
# else
	 , LineLoop(_base())
# endif
#endif
#if defined GL_LINES
# if defined Lines
#  pragma push_macro("Lines")
#  undef Lines
	 , Lines(_base())
#  pragma pop_macro("Lines")
# else
	 , Lines(_base())
# endif
#endif
#if defined GL_TRIANGLE_STRIP
# if defined TriangleStrip
#  pragma push_macro("TriangleStrip")
#  undef TriangleStrip
	 , TriangleStrip(_base())
#  pragma pop_macro("TriangleStrip")
# else
	 , TriangleStrip(_base())
# endif
#endif
#if defined GL_TRIANGLE_FAN
# if defined TriangleFan
#  pragma push_macro("TriangleFan")
#  undef TriangleFan
	 , TriangleFan(_base())
#  pragma pop_macro("TriangleFan")
# else
	 , TriangleFan(_base())
# endif
#endif
#if defined GL_TRIANGLES
# if defined Triangles
#  pragma push_macro("Triangles")
#  undef Triangles
	 , Triangles(_base())
#  pragma pop_macro("Triangles")
# else
	 , Triangles(_base())
# endif
#endif
#if defined GL_QUADS
# if defined Quads
#  pragma push_macro("Quads")
#  undef Quads
	 , Quads(_base())
#  pragma pop_macro("Quads")
# else
	 , Quads(_base())
# endif
#endif
#if defined GL_QUAD_STRIP
# if defined QuadStrip
#  pragma push_macro("QuadStrip")
#  undef QuadStrip
	 , QuadStrip(_base())
#  pragma pop_macro("QuadStrip")
# else
	 , QuadStrip(_base())
# endif
#endif
#if defined GL_POLYGON
# if defined Polygon
#  pragma push_macro("Polygon")
#  undef Polygon
	 , Polygon(_base())
#  pragma pop_macro("Polygon")
# else
	 , Polygon(_base())
# endif
#endif
	{ }
};

} // namespace enums


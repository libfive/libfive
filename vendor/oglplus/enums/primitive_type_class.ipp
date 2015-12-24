//  File implement/oglplus/enums/primitive_type_class.ipp
//
//  Automatically generated file, DO NOT modify manually.
//  Edit the source 'source/enums/oglplus/primitive_type.txt'
//  or the 'source/enums/make_enum.py' script instead.
//
//  Copyright 2010-2015 Matus Chochlik.
//  Distributed under the Boost Software License, Version 1.0.
//  See accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt
//
namespace enums {
template <typename Base, template<PrimitiveType> class Transform>
class EnumToClass<Base, PrimitiveType, Transform>
 : public Base
{
private:
	Base& _base(void) { return *this; }
public:

#if defined GL_POINTS
# if defined Points
#  pragma push_macro("Points")
#  undef Points
	Transform<PrimitiveType::Points> Points;
#  pragma pop_macro("Points")
# else
	Transform<PrimitiveType::Points> Points;
# endif
#endif
#if defined GL_LINE_STRIP
# if defined LineStrip
#  pragma push_macro("LineStrip")
#  undef LineStrip
	Transform<PrimitiveType::LineStrip> LineStrip;
#  pragma pop_macro("LineStrip")
# else
	Transform<PrimitiveType::LineStrip> LineStrip;
# endif
#endif
#if defined GL_LINE_LOOP
# if defined LineLoop
#  pragma push_macro("LineLoop")
#  undef LineLoop
	Transform<PrimitiveType::LineLoop> LineLoop;
#  pragma pop_macro("LineLoop")
# else
	Transform<PrimitiveType::LineLoop> LineLoop;
# endif
#endif
#if defined GL_LINES
# if defined Lines
#  pragma push_macro("Lines")
#  undef Lines
	Transform<PrimitiveType::Lines> Lines;
#  pragma pop_macro("Lines")
# else
	Transform<PrimitiveType::Lines> Lines;
# endif
#endif
#if defined GL_TRIANGLE_STRIP
# if defined TriangleStrip
#  pragma push_macro("TriangleStrip")
#  undef TriangleStrip
	Transform<PrimitiveType::TriangleStrip> TriangleStrip;
#  pragma pop_macro("TriangleStrip")
# else
	Transform<PrimitiveType::TriangleStrip> TriangleStrip;
# endif
#endif
#if defined GL_TRIANGLE_FAN
# if defined TriangleFan
#  pragma push_macro("TriangleFan")
#  undef TriangleFan
	Transform<PrimitiveType::TriangleFan> TriangleFan;
#  pragma pop_macro("TriangleFan")
# else
	Transform<PrimitiveType::TriangleFan> TriangleFan;
# endif
#endif
#if defined GL_TRIANGLES
# if defined Triangles
#  pragma push_macro("Triangles")
#  undef Triangles
	Transform<PrimitiveType::Triangles> Triangles;
#  pragma pop_macro("Triangles")
# else
	Transform<PrimitiveType::Triangles> Triangles;
# endif
#endif
#if defined GL_LINES_ADJACENCY
# if defined LinesAdjacency
#  pragma push_macro("LinesAdjacency")
#  undef LinesAdjacency
	Transform<PrimitiveType::LinesAdjacency> LinesAdjacency;
#  pragma pop_macro("LinesAdjacency")
# else
	Transform<PrimitiveType::LinesAdjacency> LinesAdjacency;
# endif
#endif
#if defined GL_LINE_STRIP_ADJACENCY
# if defined LineStripAdjacency
#  pragma push_macro("LineStripAdjacency")
#  undef LineStripAdjacency
	Transform<PrimitiveType::LineStripAdjacency> LineStripAdjacency;
#  pragma pop_macro("LineStripAdjacency")
# else
	Transform<PrimitiveType::LineStripAdjacency> LineStripAdjacency;
# endif
#endif
#if defined GL_TRIANGLES_ADJACENCY
# if defined TrianglesAdjacency
#  pragma push_macro("TrianglesAdjacency")
#  undef TrianglesAdjacency
	Transform<PrimitiveType::TrianglesAdjacency> TrianglesAdjacency;
#  pragma pop_macro("TrianglesAdjacency")
# else
	Transform<PrimitiveType::TrianglesAdjacency> TrianglesAdjacency;
# endif
#endif
#if defined GL_TRIANGLE_STRIP_ADJACENCY
# if defined TriangleStripAdjacency
#  pragma push_macro("TriangleStripAdjacency")
#  undef TriangleStripAdjacency
	Transform<PrimitiveType::TriangleStripAdjacency> TriangleStripAdjacency;
#  pragma pop_macro("TriangleStripAdjacency")
# else
	Transform<PrimitiveType::TriangleStripAdjacency> TriangleStripAdjacency;
# endif
#endif
#if defined GL_PATCHES
# if defined Patches
#  pragma push_macro("Patches")
#  undef Patches
	Transform<PrimitiveType::Patches> Patches;
#  pragma pop_macro("Patches")
# else
	Transform<PrimitiveType::Patches> Patches;
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
#if defined GL_LINES_ADJACENCY
# if defined LinesAdjacency
#  pragma push_macro("LinesAdjacency")
#  undef LinesAdjacency
	 , LinesAdjacency(_base())
#  pragma pop_macro("LinesAdjacency")
# else
	 , LinesAdjacency(_base())
# endif
#endif
#if defined GL_LINE_STRIP_ADJACENCY
# if defined LineStripAdjacency
#  pragma push_macro("LineStripAdjacency")
#  undef LineStripAdjacency
	 , LineStripAdjacency(_base())
#  pragma pop_macro("LineStripAdjacency")
# else
	 , LineStripAdjacency(_base())
# endif
#endif
#if defined GL_TRIANGLES_ADJACENCY
# if defined TrianglesAdjacency
#  pragma push_macro("TrianglesAdjacency")
#  undef TrianglesAdjacency
	 , TrianglesAdjacency(_base())
#  pragma pop_macro("TrianglesAdjacency")
# else
	 , TrianglesAdjacency(_base())
# endif
#endif
#if defined GL_TRIANGLE_STRIP_ADJACENCY
# if defined TriangleStripAdjacency
#  pragma push_macro("TriangleStripAdjacency")
#  undef TriangleStripAdjacency
	 , TriangleStripAdjacency(_base())
#  pragma pop_macro("TriangleStripAdjacency")
# else
	 , TriangleStripAdjacency(_base())
# endif
#endif
#if defined GL_PATCHES
# if defined Patches
#  pragma push_macro("Patches")
#  undef Patches
	 , Patches(_base())
#  pragma pop_macro("Patches")
# else
	 , Patches(_base())
# endif
#endif
	{ }
};

} // namespace enums

